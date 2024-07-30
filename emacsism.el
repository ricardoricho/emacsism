;;; emacsism --- Wrapper for exercism cli
;;; Commentary:
;;; Define elisp functions with `shell-command' to use exercism cli.
;;; https://github.com/exercism/cli
;;; Code:

(require 'ansi-color)
(require 'json)
(require 'subr-x)

(defgroup emacsism nil
  "Group for emacsism."
  :group 'tools)

(defcustom emacsism-keymap-prefix nil
  "Prefix keymap to access emacsism command map."
  :group 'emacsism
  :type 'string)

(defcustom emacsism-container-command nil
  "Container commad, for example: docker, podman, etc."
  :group 'emacsism
  :type 'string)

;;;###autoload;
(defvar emacsism-command-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "d") #'emacsism-url-download-and-open)
    (define-key map (kbd "f") #'emacsism-find-exercise)
    (define-key map (kbd "s") #'emacsism-submit)
    (define-key map (kbd "t") #'emacsism-run-tests)
    (define-key map (kbd "v") #'emacsism-visit-exercise)
    map)
  "Emacsism prefix keymap.")

(defvar emacsism-mode-map
  (let ((map (make-sparse-keymap)))
    (when emacsism-keymap-prefix
      (define-key map emacsism-keymap-prefix emacsism-command-map)))
  "Emacsism keymap, a simple map for emacsism-command-map.")

;;;###autoload
(define-minor-mode emacsism-mode
  "A wrapper for CLI emacsism."
  :lighter " Emacsism"
  :keymap emacsism-mode-map
  :global t
  (cond
   (emacsism-mode (emacsism--check-install))
   (t (message "Emacsism down."))))

(defun emacsism-workspace ()
  "Get and store the Emacsism workspace."
  (interactive)
  (let ((current-workspace (shell-command-to-string "exercism workspace")))
    (string-trim current-workspace)))

(defun emacsism--library-dir ()
  "Return directory where this source file is located."
  (let* ((emacsism-buffer (find-library "emacsism"))
         (emacsism-file (buffer-file-name emacsism-buffer))
         (emacsism-truefile (file-truename emacsism-file)))
    (file-name-directory emacsism-truefile)))

(defun emacsism--check-install ()
  "Check if emacsism is installed."
  (cond
   ((string-empty-p (shell-command-to-string "command -v exercism"))
    (progn (error "Undefined `exercism' command")
           (emacsism-mode -1)))
   (t (message "Emacsism good... "))))

(defun emacsism--tracks ()
  "Get a list of tracks."
  (directory-files (emacsism-workspace) nil "^[a-z]"))

(defun emacsism--exercises (track)
  "Given a TRACK get the list of downloaded exercises."
  (let ((exercise-dir (expand-file-name track (emacsism-workspace))))
    (directory-files exercise-dir nil "^[a-z]")))

(defun emacsism-find-exercise (track &optional exercise)
  "Open `find-file' in readme file of the EXERCISE in TRACK directory."
  (interactive (list (completing-read "Track: " (emacsism--tracks))))
  (let ((workspace (emacsism-workspace))
        (exercise (or exercise (completing-read "Exercise:" (emacsism--exercises track)))))
    (find-file
     (expand-file-name "README.md" (mapconcat 'file-name-as-directory
                                              (list workspace track exercise) nil)))))

(defun emacsism-url-download (url)
  "Download track exercise from URL."
  (interactive (list (read-string "URL: ")))
  (let* ((url-regexp (emacsism--exercism-url-regexp))
         (match (string-match url-regexp url)))
    (if match
        (let ((track (match-string 1 url))
              (exercise (match-string 2 url)))
          (emacsism-download track exercise))
      (error "Not a exercism url"))))

(defun emacsism-url-download-and-open (url)
  "Download track exercise from URL."
  (interactive (list (read-string "URL: ")))
  (if (string-match (emacsism--exercism-url-regexp) url)
      (let ((track (match-string 1 url))
            (exercise (match-string 2 url)))
        (emacsism-download-and-open track exercise))
    (error "Not an exercism url")))

(defun emacsism--build-exercism-url (track exercise)
  "Build the exercism url for TRACK and EXERCISE."
  (format "https://exercism.org/tracks/%s/exercises/%s" track exercise))

(defun emacsism--exercism-url-regexp ()
  "Return a regexp that match exersicm urls."
  (emacsism--build-exercism-url "\\(.+\\)" "\\(.+\\)"))

(defun emacsism-download (track exercise)
  "Download the `EXERCISE' from the corresponding `TRACK'."
  (interactive (list (completing-read "Track: " (emacsism--tracks))
                     (read-string "Exercise: ")))
  (let* ((exercise-string (if (symbolp exercise) (symbol-name exercise)
                            exercise))
         (track-string (if (symbolp track) (symbol-name track) track))
         (download-track (concat "--track=" track-string))
         (download-exercise (concat "--exercise=" exercise-string))
         (download-command
          (concat "exercism download " download-track " " download-exercise)))
    (message download-command)
    (shell-command download-command)))

(defun emacsism-download-and-open (track exercise)
  "Download and open an EXERCISE on the given TRACK."
  (interactive (list (completing-read "Track: " (emacsism--tracks))
                     (read-string "Exercise: ")))
  (progn (emacsism-download track exercise)
         (emacsism-find-exercise track exercise)))

(defun emacsism-submit (&optional file)
  "Submit the solution `FILE' if any, other submit the current buffer `FILE'."
  (interactive)
  (let* ((file-to-submit (or file (buffer-file-name)))
         (submit-command (concat "exercism submit " file-to-submit)))
    (message submit-command)
    (shell-command submit-command)))

(defun emacsism-visit-exercise ()
  "Build the url to visit the exercise in the browser."
  (interactive)
  (let* ((current-file (expand-file-name (buffer-file-name)))
         (track (emacsism--track-name current-file))
         (exercise (emacsism--exercise-slug track current-file))
         (url (emacsism--build-exercism-url track exercise)))
    (browse-url url)
    (message "Visit %s" url)))

(defun emacsism--exercise-slug (track file)
  "Get TRACK exercise name from FILE path."
  (let* ((directory (expand-file-name file))
         (track-match (string-match (format "/%s/" track) directory))
         (exercise-path (substring directory (+ 2 track-match (length track))))
         (rest-match (string-match "/" exercise-path)))
    (substring exercise-path 0 rest-match)))

(defun emacsism--track-name (file)
  "Get track from FILE."
  (let* ((workspace (emacsism-workspace))
         (track-rest (substring file (+ 1 (length workspace))))
         (exercise-rest (string-match "/" track-rest)))
    (substring track-rest 0 exercise-rest)))

(defun emacsism-run-tests ()
  "Run the current exercise test.
According to current buffer file, found the track and exercise.
Then for the track found run `emacissm--run-track-tests' with exercise."
  (interactive)
  (let* ((current-file (expand-file-name (buffer-file-name)))
         (current-track (emacsism--track-name current-file))
         (current-exercise
          (emacsism--exercise-slug current-track current-file)))
    (funcall 'emacsism-test-runner current-track current-exercise)))

(defun emacsism-test-runner (track exercise)
  "Call TRACK test runner for EXERCISE."
  (if emacsism-container-command
      (emacsism-container-test-runner track exercise)
    (emacsism-local-test-runner track exercise)))

(defun emacsism-local-test-runner (track exercise)
  "Run EXERCISE test file with local tools for TRACK."
  (let ((test-command (intern (format "emacsism--run-%s-tests" track))))
    (funcall test-command exercise)))

(defun emacsism-container--test-command (track exercise &optional path)
  "Build the command to test EXERCISE on TRACK mounting PATH."
  (let* ((exercise-path (or path default-directory))
         (runner (format "exercism/%s-test-runner" track))
         (volume (format "%s:/solution" exercise-path)))
    (format "%s run --rm -v %s %s %s /solution /solution"
            emacsism-container-command volume runner exercise)))

(defun emacsism-container-test-runner (track exercise)
  "Call TRACK test runner container for EXERCISE."
  (let* ((default-directory (emacsism--exercise-path track exercise))
         (test-command (emacsism-container--test-command track exercise))
         (test-buffer-name (format "*emacsism-%s-%s*" track exercise))
         (results-file (expand-file-name "results.json")))
    (with-current-buffer (get-buffer-create test-buffer-name)
      (switch-to-buffer-other-window (current-buffer))
      (setq buffer-read-only nil)
      (erase-buffer)
      (insert (format "Exercism command: %s\n" test-command))
      (shell-command test-command)
      (insert (format "Exercism results: %s\n  Found in: %s\n"
                      (file-exists-p results-file) results-file))
      (when (file-exists-p results-file)
        (let ((results (json-parse-string
                        (with-temp-buffer
                          (insert-file-contents results-file)
                          (buffer-string)))))
          (delete-file results-file)
          (insert "Results...\n"
                  (format "Hash keys: %s\n\n" (hash-table-keys results))
                  (format " Status: %s\n" (gethash "status" results))
                  (format "version: %s\n" (gethash "version" results))
                  (format "message: %s\n" (gethash "message" results))
                  "Tests:\n")
          (emacsism--results-insert-tests (gethash "tests" results))))
      (compilation-mode))))

(defun emacsism--results-insert-tests (results)
  "Insert the RESULTS in buffer."
  (mapc 'emacsism--results-insert-test results))

(defun emacsism--results-insert-test (test)
  "Insert the TEST in buffer."
  (let ((keys (hash-table-keys test))
        (name (gethash "name" test))
        (status (gethash "status" test))
        (test-code (gethash "test_code" test))
        (test-message (gethash "message" test)))
    (insert
     (format "       Name: %s\n" name)
     (format "  test-code: %s\n" test-code)
     (format "     status: %s\n" status)
     (if (and test-message (not (eq test-message :null)))
         (format "    message: %s\n\n"
                 (string-replace "\n" "\n  " (ansi-color-apply test-message)))
       "\n"))))

(defun emacsism--run-command (command track exercise)
  "Call COMMAND and show results in a TRACK EXERCISE buffer.
Execute COMMAND (general for testing) with the EXERCISE path as
`default-directory'.  The new buffer named *emacsism-TRACK-EXERCISE*
run with `compilation-mode' for results."
  (let* ((test-buffer-name (format "*emacsism-%s-%s*" track exercise))
         (test-buffer (get-buffer-create test-buffer-name))
         (default-directory (emacsism--exercise-path track exercise)))
    (with-current-buffer test-buffer
      (setq buffer-read-only nil)
      (erase-buffer)
      (insert "Emacsism: %s" command)
      (async-shell-command command test-buffer))))

(defun emacsism--track-path (track)
  "Return the path of the TRACK.
That is the workspace-directory with the appended track name."
  (expand-file-name track (emacsism-workspace)))

(defun emacsism--exercise-path (track exercise)
  "Return the EXERCISE path found in TRACK."
  (let ((exercise-slug (emacsism--exercise-slug track exercise)))
    (expand-file-name exercise-slug (emacsism--track-path track))))

;; Runners

(defun emacsism--run-clojure-tests (exercise)
  "Run test file for clojure EXERCISE."
  (emacsism--run-command "lein test" "clojure" exercise))

(defun emacsism--run-common-lisp-tests (exercise)
  "Run test suite for common-lisp EXERCISE."
  (let* ((test-file (format "%s-test.lisp" exercise))
         (quit-clause (format "'(uiop:quit (if (%s-test:run-tests) 0 1))'" exercise))
         (test-command (format "ros run --load %s --eval %s"
                              test-file quit-clause)))
    (emacsism--run-command test-command "common-lisp" exercise)))

(defun emacsism--run-elixir-tests (exercise)
  "Run test file for elixir EXERCISE."
  (emacsism--run-command "mix test" "elixir" exercise))

(defun emacsism--run-emacs-lisp-tests (exercise)
  "Run test file for emacs-lisp EXERCISE.
Run the test as batch and show results in new buffer."
  (let* ((string-command (concat "emacs -batch -l ert -l %s-test.el -f "
                                "ert-run-tests-batch-and-exit"))
         (test-command (format string-command exercise)))
    (emacsism--run-command test-command "emacs-lisp" exercise)))

(defun emacsism--run-haskell-tests (exercise)
  "Run test file for haskell EXERCISE."
  (emacsism--run-command "stack test" "haskell" exercise))

(defun emacsism--run-java-tests (exercise)
  "Run test file for java EXERCISE."
  (emacsism--run-command "gradle test" "java" exercise))

(defun emacsism--run-javascript-tests (exercise)
  "Run test file for javascript EXERCISE."
  (emacsism--run-command "yarn install && yarn test" "javascript" exercise))

(defun emacsism--run-jq-tests (exercise)
  "Run test file for jq EXERCISE."
  (emacsism--run-command (format "bats test-%s.bats" exercise) "jq" exercise))

(defun emacsism--run-mips-tests (exercise)
  "Run test file for mips EXERCISE."
  (emacsism--run-command "java -jar /mars.jar nc runner.mips impl.mips" "mips" exercise))

(defun emacsism--run-prolog-tests (exercise)
  "Run test file for prolog EXERCISE."
  (let* ((exercise-slug (replace-regexp-in-string "-" "_" exercise))
         (string-command (concat "swipl -f %s.pl -s %s_tests.plt "
                                 "-g run_tests,halt -t 'halt(1)'"))
         (test-command (format string-command exercise-slug exercise-slug)))
    (emacsism--run-command test-command "prolog" exercise)))

(defun emacsism--run-python-tests (exercise)
  "Run test file for python EXERCISE."
  (emacsism--run-command "pytest" "python" exercise))

(defun emacsism--run-racket-tests (exercise)
  "Run test file for racket EXERCISE."
  (emacsism--run-command
   (format "raco test %s-test.rkt" exercise) "racket" exercise))

(defun emacsism--run-r-tests (exercise)
  "Run test file for R EXERCISE."
  (emacsism--run-command (format "Rscript test_%s.R" exercise) "r" exercise))

(defun emacsism--run-ruby-tests (exercise)
  "Run test file for ruby EXERCISE."
  (let ((exercise-slug (replace-regexp-in-string "-" "_" exercise)))
    (emacsism--run-command (format "ruby %s_test.rb" exercise-slug) "ruby" exercise-slug)))

(defun emacsism--run-rust-tests (exercise)
  "Run test file for rust EXERCISE."
  (emacsism--run-command "cargo test" "rust" exercise))

(defun emacsism--tracks-list ()
  "Hardcoded list of tracks."
  '("Bash" "C" "C#" "C++" "CFML" "Clojure" "ClojureScript" "CoffeeScript"
    "Common" "Lisp" "Crystal" "D" "Dart" "Delphi" "Pascal" "Elixir" "Elm"
    "Emacs Lisp" "Erlang" "F#" "Fortran" "Go" "Groovy" "Haskell" "J" "Java"
    "JavaScript" "Julia" "Kotlin" "LFE" "Lua" "MIPS Assembly" "Nim" "Objective-C"
    "OCaml" "Perl 5" "Pharo" "PHP" "PL/SQL" "Prolog" "PureScript" "Python" "R"
    "Racket" "Raku" "ReasonML" "Red" "Ruby" "Rust" "Scala" "Scheme" "Standard ML"
    "Swift" "Tcl" "TypeScript" "VB.NET" "Vim script" "Wren" "x86-64 Assembly"))

(provide 'emacsism)
;;; emacsism.el ends here
