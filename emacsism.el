;;; emacsism --- Wrapper for exercism cli
;;; Commentary:
;;; Define elisp functions with `shell-command' to use exercism cli.
;;; https://github.com/exercism/cli
;;; Code:

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
    (define-key map (kbd "d") #'emacsism-download-and-open)
    (define-key map (kbd "f") #'emacsism-find-exercise)
    (define-key map (kbd "s") #'emacsism-submit)
    (define-key map (kbd "t") #'emacsism-test)
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

(defun emacsism--check-install ()
  "Check if emacsism is installed."
  (cond
   ((null emacsism-keymap-prefix)
    (progn (message "Undefined `emacsism-keymap-prefix'")
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
  (interactive (list (completing-read "Track:" (emacsism--tracks))))
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
  (let* ((url-regexp (emacsism--exercism-url-regexp))
         (match (string-match url-regexp url)))
    (if match
        (let ((track (match-string 1 url))
              (exercise (match-string 2 url)))
          (emacsism-download-and-open track exercise))
      (error "Not a exercism url."))))

(defun emacsism--exercism-url-regexp ()
  "Return a regexp that match exersicm urls."
  "https://exercism.org/tracks/\\(.+\\)/exercises/\\(.+\\)")

(defun emacsism-download (track exercise)
  "Download the `EXERCISE' from the corresponding `TRACK'."
  (interactive (list (completing-read "Track:" (emacsism--tracks))
                     (read-string "Exercise:")))
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
  (interactive (list (completing-read "Track:" (emacsism--tracks))
                     (read-string "Exercise:")))
  (progn (emacsism-download track exercise)
         (emacsism-find-exercise track exercise)))

(defun emacsism-submit (&optional file)
  "Submit the solution `FILE' if any, other submit the current buffer `FILE'."
  (interactive)
  (let* ((file-to-submit (or file (buffer-file-name)))
         (submit-command (concat "exercism submit " file-to-submit)))
    (message submit-command)
    (shell-command submit-command)))

(defun emacsism--container-attributes (track exercise)
  "Command to run TRACK container with attributes for EXERCISE."
  (when emacsism-container-command
    (concat emacsism-container-command " run "
            (emacsism--container-run-options track exercise) " "
            (emacsism--container-name track) " ")))

(defun emacsism--container-run-options (track exercise)
  "Format options to run TRACK EXERCISE."
  (format "-v %s:/%s" (emacsism--exercise-path track exercise) track))

(defun emacsism--container-name (track)
  "Emacsism container name for TRACK."
  (concat "emacsism-" track))

(defun emacsism--exercise-name (track file)
  "Get TRACK exercise name from FILE path."
  (let* ((directory (expand-file-name file))
         (track-match (string-match track directory))
         (exercise-path (substring directory (+ 1 track-match (length track))))
         (rest-match (string-match "/" exercise-path)))
    (substring exercise-path 0 rest-match)))

(defun emacsism-test (&optional exercise)
  "Run the current EXERCISE test.
Depending on the `major-mode' where the buffer is execute the corresponding
emacsism test command, usually named `emacsism--run-TRACK-test' with EXERCISE.
With the last sentences it make sense to better find the track named based on
the `default-directory'."
  (interactive)
  (let ((exercise-name (or exercise (file-name-base (buffer-file-name)))))
    (cond
     ((eq 'elixir-mode major-mode) (emacsism--run-elixir-tests exercise-name))
     ((eq 'emacs-lisp-mode major-mode) (emacsism--run-emacs-lisp-tests exercise-name))
     ((eq 'java-mode major-mode) (emacsism--run-java-tests exercise-name))
     ((eq 'prolog-mode major-mode) (emacsism--run-prolog-tests exercise-name))
     ((eq 'python-mode major-mode) (emacsism--run-python-tests exercise-name))
     ((eq 'ruby-mode major-mode) (emacsism--run-ruby-tests exercise-name))
     ((eq 'rust-mode major-mode) (emacsism--run-rust-tests exercise-name))
     (t (error "Not command defined to run %s tests" major-mode)))))

(defun emacsism--run-elixir-tests (exercise)
  "Run test file for elixir EXERCISE."
  (emacsism--run-command "elixir" exercise "mix test"))

(defun emacsism--run-emacs-lisp-tests (exercise)
  "Run test file for emacs-lisp EXERCISE.
Run the test as batch and show results in new buffer."
  (emacsism--run-command
   "emacs-lisp" exercise
   (format "emacs -batch -l ert -l %s-test.el -f ert-run-tests-batch-and-exit" exercise)))

(defun emacsism--run-prolog-tests (exercise)
  "Run test file for prolog EXERCISE."
  (emacsism--run-command
   "prolog" exercise
   (format "swipl -f %s.pl -s %s_tests.plt -g run_tests,halt -t 'halt(1)'" exercise exercise)))

(defun emacsism--run-python-tests (file)
  "Run test FILE for python exercise."
  (let ((exercise (emacsism--exercise-name "python" file)))
    (emacsism--run-command "python" exercise "pytest")))

(defun emacsism--run-ruby-tests (exercise)
  "Run test file for ruby EXERCISE."
  (emacsism--run-command
   "ruby" exercise
   (format "ruby %s_test.rb" exercise)))

(defun emacsism--run-rust-tests (file)
  "Run test file for rust FILE."
  (let ((exercise (emacsism--exercise-name "rust" file)))
    (emacsism--run-command "rust" exercise "cargo test")))

(defun emacsism--run-java-tests (file)
  "Run test file for java FILE."
  (let ((exercise (emacsism--exercise-name "java" file)))
    (emacsism--run-command "java" exercise "gradle test")))

(defun emacsism--run-command (track exercise command)
  "Call COMMAND and show results in a TRACK EXERCISE buffer.
Execute COMMAND (general for testing) with the EXERCISE path as
`default-directory'.  The new buffer named *emacsism-TRACK-EXERCISE*
run with `compilation-mode' for results."
  (let* ((test-buffer
          (get-buffer-create (concat "*emacsism-" track "-" exercise "*")))
         (default-directory (emacsism--exercise-path track exercise))
         (container-prefix (emacsism--container-attributes track exercise))
         (execute-command (concat container-prefix
                                  (if container-prefix
                                      (format "\"%s\"" command)
                                    command))))
    (switch-to-buffer-other-window test-buffer)
    (message "Emacsism: %s" execute-command)
    (with-current-buffer test-buffer
      (setq buffer-read-only nil)
      (erase-buffer)
      (shell-command execute-command test-buffer)
      (compilation-mode))))

(defun emacsism--track-path (track)
  "Return the path of the TRACK.
That is the workspace-directory with the appended track name."
  (expand-file-name track (emacsism-workspace)))

(defun emacsism--exercise-path (track exercise)
  "Return the EXERCISE path found in TRACK."
  (let ((exercise-dir (emacsism--format-dir track exercise)))
    (expand-file-name exercise-dir (emacsism--track-path track))))

(defun emacsism--format-dir (track exercise)
  "Return the correspoding format for an EXERCISE directory of a given TRACK."
  (cond
   ((equal track "ruby") (replace-regexp-in-string "_" "-" exercise))
   ((equal track "elixir") (replace-regexp-in-string "_" "-" exercise))
   (t exercise)))

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
