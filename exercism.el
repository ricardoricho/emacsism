;;; exercism --- Wrapper for exercism cli
;;; Commentary:
;;; Define elisp functions with `shell-command' to use exercism cli.
;;; https://github.com/exercism/cli
;;; Code:

(require 'subr-x)

(defgroup exercism nil
  "Group for exercism."
  :group 'tools)

(defcustom exercism-keymap-prefix nil
  "Prefix keymap to access exercism command map."
  :group 'exercism
  :type 'string)

;;;###autoload
(defvar exercism-command-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "d") #'exercism-download-and-open)
    (define-key map (kbd "f") #'exercism-find-exercise)
    (define-key map (kbd "s") #'exercism-submit)
    (define-key map (kbd "t") #'exercism-test)
    map)
  "Exercism prefix keymap.")
;;;###autoload
(fset 'exercism-command-map exercism-command-map)

(defvar exercism--current-workspace nil
  "Store the current workspace of exercism.
The output of 'exercism workspace'")

(defun exercism-workspace ()
  "Get and store the exercism workspace."
  (interactive)
  (let ((current-workspace (shell-command-to-string "exercism workspace")))
    (setq exercism--current-workspace (string-trim current-workspace))))

(defun exercism--tracks ()
  "Get a list of tracks."
  (directory-files (exercism-workspace) nil "^[a-z]"))

(defun exercism--exercises (track)
  "Given a TRACK get the list of downloaded exercises."
  (let ((exercise-dir (expand-file-name track (exercism-workspace))))
    (directory-files exercise-dir nil "^[a-z]")))

(defun exercism-find-exercise (&optional track)
  "Open `find-file' in `TRACK' directory."
  (interactive (list (completing-read "Track:" (exercism--tracks))))
  (let ((workspace (exercism-workspace)))
    (find-file (expand-file-name track workspace))))

(defun exercism-download (track exercise)
  "Download the `EXERCISE' from the corresponding `TRACK'."
  (interactive (list (completing-read "Track:" (exercism--tracks))
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

(defun exercism-submit (&optional file)
  "Submit the solution `FILE' if any, other submit the current buffer `FILE'."
  (interactive)
  (let* ((file-to-submit (or file (buffer-file-name)))
         (submit-command (concat "exercism submit " file-to-submit)))
    (message submit-command)
    (shell-command submit-command)))

(provide 'exercism)
;;; exercism.el ends here
