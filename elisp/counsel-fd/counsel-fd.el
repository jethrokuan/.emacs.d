(require 'counsel)

(defvar counsel-fd-command "fd --hidden --color never "
  "Base command for fd.")


(defun counsel-fd-dired-jump (&optional initial-input initial-directory)
  "Jump to a directory (in dired) below the current directory.
List all subdirectories within the current directory.
INITIAL-INPUT can be given as the initial minibuffer input.
INITIAL-DIRECTORY, if non-nil, is used as the root directory for search."
  (interactive
   (list nil
         (when current-prefix-arg
           (read-directory-name "From directory: "))))
  (counsel-require-program "fd")
  (let* ((default-directory (or initial-directory default-directory)))
    (ivy-read "Directory: "
              (split-string
               (shell-command-to-string
                (concat counsel-fd-command "--type d --exclude '*.git'"))
               "\n" t)
              :initial-input initial-input
              :action (lambda (d) (dired-jump nil (expand-file-name d)))
              :caller 'counsel-fd-dired-jump)))


(defun counsel-fd-file-jump (&optional initial-input initial-directory)
  "Jump to a file below the current directory.
List all files within the current directory or any of its subdirectories.
INITIAL-INPUT can be given as the initial minibuffer input.
INITIAL-DIRECTORY, if non-nil, is used as the root directory for search."
  (interactive
   (list nil
         (when current-prefix-arg
           (read-directory-name "From directory: "))))
  (counsel-require-program "fd")
  (let* ((default-directory (or initial-directory default-directory)))
    (ivy-read "Directory: "
              (split-string
               (shell-command-to-string
                (concat counsel-fd-command "--type f --exclude '*.git'"))
               "\n" t)
              :initial-input initial-input
              :action (lambda (d) (find-file (expand-file-name d)))
              :caller 'counsel-fd-file-jump)))

(provide 'counsel-fd)
