;;; -*- lexical-binding: t; -*-
(setq exwm-workspace-number 6)
(add-hook 'exwm-update-class-hook
          (defun my-exwm-update-class-hook ()
            (unless (or (string-prefix-p "sun-awt-X11-" exwm-instance-name)
                        (string= "gimp" exwm-instance-name)
                        (string= "Firefox" exwm-class-name))
              (exwm-workspace-rename-buffer exwm-class-name))))
(add-hook 'exwm-update-title-hook
          (defun my-exwm-update-title-hook ()
            (cond ((or (not exwm-instance-name)
                       (string-prefix-p "sun-awt-X11-" exwm-instance-name)
                       (string= "gimp" exwm-instance-name)
                       (string= "Firefox" exwm-class-name))
                   (exwm-workspace-rename-buffer exwm-title)))))

(setq exwm-workspace-show-all-buffers t
      exwm-layout-show-all-buffers t)

(require 'exwm-systemtray)
(exwm-systemtray-enable)

(window-divider-mode 1)
(display-battery-mode 1)
(display-time-mode 1)

;; A dirty hack to display the current workspace on the
;; mode-line.  I prefer it to adding a new item to
;; global-mode-string due to a different placement on the
;; mode-line.  Semantically the meaning is close enough to
;; the original meaning of this variable, so I'll leave it
;; this way.
(setq mode-line-frame-identification
      '(:eval (propertize
               (format "X%s "
                       (funcall exwm-workspace-index-map
                                exwm-workspace-current-index))
               'face 'bold)))

(require 'exwm-randr)
(defvar vifon/exwm-workspace->display '(1 3 5))
(defun vifon/exwm-randr-workspace-output-plist-update ()
  (setq exwm-randr-workspace-output-plist
        (mapcan (lambda (n)
                  (list n "DP-1-2"))
                vifon/exwm-workspace->display)))
(defun vifon/exwm-randr-refresh ()
  (vifon/exwm-randr-workspace-output-plist-update)
  (exwm-randr-refresh))
(vifon/exwm-randr-workspace-output-plist-update)
(exwm-randr-enable)

(defvar vifon/exwm-last-workspace-index nil)
(advice-add 'exwm-workspace-switch :before
            (defun vifon/exwm-save-last-workspace (&rest args)
              (setq vifon/exwm-last-workspace-index
                    exwm-workspace-current-index)))
(defun vifon/exwm-workspace-switch-or-last (super new-index &rest r)
  "If switching to the current workspace, switch to the last one instead."
  (apply super
         (if (and vifon/exwm-last-workspace-index
                  (equal exwm-workspace-current-index
                         new-index))
             vifon/exwm-last-workspace-index
           new-index)
         r))
(advice-add 'exwm-workspace-switch :around
            #'vifon/exwm-workspace-switch-or-last)
(defun vifon/exwm-last-workspace ()
  (interactive)
  (exwm-workspace-switch (or vifon/exwm-last-workspace-index
                             exwm-workspace-current-index)))

(defun my-exwm-mediaplayer ()
  (interactive)
  (start-process-shell-command
   "ncmpcpp" nil "urxvtcd -e ncmpcpp-run"))

(defun my-exwm-next-workspace (arg)
  (interactive "p")
  (let* ((next-idx (+ exwm-workspace-current-index
                      arg)))
    (exwm-workspace-switch next-idx)))
(defun my-exwm-prev-workspace (arg)
  (interactive "p")
  (my-exwm-next-workspace (- arg)))


(define-ibuffer-column exwm-class (:name "Class")
  (if (bound-and-true-p exwm-class-name)
      exwm-class-name
    ""))
(define-ibuffer-column exwm-instance (:name "Instance")
  (if (bound-and-true-p exwm-instance-name)
      exwm-instance-name
    ""))
(define-ibuffer-column exwm-urgent (:name "U")
  (if (bound-and-true-p exwm--hints-urgency)
      "U"
    " "))

(defun vifon/exwm-ibuffer (&optional other-window)
  (interactive "P")
  (let ((name (buffer-name)))
    (ibuffer other-window
             "*exwm-ibuffer*"
             '((mode . exwm-mode))
             nil nil nil
             '((mark exwm-urgent
                     " "
                     (name 64 64 :left :elide)
                     " "
                     (exwm-class 10 -1 :left)
                     " "
                     (exwm-instance 10 -1 :left))))
    (ignore-errors (ibuffer-jump-to-buffer name))))

(add-hook 'buffer-list-update-hook
          (defun vifon/exwm-remove-urgency ()
            (when (and (eq major-mode 'exwm-mode)
                       (eq (current-buffer) (window-buffer))
                       exwm--hints-urgency)
              (setf exwm--hints-urgency nil))))


(defun my-exwm-launch (command)
  (lambda ()
    (interactive)
    (start-process-shell-command
     command nil command)))

(defun vifon/exwm-terminal ()
  (interactive)
  (let ((default-directory (if (derived-mode-p 'dired-mode)
                               (dired-current-directory)
                             default-directory)))
    (call-process "urxvtcd")))

(defun vifon/switch-to-last-buffer ()
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

(setq exwm-input-global-keys
      `((,(kbd "s-M")        . my-exwm-mediaplayer)
        (,(kbd "<s-escape>") . my-exwm-mediaplayer)
        (,(kbd "C-c <C-tab>") . vifon/switch-to-last-buffer)
        (,(kbd "<C-s-tab>") . vifon/switch-to-last-buffer)
        (,(kbd "s-d") . exwm-reset)
        (,(kbd "s-c") . exwm-input-release-keyboard)
        (,(kbd "s-w") . exwm-workspace-switch)
        (,(kbd "s-x") . my-exwm-next-workspace)
        (,(kbd "s-z") . my-exwm-prev-workspace)
        (,(kbd "s-q") . vifon/exwm-last-workspace)
        (,(kbd "<s-tab>") . vifon/exwm-ibuffer)
        (,(kbd "s-SPC") . vifon/exwm-terminal)
        (,(kbd "s-f") . exwm-layout-toggle-fullscreen)
        ,@(mapcar (lambda (arg)
                    (let ((key (car arg))
                          (direction (cadr arg)))
                      `(,(kbd (format "s-%s" key)) .
                        ,(intern (concat "windmove-" direction)))))
                  '(("h" "left")
                    ("j" "down")
                    ("k" "up")
                    ("l" "right")))
        (,(kbd "s-u") . (lambda ()
                          (interactive)
                          (split-window-below)
                          (other-window 1)))
        (,(kbd "s-o") . (lambda ()
                          (interactive)
                          (split-window-right)
                          (other-window 1)))
        (,(kbd "S-s-<return>") . exwm-floating-toggle-floating)
        (,(kbd "s-Q") . (lambda () (interactive) (kill-buffer)))
        ;; Bind "s-0" to "s-9" to switch to a workspace by its index.
        ,@(mapcar (lambda (i)
                    `(,(kbd (format "s-%d" i)) .
                      (lambda ()
                        (interactive)
                        (exwm-workspace-switch ,i))))
                  (number-sequence 0 9))
        (,(kbd "s-r") . (lambda (command)
                          (interactive (list (read-shell-command "$ ")))
                          (start-process-shell-command command nil command)))
        (,(kbd "s-e") . counsel-linux-app)
        (,(kbd "s-R") . rename-buffer)
        (,(kbd "s-' s") . ,(my-exwm-launch "signal-desktop"))
        (,(kbd "s-' t") . ,(my-exwm-launch "telegram"))
        (,(kbd "s-' m") . ,(my-exwm-launch "notmuch-sync"))))

(define-key exwm-mode-map (kbd "C-q")
  (lambda ()
    (interactive)
    ;; Swallow the prefix argument to prevent happy accidents.
    (exwm-input-send-next-key 1)))

(setq exwm-input-simulation-keys
      `(;; movement
        ([?\C-b] . [left])
        ([?\M-b] . [C-left])
        ([?\C-f] . [right])
        ([?\M-f] . [C-right])
        ([?\C-p] . [up])
        ([?\C-n] . [down])
        ([?\C-a] . [home])
        ([?\C-e] . [end])
        ([?\M-v] . [prior])
        ([?\C-v] . [next])
        ([?\C-d] . [delete])
        ([?\C-k] . [S-end delete])
        ([?\M-h] . [?\C-a])
        ;; cut/paste.
        ([?\C-w] . [?\C-x])
        ([?\M-w] . [?\C-c])
        ([?\C-y] . [?\C-v])
        ;; search
        ([?\C-s] . [?\C-f])))

(add-hook 'exwm-manage-finish-hook
          (defun my-exwm-urxvt-simulation-keys ()
            (when exwm-class-name
              (cond
               ((string= exwm-class-name "URxvt")
                (exwm-input-set-local-simulation-keys
                 (mapcar (lambda (key)
                           `([,key] . [,key]))
                         '(?\C-d
                           ?\C-a
                           ?\C-e
                           ?\C-w
                           ?\M-w
                           ?\C-f
                           ?\C-b
                           ?\C-n
                           ?\C-p
                           ?\M-b
                           ?\M-f
                           ?\M-h
                           ?\C-y
                           ?\C-s
                           ?\C-k
                           ?\C-u))))
               ((string= exwm-class-name "Firefox")
                (exwm-input-set-local-simulation-keys
                 ;; TODO: Why is
                 ;; exwm-input-simulation-keys needed here
                 ;; but not for urxvt?
                 `(,@exwm-input-simulation-keys
                   ([?\C-w] . [?\C-w]))))))))

(add-hook 'exwm-manage-finish-hook
          (defun exwm--set-cwd ()
            (cd (getenv "HOME"))))

(exwm-enable)

(provide 'exwm-config)
