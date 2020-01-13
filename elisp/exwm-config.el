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
(exwm-randr-enable)

(defun jethro/launch (command)
  (interactive (list (read-shell-command "$ ")))
  (start-process-shell-command command nil command))

(exwm-input-set-key (kbd "s-SPC") #'jethro/launch)
(exwm-input-set-key (kbd "C-c C-p") #'ivy-pass)
(exwm-input-set-key (kbd "C-x t") #'jethro/exwm-terminal)

(exwm-enable)

(provide 'exwm-config)
