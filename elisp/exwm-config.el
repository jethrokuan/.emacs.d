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

(require 'exwm-randr)
(exwm-randr-enable)

(defun jethro/launch (command)
  (interactive (list (read-shell-command "$ ")))
  (start-process-shell-command command nil command))

(defun jethro/exwm-terminal ()
  (interactive)
  (call-process "urxvtc"))

(exwm-input-set-key (kbd "s-SPC") #'jethro/launch)
(exwm-input-set-key (kbd "C-c C-p") #'ivy-pass)
(exwm-input-set-key (kbd "C-x t") #'jethro/exwm-terminal)

(add-hook 'exwm-manage-finish-hook
          (lambda ()
            (when (and exwm-class-name
                       (string= exwm-class-name "URxvt"))
              (exwm-input-set-local-simulation-keys '(([?\C-c ?\C-c] . ?\C-c))))))

(use-package pulseaudio-control
  :config
  (exwm-input-set-key (kbd "<XF86AudioRaiseVolume>") #'pulseaudio-control-increase-volume)
  (exwm-input-set-key (kbd "<XF86AudioLowerVolume>") #'pulseaudio-control-decrease-volume)
  (exwm-input-set-key (kbd "<XF86AudioMute>") #'pulseaudio-control-toggle-current-sink-mute))

(exwm-enable)

(provide 'exwm-config)
