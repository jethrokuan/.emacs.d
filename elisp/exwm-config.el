;;; -*- lexical-binding: t; -*-
(setq exwm-workspace-number 6)
(defun jethro/exwm-rename-buffer-to-title () (exwm-workspace-rename-buffer exwm-title))
(add-hook 'exwm-update-title-hook 'jethro/exwm-rename-buffer-to-title)
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

(push ?\s-  exwm-input-prefix-keys)
(push ?\M-  exwm-input-prefix-keys)

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
(exwm-input-set-key (kbd "s-p") #'ivy-pass)
(exwm-input-set-key (kbd "C-x t") #'jethro/exwm-terminal)
(exwm-input-set-key (kbd "s-f") #'counsel-find-file)
(exwm-input-set-key (kbd "s-F") #'counsel-locate)

(add-hook 'exwm-manage-finish-hook
          (lambda ()
            (when (and exwm-class-name
                       (string= exwm-class-name "URxvt"))
              (exwm-input-set-local-simulation-keys '(([?\C-c ?\C-c] . ?\C-c))))))

(setq exwm-input-simulation-keys
      '(
        ;; movement
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
        ;; copy/paste.
        ([?\M-w] . [?\C-c])
        ([?\C-y] . [?\C-v])
        ;; search
        ([?\C-s] . [?\C-f])))

(use-package pulseaudio-control
  :config
  (exwm-input-set-key (kbd "<XF86AudioRaiseVolume>") #'pulseaudio-control-increase-volume)
  (exwm-input-set-key (kbd "<XF86AudioLowerVolume>") #'pulseaudio-control-decrease-volume)
  (exwm-input-set-key (kbd "<XF86AudioMute>") #'pulseaudio-control-toggle-current-sink-mute))

(when (executable-find "brightnessctl")
  (defun jethro/return-brightness-percentage ()
    (interactive)
    (string-to-number (shell-command-to-string "brightnessctl get")))
  (defun jethro/brightness-up ()
    (interactive)
    (shell-command "brightnessctl set 100+")
    (message "Screen Brightness: %s" (jethro/return-brightness-percentage))
    (kill-buffer "*Shell Command Output*"))
  (defun jethro/brightness-down ()
    (interactive)
    (shell-command "brightnessctl set 100-")
    (message "Screen Brightness: %s" (jethro/return-brightness-percentage))
    (kill-buffer "*Shell Command Output*"))
  (exwm-input-set-key (kbd "<XF86MonBrightnessDown>") #'jethro/brightness-down)
  (exwm-input-set-key (kbd "<XF86MonBrightnessUp>") #'jethro/brightness-up))



(exwm-enable)

(provide 'exwm-config)
