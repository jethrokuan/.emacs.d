(require 'package)

(if (memq window-system '(mac ns))
    (progn
      (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
      (add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t))
  (setq package-archives nil))

(package-initialize)

(let ((file-name-handler-alist nil))
  ;; If config is pre-compiled, then load that
  (if (file-exists-p (expand-file-name "config.elc" user-emacs-directory))
      (load-file (expand-file-name "config.elc" user-emacs-directory))
    ;; Otherwise use org-babel to tangle and load the configuration
    (require 'org)
    (org-babel-load-file (expand-file-name "config.org" user-emacs-directory))))
