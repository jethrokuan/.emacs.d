(require 'package)

(if (memq window-system '(mac ns))
    (progn
      (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
      (add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t))
  (setq package-archives nil))

(package-initialize)

(org-babel-load-file (expand-file-name "~/.emacs.d/config.org"))
(put 'dired-find-alternate-file 'disabled nil)
