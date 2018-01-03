(require 'package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
(package-initialize)

;; (require 'org)
;; (require 'ob-tangle)
(org-babel-load-file (expand-file-name "~/.emacs.d/config.org"))
(put 'dired-find-alternate-file 'disabled nil)
