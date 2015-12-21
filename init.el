;; For debugging purposes
(unless noninteractive
  (message "Loading %s..." load-file-name))

;;Add MELPA
(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
  (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
  (package-initialize))

(setq message-log-max 16384)

(set-face-attribute 'default nil :height 140)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;;Enable use-package
(eval-and-compile
  (defvar use-package-verbose t)
  ;; (defvar use-package-expand-minimally t)
  (eval-after-load 'advice
    `(setq ad-redefinition-action 'accept))
  (require 'cl)
  (require 'use-package))

;;Ensure that all packages are installed
(setq use-package-always-ensure t)

;; Change backup directory to prevent littering of working dir
(setq backup-directory-alist `(("." . "~/.saves")))

;;Improved keybindings
(bind-key* "C-x m" 'eshell)

(require 'bind-key)
(require 'diminish nil t)

;;Disable Toolbars
(when window-system
  (tooltip-mode -1)
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (scroll-bar-mode -1))

;;No startup screen
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)

;;Setting up shell path
(use-package exec-path-from-shell
  :config (exec-path-from-shell-initialize))

;;User-details
(setq user-full-name "Jethro Kuan"
      user-mail-address "jethrokuan95@gmail.com")

;;;; Theming
;;   Color Schemes
(use-package base16-theme
  :init (load-theme 'base16-chalk-dark t))

(use-package monokai-theme
  :disabled t
  :config (load-theme 'monokai t)
  (setq org-fontify-whole-heading-line t))

(show-paren-mode 1)
(setq show-paren-delay 0)

;; Some useful functions
(defun split-window-right-and-move-there-dammit ()
  (interactive)
  (split-window-right)
  (windmove-right))

(defun nuke-all-buffers ()
  "Kill all buffers, leaving *scratch* only."
  (interactive)
  (mapc
   (lambda (buffer)
     (kill-buffer buffer))
   (buffer-list))
  (delete-other-windows))

(defun now-is ()
  "Insert current time."
  (interactive)
  (insert (format-time-string "%l:%M%P(%z) %Y-%m-%d")))

;; Writegood mode
(use-package writegood-mode
  :bind ("C-c m g" . writegood-mode))

(use-package draft-mode)
(use-package focus)

;; Emacs profiling tool
(use-package esup)

;; Shows x/y for isearch
(use-package anzu
  :diminish anzu-mode
  :config (global-anzu-mode +1))

;; Highlights copy/paste changes
(use-package volatile-highlights
  :diminish volatile-highlights-mode
  :config (volatile-highlights-mode t))

;; Display line nums in prog-mode
(defun linum-mode-hook () 
  (linum-mode 1))

(add-hook 'prog-mode-hook 'linum-mode-hook) 

;; Rainbow-delimiters for pretty brackets
(use-package rainbow-delimiters
  :init (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

;; Rainbow-mode for displaying colors for RGB and hex values
(use-package rainbow-mode
  :init (add-hook 'css-mode-hook 'rainbow-mode))

;;Movement
(use-package avy
  :bind (("C-'" . avy-goto-char)
	 ("C-," . avy-goto-char-2)))

(use-package ace-window
  :bind (("M-'" . ace-window)))

;;Helm
(use-package helm
  :diminish helm-mode
  :bind* (("C-c h" . helm-mini)
	  ("C-x C-f" . helm-find-files)
	  ("C-o" . helm-find-files)
	  ("C-c f" . helm-recentf)
	  ("C-h a" . helm-apropos)
	  ("C-x C-b" . helm-buffers-list)
	  ("C-x b" . helm-buffers-list)
	  ("M-y" . helm-show-kill-ring)
	  ("M-a" . helm-M-x)
	  ("C-x c o" . helm-occur)
	  ("C-x c SPC" . helm-all-mark-rings))
  :config (progn
	    (require 'helm-config)
	    (setq helm-candidate-number-limit 100)
            (setq helm-idle-delay 0.0 ; update fast sources immediately (doesn't).
	          helm-input-idle-delay 0.01  ; this actually updates things
					; reeeelatively quickly.
	          helm-quick-update t
		  helm-M-x-requires-pattern nil
		  helm-ff-skip-boring-files t)
	    (helm-mode 1)))

(use-package helm-ag
  :bind ("C-c g" . helm-ag))

(use-package helm-descbinds
  :bind ("C-c d" . helm-descbinds))

;;;; Modules
;;   Expand Region
(use-package expand-region
  :bind (("C-=" . er/expand-region)))

(use-package change-inner
  :bind (("M-i" . change-inner)
	 ("M-o" . change-outer)))

;;   Which-key
(use-package which-key
  :diminish which-key-mode
  :init (add-hook 'after-init-hook 'which-key-mode))

;;   Aggressive-indent mode
(use-package aggressive-indent
  :diminish aggressive-indent-mode
  :init (add-hook 'prog-mode-hook 'aggressive-indent-mode))

;;   Paredit
(use-package paredit
  :commands paredit-mode
  :diminish paredit-mode
  :config (add-hook 'emacs-lisp-mode-hook #'paredit-mode))

;;   Magit
(use-package magit
  :init (add-hook 'magit-mode-hook 'hl-line-mode)
  :bind (("C-x g" . magit-status)
	 ("C-x M-g" . magit-dispatch-popup)))

;;   Yasnippet
(use-package yasnippet
  :commands (yas-global-mode yas-minor-mode)
  :init (add-hook 'after-init-hook 'yas-global-mode)
  :config (progn
	    (setq yas-snippet-dirs '("~/.emacs.d/snippets/"))))

;;   Company
(use-package company
  :diminish company-mode
  :defer 5
  :init (progn
	  (require 'company-etags)
	  (add-hook 'after-init-hook 'global-company-mode)
	  (add-to-list 'company-etags-modes 'clojure-mode)
	  (setq company-idle-delay 0.5)
	  (setq company-transformers '(company-sort-by-occurrence)))
  :config (progn
	    (use-package company-quickhelp
	      :init (add-hook 'global-company-mode 'company-quickhelp-mode))))

;;   Projectile
(use-package projectile
  :init (add-hook 'after-init-hook 'projectile-global-mode))

;;   Flycheck
(use-package flycheck
  :defer 5
  :init (add-hook 'after-init-hook 'global-flycheck-mode)
  :config (use-package flycheck-pos-tip
	    :config (progn
		      (custom-set-variables '(flycheck-display-errors-function #'flycheck-pos-tip-error-messages)))))
;;   Org Mode
(use-package org-plus-contrib
  :bind* (("C-c c" . org-capture)
	  ("C-c a" . org-agenda)
	  ("C-c l" . org-store-link))
  :mode ("\\.org\\'" . org-mode)
  :init (progn
	  (setq org-modules '(org-drill))
	  (setq org-directory "~/.org")
	  (setq org-default-notes-directory (concat org-directory "/notes.org"))
	  (setq org-agenda-files (file-expand-wildcards "~/.org/*.org"))
	  (setq org-agenda-dim-blocked-tasks t) ;;clearer agenda
	  (setq org-refile-targets
		'((nil :maxlevel . 3)
		  (org-agenda-files :maxlevel . 3)))
	  (setq org-use-fast-todo-selection t)
	  (setq org-treat-S-cursor-todo-selection-as-state-change nil)
	  (setq org-capture-templates
		'(("t" "Todo" entry (file+headline "~/.org/someday.org" "Tasks")
		   "* TODO %? %i\n")
		  ("p" "Project" entry (file+headline "~/.org/someday.org" "Projects")
		   "* TODO %? %i\n")
		  ("b" "Book" entry (file "~/.org/books.org")
		   "* TO-READ %(org-set-tags) %? %i\n")
		  ("v" "Vocab" entry (file+headline "~/.org/vocab.org" "Vocabulary")
		   "* %^{The word} :drill:\n %\\1 \n** Answer \n%^{The definition}")
		  ("i" "Idea" entry (file+datetree "~/.org/ideas.org") "* %?\nEntered on %U\n %i\n")))
	  (setq org-publish-project-alist
		'(("org-books"
		   ;; Path to your org files.
		   :base-directory "~/.org/"
		   :exclude ".*"
		   :include ["books.org"]
		   :with-emphasize t
		   :with-todo-keywords t
		   :with-toc nil
		   :with-tags nil
		   
		   ;; Path to  project.
		   :publishing-directory "~/Documents/blog/content/"
		   :publishing-function org-html-publish-to-html
		   :html-extension "md"
		   :headline-levels 0
		   :body-only t ;; Only export section between <body> </body>
		   )))))

;;   Markdown mode
(use-package markdown-mode
  :mode "\\.md\\'")

;;   Clojure
;;   Clojure-mode
(use-package clojure-mode
  :mode (("\\.clj\\'" . clojure-mode)
	 ("\\.boot\\'" . clojure-mode)
	 ("\\.edn\\'" . clojure-mode)
	 ("\\.cljs\\'" . clojure-mode)
	 ("\\.cljs\\.hl\\'" . clojure-mode))
  :init (progn
	  (add-hook 'clojure-mode-hook #'eldoc-mode)
	  (add-hook 'clojure-mode-hook #'paredit-mode)
	  (add-hook 'clojure-mode-hook #'inf-clojure-minor-mode)	  
	  (add-hook 'clojure-mode-hook
		    '(lambda ()
		       (define-key clojure-mode-map "\C-c\C-k" 'reload-current-clj-ns)
		       (define-key clojure-mode-map "\M-." 'find-tag-without-ns)
		       (define-key clojure-mode-map "\C-cl" 'erase-inf-buffer)
		       (define-key clojure-mode-map "\C-c\C-t" 'clojure-toggle-keyword-string))))
  :config (use-package align-cljlet
	    :bind ("C-c C-a" . align-cljlet)))

;; Inf-clojure
(use-package inf-clojure
  :init (progn
	  (setq inf-clojure-program "boot repl")	 
	  (defun reload-current-clj-ns (next-p)
	    (interactive "P")
	    (let ((ns (clojure-find-ns)))
	      (message (format "Loading %s ..." ns))
	      (inf-clojure-eval-string (format "(require '%s :reload)" ns))
	      (when (not next-p) (inf-clojure-eval-string (format "(in-ns '%s)" ns)))))

	  (defun find-tag-without-ns (next-p)
	    (interactive "P")
	    (find-tag (first (last (split-string (symbol-name (symbol-at-point)) "/")))
		      next-p))

	  (defun run-scripted-repl ()
	    (interactive)
	    (run-clojure "lein trampoline run -m clojure.main script/repl.clj"))

	  (defun run-figwheel-repl ()
	    (interactive)
	    (run-clojure "lein figwheel"))

	  (defun run-lein-repl ()
	    (interactive)
	    (run-clojure "lein repl"))

	  (defun run-boot-repl (x)
	    (interactive "sEnter Port Number:")
	    (run-clojure (format  "boot repl -cp %s" x)))
	  
	  (defun erase-inf-buffer ()
	    (interactive)
	    (with-current-buffer (get-buffer "*inf-clojure*")
	      (erase-buffer))
	    (inf-clojure-eval-string ""))
	  (setq inf-clojure-prompt-read-only nil)
	  (add-hook 'inf-clojure-minor-mode-hook
		    (lambda () (setq completion-at-point-functions nil)))
	  (add-hook 'inf/clojure-mode-hook #'eldoc-mode-hook)
	  (add-hook 'inf-clojure-mode-hook
		    '(lambda ()
		       (define-key inf-clojure-mode-map "\C-cl" 'erase-inf-buffer)))))

;;   Cider
(use-package cider
  :disabled t
  :defer t
  :init (progn
	  (add-hook 'clojure-mode-hook #'clj-refactor-mode)
	  (setq nrepl-popup-stacktraces nil)
	  (add-to-list 'same-window-buffer-names "<em>nrepl</em>")))

(use-package clj-refactor
  :disabled t
  :defer t
  :diminish clj-refactor-mode
  :config (cljr-add-keybindings-with-prefix "C-c j"))

(use-package flycheck-clojure
  :disabled t
  :defer 5
  :config (flycheck-clojure-setup))

(use-package json-mode
  :mode "\\.json\\'")

(use-package emmet-mode
  :init (progn (add-hook 'sgml-mode-hook 'emmet-mode)
	       (add-hook 'css-mode-hook 'emmet-mode)))

(provide 'init.el)
;;init.el ends here
