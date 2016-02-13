
;;;; Use narrow-to-page to manipulate document
(put 'narrow-to-page 'disabled nil) ;; Bound to C-x n p, Widen with C-x n w
(setq-default indent-tabs-mode nil)

;;;;Add MELPA
(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
  (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
  (package-initialize))

(setq message-log-max 16384)
(set-face-attribute 'default nil :height 140)
(setq-default tab-width 2)


;;;; `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-and-compile
  (defvar use-package-verbose t)
  ;; (defvar use-package-expand-minimally t)
  (eval-after-load 'advice
    `(setq ad-redefinition-action 'accept))
  (require 'cl)
  (require 'use-package))

(require 'bind-key)
(require 'diminish nil t)
(setq use-package-always-ensure t)

;;;; Setting Emacs registers
(bind-key* "C-o" 'jump-to-register)
(set-register ?b (cons 'file "~/.org/books.org"))
(set-register ?i (cons 'file "~/.emacs.d/init.el"))
(set-register ?s (cons 'file "~/.org/someday.org"))
(set-register ?t (cons 'file "~/.org/today.org"))



;;;; Changes for sanity
;; Change backup directory to prevent littering of working dir
(setq backup-directory-alist `(("." . "~/.saves")))

;;Keybindings
(bind-key* "C-x m" 'eshell)

(require 'compile)
(add-hook 'c++-mode-hook
	  (lambda ()
	    (unless (file-exists-p "Makefile")
	      (set (make-local-variable 'compile-command)
		   (let ((file (file-name-nondirectory buffer-file-name)))
		     (format "g++ -Wall -s -pedantic-errors %s -o %s --std=c++11"
			     file
			     (file-name-sans-extension file)))))))

;; Shortcut for compile
(global-set-key (kbd "<f5>") (lambda ()
                               (interactive)
                               (setq-local compilation-read-command nil)
                               (call-interactively 'compile)))

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

;;Firestarter
(use-package firestarter
  :bind ("C-c m s" . firestarter-mode)
  :init (put 'firestarter 'safe-local-variable 'identity))

;;User-details
(setq user-full-name "Jethro Kuan"
      user-mail-address "jethrokuan95@gmail.com")

;; Email
(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu4e")
(require 'mu4e)
(require 'org-mu4e)
(bind-key* "C-c e" #'mu4e)

(setq mu4e-drafts-folder "/[Gmail].Drafts")
(setq mu4e-sent-folder   "/[Gmail].Sent Mail")
(setq mu4e-trash-folder  "/[Gmail].Trash")

;; don't save message to Sent Messages, Gmail/IMAP takes care of this
(setq mu4e-sent-messages-behavior 'delete)

(setq
 mu4e-view-show-images t
 mu4e-view-image-max-width 800)

(setq mu4e-update-interval 300)

;; (See the documentation for `mu4e-sent-messages-behavior' if you have
;; additional non-Gmail addresses and want assign them different
;; behavior.)

;; setup some handy shortcuts
;; you can quickly switch to your Inbox -- press ``ji''
;; then, when you want archive some messages, move them to
;; the 'All Mail' folder by pressing ``ma''.

(setq mu4e-maildir-shortcuts
      '( ("/INBOX"               . ?i)
	 ("/[Gmail].Sent Mail"   . ?s)
	 ("/[Gmail].Trash"       . ?t)
	 ("/[Gmail].All Mail"    . ?a)))

;; allow for updating mail using 'U' in the main view:
(setq mu4e-get-mail-command "offlineimap")
(setq mu4e-attachment-dir  "~/Downloads")

;;store link to message if in header view, not to header query
(setq org-mu4e-link-query-in-headers-mode nil)

(setq mu4e-view-show-images t)
;; use imagemagick, if available
(when (fboundp 'imagemagick-register-types)
  (imagemagick-register-types))

;; something about ourselves
(setq
 mu4e-compose-signature
 (concat
  "Warmest Regards,\n"
  "Jethro Kuan\n"))

;; sending mail -- replace USERNAME with your gmail username
;; also, make sure the gnutls command line utils are installed
;; package 'gnutls-bin' in Debian/Ubuntu

(require 'smtpmail)
(setq message-send-mail-function 'smtpmail-send-it
      starttls-use-gnutls t
      smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
      smtpmail-auth-credentials
      '(("smtp.gmail.com" 587 "jethrokuan95@gmail.com" nil))
      smtpmail-default-smtp-server "smtp.gmail.com"
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587)

;; alternatively, for emacs-24 you can use:
;;(setq message-send-mail-function 'smtpmail-send-it
;;     smtpmail-stream-type 'starttls
;;     smtpmail-default-smtp-server "smtp.gmail.com"
;;     smtpmail-smtp-server "smtp.gmail.com"
;;     smtpmail-smtp-service 587)

;; don't keep message buffers around
(setq message-kill-buffer-on-exit t)

;; Emacs profiling tool
(use-package esup
  :defer t)


;;;; Theming
;;   Color Schemes 
(use-package base16-theme
  :init (load-theme 'base16-chalk-dark t))

(use-package solarized-theme
  :disabled t
  :init (progn
          (setq solarized-distinct-fringe-background t)
          (setq solarized-high-contrast-mode-line t)
          (load-theme 'solarized-dark t)
          ))
(when (window-system)
  (set-default-font "Fira Code"))
(let ((alist '((33 . ".\\(?:\\(?:==\\)\\|[!=]\\)")
               (35 . ".\\(?:[(?[_{]\\)")
               (38 . ".\\(?:\\(?:&&\\)\\|&\\)")
               (42 . ".\\(?:\\(?:\\*\\*\\)\\|[*/]\\)")
               (43 . ".\\(?:\\(?:\\+\\+\\)\\|\\+\\)")
               (45 . ".\\(?:\\(?:-[>-]\\|<<\\|>>\\)\\|[<>}~-]\\)")
               (46 . ".\\(?:\\(?:\\.[.<]\\)\\|[.=]\\)")
               (47 . ".\\(?:\\(?:\\*\\*\\|//\\|==\\)\\|[*/=>]\\)")
               (58 . ".\\(?:[:=]\\)")
               (59 . ".\\(?:;\\)")
               (60 . ".\\(?:\\(?:!--\\)\\|\\(?:\\$>\\|\\*>\\|\\+>\\|--\\|<[<=-]\\|=[<=>]\\||>\\)\\|[/<=>|-]\\)")
               (61 . ".\\(?:\\(?:/=\\|:=\\|<<\\|=[=>]\\|>>\\)\\|[<=>~]\\)")
               (62 . ".\\(?:\\(?:=>\\|>[=>-]\\)\\|[=>-]\\)")
               (63 . ".\\(?:[:=?]\\)")
               (92 . ".\\(?:\\(?:\\\\\\\\\\)\\|\\\\\\)")
               (94 . ".\\(?:=\\)")
               (123 . ".\\(?:-\\)")
               (124 . ".\\(?:\\(?:|[=|]\\)\\|[=>|]\\)")
               (126 . ".\\(?:[=@~-]\\)")
               )
             ))
  (dolist (char-regexp alist)
    (set-char-table-range composition-function-table (car char-regexp)
                          `([,(cdr char-regexp) 0 font-shape-gstring]))))

;;   Show parens
(show-paren-mode 1)
(setq show-paren-delay 0)


;;;;   Useful functions
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


;;;; Modes for general writing
;; Writegood mode
(use-package writegood-mode
  :bind ("C-c m g" . writegood-mode))

(use-package draft-mode
  :bind ("C-c m d" . draft-mode))

(use-package focus
  :bind ("C-c m f" . focus-mode))


;;;; Minor Modes
;;; Visual Upgrades
;;   Shows x/y for isearch
(use-package anzu
  :functions anzu-mode
  :defer 5
  :diminish anzu-mode
  :config (global-anzu-mode +1))

;;   Highlights copy/paste changes
(use-package volatile-highlights
  :functions volatile-highlights-mode
  :defer 5
  :diminish volatile-highlights-mode
  :config (volatile-highlights-mode t))

;;   Aggressive-indent mode
(use-package aggressive-indent
  :diminish aggressive-indent-mode
  :init (add-hook 'prog-mode-hook 'aggressive-indent-mode))

;;   Which-key
(use-package which-key
  :diminish which-key-mode
  :init (add-hook 'after-init-hook 'which-key-mode))

;;   Rainbow-delimiters for pretty brackets
(use-package rainbow-delimiters
  :init (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

;;   Rainbow-mode for displaying colors for RGB and hex values
(use-package rainbow-mode
  :init (add-hook 'css-mode-hook 'rainbow-mode))

;;   Display line nums in prog-mode
(defun linum-mode-hook () 
  (linum-mode 1))

(add-hook 'prog-mode-hook 'linum-mode-hook)


;;;; Movement
(use-package avy
  :bind (("C-'" . avy-goto-char)
	 ("C-," . avy-goto-char-2)))

(use-package ace-window
  :bind (("M-'" . ace-window)))

;;   Expand Region
(use-package expand-region
  :bind (("C-=" . er/expand-region)))

(use-package change-inner
  :bind (("M-i" . change-inner)
	 ("M-o" . change-outer)))


;;;; Helm
(use-package helm
  :diminish helm-mode
  :bind* (("C-c h" . helm-mini)
	  ("C-x C-f" . helm-find-files)
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
            (setq helm-idle-delay 0.0
	          helm-input-idle-delay 0.01 
	          helm-quick-update t
		  helm-M-x-requires-pattern nil
		  helm-ff-skip-boring-files t)
	    (helm-mode 1)))

;; Search
(use-package helm-ag
  :bind ("C-c g" . helm-ag))

;; Descbinds
(use-package helm-descbinds
  :bind ("C-c d" . helm-descbinds))

;; Rhythmbox (linux only)
(use-package helm-rhythmbox
  :bind ("C-S-o" . helm-rhythmbox))


;;   Paredit
(use-package paredit
  :commands paredit-mode
  :diminish paredit-mode
  :init (progn
	  (add-hook 'emacs-lisp-mode-hook #'paredit-mode)))


;;   Magit
(use-package magit
  :init (add-hook 'magit-mode-hook 'hl-line-mode)
  :bind (("C-x g" . magit-status)
	 ("C-x M-g" . magit-dispatch-popup)))


;;;; Code/Text Completion
;;   Yasnippet - snippets
(use-package yasnippet
  :defer 5
  :commands (yas-global-mode yas-minor-mode)
  :init (add-hook 'after-init-hook 'yas-global-mode)
  :config (progn
	    (setq yas-snippet-dirs '("~/.emacs.d/snippets/"))))

;;   Company - code completions
(use-package company
  :diminish company-mode
  :defer 5
  :init (progn
	  (add-hook 'after-init-hook 'global-company-mode)
	  (setq company-idle-delay 0.1)
	  (setq company-transformers '(company-sort-by-occurrence)))
  :config (progn
	    (use-package company-irony
	      :disabled t
	      :init (eval-after-load 'company '(add-to-list 'company-backends 'company-irony)))
	    (use-package company-quickhelp
	      :init (add-hook 'global-company-mode 'company-quickhelp-mode))))


;;;; Project Management
;;   Projectile
(use-package projectile
  :defer 5
  :commands projectile-global-mode
  :bind-keymap ("C-c p" . projectile-command-map)
  :config (use-package helm-projectile
	    :config (progn
		      (setq projectile-completion-system 'helm)
		      (helm-projectile-on)))
  (projectile-global-mode))


;;;; Org Mode
(use-package org-plus-contrib
  :bind* (("C-c c" . org-capture)
	  ("C-c a" . org-agenda)
	  ("C-c l" . org-store-link))
  :mode ("\\.org\\'" . org-mode)
  :init (progn
	  (use-package org-trello
	    :init (progn
		    (custom-set-variables '(org-trello-files '("/home/jethro/.org/Trello/fridge.org")))
		    (setq org-trello-consumer-key "f8bcf0f535a7cd6be5c2533bc1c9c809"
			  org-trello-access-token "548bee5e0e1a40385e087ea544ebdd19bfe6ea6034d812ca99e0948149c4353c")))
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
		  ("e" "Email" entry (file+headline "~/.org/today.org" "Emails")
		   "* TODO [#A] %?\nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+0d\"))\n%a\n")
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

;;;; Language Specific Modes

;;;; Markdown mode
(use-package markdown-mode
  :mode "\\.md\\'")


;;;; Clojure
;;   Clojure-mode
(use-package clojure-mode
  :mode (("\\.clj\\'" . clojure-mode)
         ("\\.boot\\'" . clojure-mode)
         ("\\.edn\\'" . clojure-mode)
         ("\\.cljs\\'" . clojure-mode)
         ("\\.cljs\\.hl\\'" . clojure-mode))
  :init (progn
          (require 'clojure-mode-extra-font-locking)
          (add-hook 'clojure-mode-hook #'eldoc-mode)
          (add-hook 'clojure-mode-hook #'subword-mode)
          (add-hook 'clojure-mode-hook #'paredit-mode)
          (add-hook 'clojure-mode-hook #'clj-refactor-mode)
          (add-hook 'clojure-mode-hook #'inf-clojure-minor-mode)	  
          (add-hook 'clojure-mode-hook
                    '(lambda ()
                       (define-key clojure-mode-map "\C-c\C-k" 'reload-current-clj-ns)
                       (define-key clojure-mode-map "\C-cl" 'erase-inf-buffer)
                       (define-key clojure-mode-map "\C-c\C-t" 'clojure-toggle-keyword-string))))
  :config (progn
            (use-package clojure-mode-extra-font-locking)
            (use-package align-cljlet
              :bind ("C-c C-a" . align-cljlet))))

;; Inf-clojure
(use-package inf-clojure
  :functions (run-clojure clojure-find-ns inf-clojure-eval-string inf-clojure-switch-to-repl)
  :commands inf-clojure-switch-to-repl
  :init (progn
	  (setq inf-clojure-program "boot -C repl -c")
	  (defun reload-current-clj-ns (next-p)
	    (interactive "P")
	    (let ((ns (clojure-find-ns)))
	      (message (format "Loading %s ..." ns))
	      (inf-clojure-eval-string (format "(require '%s :reload)" ns))
	      (when (not next-p) (inf-clojure-eval-string (format "(in-ns '%s)" ns)))))

	  (defun run-boot-repl (x)
	    (interactive "sEnter Port Number:")
	    (run-clojure (format  "boot repl -cp %s" x)))
	  
	  (defun erase-inf-buffer ()
	    (interactive)
	    (with-current-buffer (get-buffer "*inf-clojure*")
	      (erase-buffer))
	    (inf-clojure-eval-string ""))
	  (setq inf-clojure-prompt-read-only nil)
	  (add-hook 'inf-clojure-mode-hook #'eldoc-mode)
	  (add-hook 'inf-clojure-mode-hook
		    '(lambda ()
		       (define-key inf-clojure-mode-map "\C-cl" 'erase-inf-buffer)))))

(use-package clj-refactor
  :defer t
  :diminish clj-refactor-mode
  :config (cljr-add-keybindings-with-prefix "C-c j"))


;;;; JSON
(use-package json-mode
  :mode "\\.json\\'")


;;;; Web
(use-package web-mode
	:init (progn
					(setq web-mode-code-indent-offset 2)
					(setq web-mode-markup-indent-offset 2))
  :mode (("\\.html\\'" . web-mode)
				 ("\\.erb\\'" . web-mode)
				 ("\\.jsx\\'" . web-mode)
				 ("\\.mustache'" . web-mode))
  :config (progn
						(setq web-mode-content-types-alist
									'(("jsx"  . "/home/jethro/Code/tooople/frontend/.*\\.js[x]?\\'")))))

(use-package js2-mode
  :diminish js2-mode
	:init (setq js2-basic-offset 2)
  :mode (("\\.js\\'" . js2-mode))
  :config (require 'js2-imenu-extras))

(use-package scss-mode
  :mode (("\\.scss\\'" . scss-mode)
	 ("\\.sass\\'" . sass-mode))
  :init (add-hook 'rainbow-mode-hook 'scss-mode))

(use-package emmet-mode
  :defer t
  :init (progn (add-hook 'sgml-mode-hook 'emmet-mode)
	       (add-hook 'css-mode-hook 'emmet-mode)))

(use-package irony
  :disabled t
  :mode (("\\.cpp\\'" . irony-mode)
	 ("\\.c\\'" . irony-mode))
  :diminish irony-mode
  :init (progn (defun my-irony-mode-hook ()
		 (define-key irony-mode-map [remap completion-at-point]
		   'irony-completion-at-point-async)
		 (define-key irony-mode-map [remap complete-symbol]
		   'irony-completion-at-point-async))
	       (add-hook 'c++-mode-hook 'irony-mode)
	       (add-hook 'c-mode-hook 'irony-mode)
	       (add-hook 'objc-mode-hook 'irony-mode)
	       (add-hook 'irony-mode-hook 'my-irony-mode-hook)
	       (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-optionss)))
(provide 'init.el)
;;init.el ends here
