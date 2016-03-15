;;;; Use narrow-to-page to manipulate document
(put 'narrow-to-page 'disabled nil) ;; Bound to C-x n p, Widen with C-x n w

(setq-default indent-tabs-mode nil)
(setq-default truncate-lines t)

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

(when (window-system)
  (set-frame-font "Input Mono")
  (tooltip-mode -1)
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (scroll-bar-mode -1))

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

;;Zap-up-to-char
(autoload 'zap-up-to-char "misc"
  "Kill up to, but not including ARGth occurrence of CHAR.
  
  \(fn arg char)"
  'interactive)
(bind-key* "M-z" 'zap-up-to-char)

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

;;No startup screen
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)

;; Truncate lines
(use-package visual-fill-column)
(defun trunc-lines-hook ()
  (visual-fill-column-mode 1)
  (set-fill-column 80)
  (setq truncate-lines nil))


;;Setting up shell path
(use-package exec-path-from-shell
  :config (exec-path-from-shell-initialize))

;; Fish-mode
(use-package fish-mode
  :mode ("\\.fish\\'" . fish-mode))


;;Firestarter
(use-package firestarter
  :bind ("C-c m s" . firestarter-mode)
  :config (put 'firestarter 'safe-local-variable 'identity))

;;User-details
(setq user-full-name "Jethro Kuan"
      user-mail-address "jethrokuan95@gmail.com")

;; Emacs profiling tool
(use-package esup
  :defer t)


;;;; Theming
;;   Color Schemes
(use-package tao-theme
  :init (load-theme 'tao-yang t))

(use-package gruvbox-theme
  :disabled t
  :init (load-theme 'gruvbox t))

(use-package smart-mode-line
  :config (sml/setup))

(use-package beacon
  :diminish beacon-mode
  :config (progn
            (beacon-mode 1)
            (setq beacon-push-mark 35)
            (setq beacon-color "#FFF")))

(use-package golden-ratio
  :diminish golden-ratio-mode
  :config (progn
            (add-to-list 'golden-ratio-extra-commands 'ace-window)
            (golden-ratio-mode 1)))

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
(use-package flycheck
  :config (progn
            (use-package flycheck-pos-tip
              :config (flycheck-pos-tip-mode))
            (flycheck-define-checker proselint
              "A linter for prose."
              :command ("proselint" source-inplace)
              :error-patterns
              ((warning line-start (file-name) ":" line ":" column ": "
                        (id (one-or-more (not (any " "))))
                        (message) line-end))
              :modes (text-mode markdown-mode gfm-mode))
            (add-to-list 'flycheck-checkers 'proselint)
            (add-hook 'after-init-hook 'global-flycheck-mode)))

(use-package focus
  :diminish focus-mode
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
  :config (add-hook 'prog-mode-hook 'aggressive-indent-mode))

;;   Which-key
(use-package which-key
  :diminish which-key-mode
  :config (add-hook 'after-init-hook 'which-key-mode))

;;   Rainbow-delimiters for pretty brackets
(use-package rainbow-delimiters
  :config (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

;;   Rainbow-mode for displaying colors for RGB and hex values
(use-package rainbow-mode
  :config (add-hook 'css-mode-hook 'rainbow-mode))

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
          ("C-x f" . helm-fzf)
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
            (defvar helm-fzf-source
              (helm-build-async-source "fzf"
                :candidates-process 'helm-fzf--do-candidate-process
                :nohighlight t
                :requires-pattern 2
                :candidate-number-limit 9999))

            (defun helm-fzf--do-candidate-process ()
              (let* ((cmd-args `("fzf" "-x" "-f" ,helm-pattern))
                     (proc (apply #'start-file-process "helm-fzf" nil cmd-args)))
                (prog1 proc
                  (set-process-sentinel
                   proc
                   (lambda (process event)
                     (helm-process-deferred-sentinel-hook
                      process event (helm-default-directory)))))))

            (defun helm-fzf ()
              (interactive)
              (let ((default-directory "~/"))
                (find-file
                 (concat "~/" (helm :sources '(helm-fzf-source)
                                    :buffer "*helm-fzf*")))))
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
  :diminish paredit-mode
  :config  (add-hook 'emacs-lisp-mode-hook #'paredit-mode))


;;   Magit
(use-package magit  
  :bind (("C-x g" . magit-status)
         ("C-x M-g" . magit-dispatch-popup))
  :config (add-hook 'magit-mode-hook 'hl-line-mode))


;;;; Code/Text Completion
;;   Yasnippet - snippets
(use-package yasnippet
  :diminish yas-global-mode
  :defer 5
  :init (add-hook 'after-init-hook 'yas-global-mode)
  :config (setq yas-snippet-dirs '("~/.emacs.d/snippets/")))

;;   Company - code completions
(use-package company
  :diminish company-mode
  :defer 5
  :init (add-hook 'after-init-hook 'global-company-mode)
  :config (progn            
            (setq company-idle-delay .3)
            (setq company-idle-delay 0)
            (setq company-begin-commands '(self-insert-command)) 
            (setq company-transformers '(company-sort-by-occurrence))
            (use-package company-irony
              :disabled t
              :init (eval-after-load 'company '(add-to-list 'company-backends 'company-irony)))
            (use-package company-quickhelp
              :config (company-quickhelp-mode 1))))

;;;; Project Management
;;   Projectile
(use-package projectile
  :defer 5
  :commands projectile-global-mode
  :bind-keymap ("C-c p" . projectile-command-map)
  :config (progn
            (use-package helm-projectile
              :config (progn
                        (setq projectile-completion-system 'helm)
                        (helm-projectile-on)))
            (projectile-global-mode)))


;;;; Org Mode
(use-package org-plus-contrib
  :init  (add-hook 'org-mode-hook #'trunc-lines-hook)
  :bind* (("C-c c" . org-capture)
          ("C-c a" . org-agenda)
          ("C-c l" . org-store-link))
  :mode ("\\.org\\'" . org-mode)
  :init (progn
          (setq org-ellipsis "⤵")
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
                   :with-tags nil))))
  :config (progn
            (use-package ox-reveal
              :config (require 'ox-reveal))
            (use-package org-trello
              :config (progn
                        (custom-set-variables '(org-trello-files '("/home/jethro/.org/Trello/fridge.org")))
                        (setq org-trello-consumer-key "f8bcf0f535a7cd6be5c2533bc1c9c809"
                              org-trello-access-token "548bee5e0e1a40385e087ea544ebdd19bfe6ea6034d812ca99e0948149c4353c")))))


;;;; Markdown mode
(use-package markdown-mode
  :mode ("\\.md\\'" . markdown-mode)
  :config (add-hook 'markdown-mode-hook #'truc-lines-hook))


;;;; Clojure
;;   Clojure-mode
(use-package clojure-mode
  :mode (("\\.clj\\'" . clojure-mode)
         ("\\.boot\\'" . clojure-mode)
         ("\\.edn\\'" . clojure-mode)
         ("\\.cljs\\'" . clojure-mode)
         ("\\.cljs\\.hl\\'" . clojure-mode))
  :config (progn
            (add-hook 'clojure-mode-hook #'eldoc-mode)
            (add-hook 'clojure-mode-hook #'subword-mode)
            (add-hook 'clojure-mode-hook #'paredit-mode)
            (add-hook 'clojure-mode-hook #'clj-refactor-mode)
            (add-hook 'clojure-mode-hook #'inf-clojure-minor-mode)	  
            (add-hook 'clojure-mode-hook
                      '(lambda ()
                         (define-key clojure-mode-map "\C-c\C-k" 'reload-current-clj-ns)
                         (define-key clojure-mode-map "\C-cl" 'erase-inf-buffer)
                         (define-key clojure-mode-map "\C-c\C-t" 'clojure-toggle-keyword-string)))            
            (use-package align-cljlet
              :bind ("C-c C-a" . align-cljlet))))

;; Inf-clojure
(use-package inf-clojure
  :functions (run-clojure clojure-find-ns inf-clojure-eval-string inf-clojure-switch-to-repl)
  :commands inf-clojure-switch-to-repl
  :config (progn
            (setq inf-clojure-program "boot -C repl -c")
            (defun reload-current-clj-ns (next-p)
              (interactive "P")
              (let ((ns (clojure-find-ns)))
                (message (format "Loading %s ..." ns))
                (inf-clojure-eval-string (format "(require '%s :reload)" ns))
                (when (not next-p) (inf-clojure-eval-string (format "(in-ns '%s)" ns)))))

            (defun run-boot-repl (x)
              (interactive "sEnter Port Number:")
              (run-clojure (format  "boot -C repl -cp %s" x)))
            
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
  :defines cljr-add-keybindings-with-prefix
  :defer t
  :diminish clj-refactor-mode
  :config (cljr-add-keybindings-with-prefix "C-c j"))


;;;; JSON
(use-package json-mode
  :mode "\\.json\\'")


;;;; Web
(use-package web-mode 
  :mode (("\\.html\\'" . web-mode)
				 ("\\.erb\\'" . web-mode)
         ("\\.js\\'" . web-mode)
				 ("\\.jsx\\'" . web-mode)
				 ("\\.mustache'" . web-mode))
  :config (progn
            (setq web-mode-code-indent-offset 2)
            (setq web-mode-markup-indent-offset 2)
            (setq web-mode-content-types-alist
                  '(("jsx"  . "/home/jethro/Code/tooople/frontend/.*\\.js[x]?\\'")))))

(use-package scss-mode
  :mode (("\\.scss\\'" . scss-mode)
         ("\\.sass\\'" . scss-mode))
  :config (add-hook 'scss-mode-hook 'rainbow-mode))

(use-package emmet-mode
  :diminish emmet-mode
  :config (progn
            (add-hook 'sgml-mode-hook 'emmet-mode)
            (add-hook 'web-mode-hook 'emmet-mode)
            (add-hook 'js2-mode-hook 'emmet-mode)
            (add-hook 'css-mode-hook 'emmet-mode)))

(use-package irony
  :disabled t
  :mode (("\\.cpp\\'" . irony-mode)
         ("\\.c\\'" . irony-mode))
  :diminish irony-mode
  :config (progn
            (defun my-irony-mode-hook ()
              (define-key irony-mode-map [remap completion-at-point]
                'irony-completion-at-point-async)
              (define-key irony-mode-map [remap complete-symbol]
                'irony-completion-at-point-async))
            (add-hook 'c++-mode-hook 'irony-mode)
            (add-hook 'c-mode-hook 'irony-mode)
            (add-hook 'objc-mode-hook 'irony-mode)
            (add-hook 'irony-mode-hook 'my-irony-mode-hook)
            (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)))


(use-package go-mode
  :mode ("\\.go\\'" . go-mode)
  :config (progn
            (add-hook 'go-mode-hook (lambda ()
                                      (set (make-local-variable 'company-backends) '(company-go))
                                      (company-mode)))
            (add-hook 'go-mode-hook (lambda ()
                                      (add-hook 'before-save-hook 'gofmt-before-save)
                                      (local-set-key (kbd "M-.") 'godef-jump)))
            (add-hook 'go-mode-hook
                      (lambda ()
                        (unless (file-exists-p "Makefile")
                          (set (make-local-variable 'compile-command)
                               (let ((file (file-name-nondirectory buffer-file-name)))
                                 (format "go build %s"
                                         file))))))
            (use-package company-go
              :config (require 'company-go))))

(provide 'init.el)
;;init.el ends here
