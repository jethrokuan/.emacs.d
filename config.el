(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-and-compile
  (defvar use-package-verbose t) 
  (require 'cl)
  (require 'use-package)
  (require 'bind-key)
  (require 'diminish)
  (setq use-package-always-ensure t))

(load "server")
(unless (server-running-p) (server-start))

(defun reload-init ()
  (interactive)
  (load-file "~/.emacs.d/init.el"))

(global-set-key (kbd "<f11>") 'reload-init)

(use-package validate)

(validate-setq user-full-name "Jethro Kuan"
      user-mail-address "jethrokuan95@gmail.com")

(global-auto-revert-mode 1)

(validate-setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

(add-to-list 'initial-frame-alist
             '(font . "Iosevka-12"))
(add-to-list 'default-frame-alist
             '(font . "Iosevka-12"))

(tooltip-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(validate-setq inhibit-splash-screen t)
(validate-setq inhibit-startup-message t)

(defalias 'yes-or-no-p 'y-or-n-p)

(delete-selection-mode +1)

(validate-setq sentence-end-double-space nil)

(setq-default tab-width 2)
(setq-default js-indent-level 2)
(setq-default indent-tabs-mode nil)

(setq-default truncate-lines t)
(defun trunc-lines-hook ()
  (validate-setq truncate-lines nil))

(validate-setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(validate-setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(load "~/.emacs.d/secrets.el" t)

(use-package exec-path-from-shell 
  :config
  (exec-path-from-shell-initialize))

(setq-default explicit-shell-file-name "/bin/bash")
(setq-default shell-file-name "/bin/bash")

(use-package zenburn-theme
  :init
  (load-theme 'zenburn t))

(defun open-next-line (arg)
  "Move to the next line and then opens a line.
   See also `newline-and-indent'."
  (interactive "p")
  (end-of-line)
  (open-line arg)
  (next-line 1)
  (when 'newline-and-indent
    (indent-according-to-mode)))

(defun open-previous-line (arg)
  "Open a new line before the current one. 
     See also `newline-and-indent'."
  (interactive "p")
  (beginning-of-line)
  (open-line arg)
  (when 'newline-and-indent
    (indent-according-to-mode)))

(bind-key "C-o" 'open-next-line)
(bind-key "M-o" 'open-previous-line)

(defun jethro/nuke-all-buffers ()
  (interactive)
  (mapcar 'kill-buffer (buffer-list))
  (delete-other-windows))

(bind-key* "C-c !" 'jethro/nuke-all-buffers)

(bind-key* "C-x m" 'eshell)

(bind-key* "M-p" 'mark-paragraph)

(bind-key* "<f9>" (lambda ()
                    (interactive)
                    (validate-setq-local compilation-read-command nil)
                    (call-interactively 'compile)))

(use-package hydra)

(use-package notmuch
  :bind (("<f10>" . notmuch))
  :config
  (define-key notmuch-search-mode-map "R"
  (lambda ()
    "mark message as read"
    (interactive)
    (notmuch-search-tag '("-unread")))))

(require 'notmuch-address)
(validate-setq notmuch-address-command "~/.emacs.d/goobook")
(notmuch-address-message-insinuate)

(use-package flx)

(use-package counsel
  :demand t
  :diminish ivy-mode
  :bind*
  (("C-c C-r" . ivy-resume)
   ("M-a" . counsel-M-x)
   ("C-M-i" . counsel-imenu)
   ("C-x C-f" . counsel-find-file)
   ("C-x j" . counsel-dired-jump)
   ("C-x l" . counsel-locate)
   ("C-c g" . counsel-git)
   ("C-c j" . counsel-git-grep)
   ("C-c s" . counsel-projectile-rg)
   ("C-c f" . counsel-recentf)
   ("M-y" . counsel-yank-pop))
  :bind ((:map help-map
               ("f" . counsel-describe-function)
               ("v" . counsel-describe-variable)
               ("l" . counsel-info-lookup-symbol)))
  :config
  (defun ivy-dired ()
    (interactive)
    (if ivy--directory
        (ivy-quit-and-run
         (dired ivy--directory)
         (when (re-search-forward
                (regexp-quote
                 (substring ivy--current 0 -1)) nil t)
           (goto-char (match-beginning 0))))
      (user-error
       "Not completing files currently")))
  (ivy-mode 1)
  (validate-setq counsel-find-file-at-point t)
  (validate-setq ivy-use-virtual-buffers t)
  (validate-setq ivy-display-style 'fancy)
  (validate-setq ivy-initial-inputs-alist nil)
  (validate-setq ivy-re-builders-alist'
        '((ivy-switch-buffer . ivy--regex-plus)
          (swiper . ivy--regex-plus)
          (t . ivy--regex-fuzzy)))
  (define-key ivy-minibuffer-map (kbd "C-:") 'ivy-dired)
  (define-key ivy-minibuffer-map (kbd "C-c o") 'ivy-occur)
  (define-key read-expression-map (kbd "C-r") 'counsel-expression-history)
  (ivy-set-actions
   t
   '(("I" insert "insert")))
  (define-key ivy-minibuffer-map (kbd "<return>") 'ivy-alt-done)
  (define-key ivy-minibuffer-map (kbd "M-<return>") 'ivy-immediate-done))

(use-package crux 
  :bind* (("C-c o" . crux-open-with)
          ("C-c n" . crux-cleanup-buffer-or-region)
          ("C-c D" . crux-delete-file-and-buffer)
          ("C-a" . crux-move-beginning-of-line)
          ("M-o" . crux-smart-open-line)
          ("C-c r" . crux-rename-file-and-buffer)
          ("M-d" . crux-duplicate-current-line-or-region)
          ("M-D" . crux-duplicate-and-comment-current-line-or-region)
          ("s-o" . crux-smart-open-line-above)))

(defun jethro/open-in-external-app ()
  "Open the current file or dired marked files in external app.
The app is chosen from your OS's preference."
  (interactive)
  (let* (
         (-file-list
          (if (string-equal major-mode "dired-mode")
              (dired-get-marked-files)
            (list (buffer-file-name))))
         (-do-it-p (if (<= (length -file-list) 5)
                       t
                     (y-or-n-p "Open more than 5 files? "))))
    (when -do-it-p
      (cond
       ((string-equal system-type "windows-nt")
        (mapc
         (lambda (-fpath)
           (w32-shell-execute "open" (replace-regexp-in-string "/" "\\" -fpath t t))) -file-list))
       ((string-equal system-type "darwin")
        (mapc
         (lambda (-fpath)
           (shell-command
            (concat "open " (shell-quote-argument -fpath))))  -file-list))
       ((string-equal system-type "gnu/linux")
        (mapc
         (lambda (-fpath) (let ((process-connection-type nil))
                            (start-process "" nil "xdg-open" -fpath))) -file-list))))))
(bind-key* "<f8>" 'jethro/open-in-external-app)

(use-package anzu
  :diminish anzu-mode 
  :config
  (global-anzu-mode +1)
  (define-key isearch-mode-map [remap isearch-query-replace]  #'anzu-isearch-query-replace)
  (define-key isearch-mode-map [remap isearch-query-replace-regexp] #'anzu-isearch-query-replace-regexp))

(use-package avy
  :bind* (("C-'" . avy-goto-char)
          ("C-," . avy-goto-char-2))
  :config
  (validate-setq avy-keys '(?h ?t ?n ?s)))

(use-package windmove 
  :config
  ;; use command key on Mac
  (windmove-default-keybindings 'super)
  ;; wrap around at edges
  (validate-setq windmove-wrap-around t))

(require 'dired)

(let ((gls "/usr/local/bin/gls"))
  (if (file-exists-p gls) (validate-setq insert-directory-program gls)))

(validate-setq delete-by-moving-to-trash t)

(require 'find-dired)
(validate-setq find-ls-option '("-print0 | xargs -0 ls -ld" . "-ld"))

(validate-setq dired-listing-switches "-aBhl  --group-directories-first")

(validate-setq dired-recursive-copies (quote always))
(validate-setq dired-recursive-deletes (quote top))

(require 'dired-x)

(use-package wdired
  :config
  (validate-setq wdired-allow-to-change-permissions t))

(use-package dired-k
  :config
  (define-key dired-mode-map (kbd "K") 'dired-k)
  (validate-setq dired-k-style 'git))

(use-package dired-narrow
  :bind (:map dired-mode-map
              ("N" . dired-narrow-fuzzy)))

(use-package dired-ranger
  :bind (:map dired-mode-map
              ("C" . dired-ranger-copy)
              ("P" . dired-ranger-paste)
              ("M" . dired-ranger-move)))

(use-package visual-regexp
  :bind* (("C-M-%" . vr/query-replace)
          ("C-c m" . vr/mc-mark)))

(use-package electric-align
  :ensure f
  :load-path "elisp/"
  :diminish electric-align-mode
  :config (add-hook 'prog-mode-hook 'electric-align-mode))

(use-package aggressive-indent
  :diminish aggressive-indent-mode
  :config (add-hook 'prog-mode-hook 'aggressive-indent-mode))

(use-package multiple-cursors
  :bind (("C-M-c" . mc/edit-lines)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)))

(use-package expand-region
  :bind (("C-=" . er/expand-region)))

(use-package iedit)

(use-package smartparens
  :bind
  (:map smartparens-mode-map
        ("C-M-f" . sp-forward-sexp)
        ("C-M-b" . sp-backward-sexp)
        ("C-M-u" . sp-backward-up-sexp)
        ("C-M-d" . sp-down-sexp)
        ("C-M-p" . sp-backward-down-sexp)
        ("C-M-n" . sp-up-sexp)
        ("M-s" . sp-splice-sexp)
        ("M-<up>" . sp-splice-sexp-killing-backward)
        ("M-<down>" . sp-splice-sexp-killing-forward)
        ("M-r" . sp-splice-sexp-killing-around)
        ("C-)" . sp-forward-slurp-sexp)
        ("C-<right>" . sp-forward-slurp-sexp)
        ("C-}" . sp-forward-barf-sexp)
        ("C-<left>" . sp-forward-barf-sexp)
        ("C-(" . sp-backward-slurp-sexp)
        ("C-M-<left>" . sp-backward-slurp-sexp)
        ("C-{" . sp-backward-barf-sexp)
        ("C-M-<right>" . sp-backward-barf-sexp)
        ("M-S" . sp-split-sexp))
  :init
  (add-hook 'lisp-mode-hook 'turn-on-smartparens-strict-mode)
  (add-hook 'emacs-lisp-mode-hook 'turn-on-smartparens-strict-mode)
  (add-hook 'clojure-mode-hook 'turn-on-smartparens-strict-mode)
  (add-hook 'js2-mode-hook 'turn-on-smartparens-strict-mode)
  :config
  (require 'smartparens-config)

  ;; Org-mode config

  (sp-with-modes 'org-mode
    (sp-local-pair "'" nil :unless '(sp-point-after-word-p))
    (sp-local-pair "*" "*" :actions '(insert wrap) :unless '(sp-point-after-word-p sp-point-at-bol-p) :wrap "C-*" :skip-match 'sp--org-skip-asterisk)
    (sp-local-pair "_" "_" :unless '(sp-point-after-word-p))
    (sp-local-pair "/" "/" :unless '(sp-point-after-word-p) :post-handlers '(("[d1]" "SPC")))
    (sp-local-pair "~" "~" :unless '(sp-point-after-word-p) :post-handlers '(("[d1]" "SPC")))
    (sp-local-pair "=" "=" :unless '(sp-point-after-word-p) :post-handlers '(("[d1]" "SPC")))
    (sp-local-pair "«" "»"))

  (defun sp--org-skip-asterisk (ms mb me)
    (or (and (= (line-beginning-position) mb)
             (eq 32 (char-after (1+ mb))))
        (and (= (1+ (line-beginning-position)) me)
             (eq 32 (char-after me))))))

(autoload 'zap-up-to-char "misc"
  "Kill up to, but not including ARGth occurrence of CHAR.

  \(fn arg char)"
  'interactive)

(bind-key* "M-z" 'zap-up-to-char)

(use-package move-text
  :bind (("M-<up>" . move-text-up)
         ("M-<down>" . move-text-down)))

(use-package flycheck
  :config
  (global-set-key (kbd "C-c h f")
                  (defhydra hydra-flycheck
                    (:pre (progn (validate-setq hydra-lv t) (flycheck-list-errors))
                          :post (progn (validate-setq hydra-lv nil) (quit-windows-on "*Flycheck errors*"))
                          :hint nil)
                    "Errors"
                    ("f"  flycheck-error-list-set-filter                            "Filter")
                    ("n"  flycheck-next-error                                       "Next")
                    ("p"  flycheck-previous-error                                   "Previous")
                    ("<" flycheck-first-error                                      "First")
                    (">"  (progn (goto-char (point-max)) (flycheck-previous-error)) "Last")
                    ("q"  nil)))
  (use-package flycheck-pos-tip
    :config (flycheck-pos-tip-mode))
  (add-hook 'prog-mode-hook 'global-flycheck-mode))

(use-package yasnippet
  :diminish yas-global-mode yas-minor-mode
  :init (add-hook 'after-init-hook 'yas-global-mode)
  :config (validate-setq yas-snippet-dirs '("~/.emacs.d/snippets/")))

(use-package company
  :diminish company-mode
  :init
  (add-hook 'after-init-hook 'global-company-mode)
  :config
  (require 'company-dabbrev)
  (validate-setq company-dabbrev-ignore-case nil
                 company-dabbrev-code-ignore-case nil
                 company-dabbrev-downcase nil
                 company-idle-delay 0
                 company-begin-commands '(self-insert-command)
                 company-transformers '(company-sort-by-occurrence))
  (use-package company-quickhelp
    :config (company-quickhelp-mode 1)))

(use-package flyspell 
  :ensure f 
  :diminish flyspell-mode
  :init
  (setenv "DICTIONARY" "en_GB")
  :config   
  (add-hook 'markdown-mode-hook 'flyspell-mode))

(use-package direnv
  :config
  (direnv-mode)
  (validate-setq direnv-always-show-summary t))

(use-package slime
  :config
  (validate-setq inferior-lisp-program "sbcl")
  (validate-setq slime-contribs '(slime-fancy))
  (use-package slime-company
    :config
    (slime-setup '(slime-company))))

(bind-key "C-c C-k" 'eval-buffer emacs-lisp-mode-map)

(use-package nix-mode
  :config
  (add-hook 'nix-mode-hook (lambda ()
                             (aggressive-indent-mode -1))))

(use-package company-nixos-options
  :config
  (add-to-list 'company-backends 'company-nixos-options))

(use-package go-mode
  :mode ("\\.go\\'" . go-mode)
  :config (progn
            (add-hook 'go-mode-hook 'compilation-auto-quit-window)
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
            (use-package go-dlv
              :config (require 'go-dlv))
            (use-package golint
              :config
              (add-to-list 'load-path (concat (getenv "GOPATH")  "/src/github.com/golang/lint/misc/emacs"))
              (require 'golint))
            (use-package gorepl-mode
              :config (add-hook 'go-mode-hook #'gorepl-mode))
            (use-package company-go
              :config (add-hook 'go-mode-hook (lambda ()
                                                (set (make-local-variable 'company-backends) '(company-go))
                                                (company-mode))))))

(add-hook 'c++-mode-hook
          (lambda ()
            (unless (file-exists-p "Makefile")
              (set (make-local-variable 'compile-command)
                   (let ((file (file-name-nondirectory buffer-file-name)))
                     (format "g++ -Wall -s -pedantic-errors %s -o %s --std=c++14"
                             file
                             (file-name-sans-extension file)))))))

(use-package fish-mode
  :mode ("\\.fish\\'" . fish-mode)
  :init (add-hook 'fish-mode-hook
                  (lambda () (aggressive-indent-mode -1))))

(use-package rust-mode
  :mode ("\\.rs\\'" . rust-mode))

(add-hook 'python-mode-hook (lambda () (aggressive-indent-mode -1)))

(eval-after-load "python-mode"
  (lambda ()
    (validate-setq python-remove-cwd-from-path t)))

(use-package sphinx-doc
  :init
  (add-hook 'python-mode-hook (lambda ()
                                (sphinx-doc-mode 1))))

(use-package anaconda-mode
  :init
  (add-hook 'python-mode-hook 'anaconda-mode)
  (add-hook 'python-mode-hook 'anaconda-eldoc-mode))

(use-package company-anaconda
  :config
  (eval-after-load "company"
    '(add-to-list 'company-backends '(company-anaconda))))

(use-package py-isort
  :defer t
  :init
  (add-hook 'before-save-hook 'py-isort-before-save))

(use-package yapfify 
  :init
  (add-hook 'python-mode-hook 'yapf-mode))

(use-package py-autopep8
  :init
  (add-hook 'python-mode-hook 'py-autopep8-enable-on-save))

(use-package pyvenv)

(use-package pytest
  :bind (:map python-mode-map
              ("C-c a" . pytest-all)
              ("C-c m" . pytest-module)
              ("C-c ." . pytest-one)
              ("C-c d" . pytest-directory)
              ("C-c p a" . pytest-pdb-all)
              ("C-c p m" . pytest-pdb-module)
              ("C-c p ." . pytest-pdb-one)))

(use-package web-mode
  :mode (("\\.html\\'" . web-mode)
         ("\\.html\\.erb\\'" . web-mode)
         ("\\.mustache\\'" . web-mode)
         ("\\.jinja\\'" . web-mode)
         ("\\.njk\\'" . web-mode)
         ("\\.php\\'" . web-mode))
  :config
  (validate-setq web-mode-enable-css-colorization t)
  (validate-setq web-mode-code-indent-offset 2)
  (validate-setq web-mode-markup-indent-offset 2))

(use-package emmet-mode
  :diminish emmet-mode
  :config
  (add-hook 'web-mode-hook 'emmet-mode)
  (add-hook 'vue-mode-hook 'emmet-mode))

(use-package scss-mode
  :mode "\\.scss\\'" 
  :config (progn
            (validate-setq scss-compile-at-save nil)))

(setq-default flycheck-disabled-checkers
	(append flycheck-disabled-checkers
		'(javascript-jshint)))
(flycheck-add-mode 'javascript-eslint 'js2-mode)
(flycheck-add-mode 'javascript-eslint 'web-mode)

(use-package skewer-mode  
  :bind (:map skewer-mode-map
              ("C-c C-k" . skewer-load-buffer))
  :config
  (add-hook 'js2-mode-hook 'skewer-mode))

(use-package js2-mode
  :mode ("\\.js\\'" . js2-mode)
  :config
  (use-package tern
    :diminish tern-mode
    :config
    (validate-setq js-switch-indent-offset 2)
    (add-hook 'js2-mode-hook 'tern-mode) 
    (use-package company-tern
      :config
      (add-to-list 'company-backends 'company-tern))))

(use-package js-doc
  :config
  (validate-setq js-doc-mail-address "jethrokuan95@gmail.com"
        js-doc-author (format "Jethro Kuan <%s>" js-doc-mail-address)
        js-doc-url "http://www.jethrokuan.com/"
        js-doc-license "MIT")
  (add-hook 'js2-mode-hook
            #'(lambda ()
                (define-key js2-mode-map "\C-ci" 'js-doc-insert-function-doc)
                (define-key js2-mode-map "@" 'js-doc-insert-tag))))

(use-package js2-refactor
  :config
  (add-hook 'js2-mode-hook #'js2-refactor-mode)
  (js2r-add-keybindings-with-prefix "C-c C-j"))

(use-package vue-mode
  :mode "\\.vue\\'")

(use-package json-mode
  :mode "\\.json\\'"
  :config (add-hook 'json-mode-hook (lambda ()
                                      (make-local-variable 'js-indent-level)
                                      (validate-setq js-indent-level 2))))

(use-package markdown-mode
  :mode ("\\.md\\'" . markdown-mode)
  :config (progn
            (validate-setq markdown-command "multimarkdown")
            (add-hook 'markdown-mode-hook #'trunc-lines-hook)))

(use-package clojure-mode
  :mode (("\\.clj\\'" . clojure-mode)
         ("\\.boot\\'" . clojure-mode)
         ("\\.edn\\'" . clojure-mode)
         ("\\.cljs\\'" . clojurescript-mode)
         ("\\.cljs\\.hl\\'" . clojurescript-mode))
  :init
  (add-hook 'clojure-mode-hook #'eldoc-mode)
  (add-hook 'clojure-mode-hook #'subword-mode)
  (add-hook 'clojure-mode-hook #'cider-mode)
  (add-hook 'clojure-mode-hook #'clj-refactor-mode))

(use-package cider
  :init
  (add-hook 'cider-mode-hook #'clj-refactor-mode)
  (add-hook 'cider-repl-mode-hook #'company-mode)
  (add-hook 'cider-mode-hook #'company-mode)
  :diminish subword-mode
  :config
  (validate-setq nrepl-log-messages t                  
        cider-repl-display-in-current-window t
        cider-repl-use-clojure-font-lock t    
        cider-prompt-save-file-on-load 'always-save
        cider-font-lock-dynamically '(macro core function var)
        nrepl-hide-special-buffers t
        cider-show-error-buffer nil
        cider-overlays-use-font-lock t
        cider-repl-result-prefix ";; => ")
  (validate-setq cider-cljs-lein-repl "(do (use 'figwheel-sidecar.repl-api) (start-figwheel!) (cljs-repl))")
  (cider-repl-toggle-pretty-printing))

(use-package clj-refactor
  :defines cljr-add-keybindings-with-prefix
  :diminish clj-refactor-mode
  :config (cljr-add-keybindings-with-prefix "C-c C-j"))

(use-package flycheck-clojure
  :config
  (flycheck-clojure-setup))

(use-package auctex
  :defer t
  :config
  (validate-setq TeX-auto-save t
        TeX-parse-self t
        TeX-syntactic-comment t
        ;; Synctex support
        TeX-source-correlate-start-server nil
        ;; Don't insert line-break at inline math
        LaTeX-fill-break-at-separators nil)
  (validate-setq TeX-view-program-list '(("Evince" "evince --page-index=%(outpage) %o")
                                ("qpdfview" "qpdfview %o#%(outpage)")))
  (validate-setq TeX-view-program-selection '((output-pdf "qpdfview")
                                     (output-pdf "Evince")))
  (when latex-enable-auto-fill
    (add-hook 'LaTeX-mode-hook 'latex/auto-fill-mode))
  (when latex-enable-folding
    (add-hook 'LaTeX-mode-hook 'TeX-fold-mode))
  (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
  (add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode)
  (add-hook 'LaTeX-mode-hook 'TeX-PDF-mode))

(use-package company-auctex
  :defer t)

(global-hl-line-mode 1)

(require 'whitespace)
(validate-setq whitespace-line-column 80) ;; limit line length
(validate-setq whitespace-style '(face lines-tail))

(add-hook 'prog-mode-hook 'whitespace-mode)

(use-package page-break-lines)

(display-time-mode 1)
(eval-after-load "display-time-mode"
  (validate-setq display-time-24hr-format t))

(defhydra hydra-zoom (global-map "<f2>")
  "zoom"
  ("i" text-scale-increase "in")
  ("o" text-scale-decrease "out"))

(use-package beacon
  :diminish beacon-mode
  :config
  (beacon-mode 1)
  (validate-setq beacon-push-mark 10))

(show-paren-mode 1)
(validate-setq show-paren-delay 0)

(use-package golden-ratio
  :diminish golden-ratio-mode
  :config (progn
            (add-to-list 'golden-ratio-extra-commands 'ace-window)
            (golden-ratio-mode 1)))

(use-package volatile-highlights
  :diminish volatile-highlights-mode
  :config (volatile-highlights-mode t))

(use-package git-gutter-fringe+
  :diminish git-gutter+-mode
  :config
  (global-git-gutter+-mode)
  (set-face-foreground 'git-gutter+-modified "gold1")
  (set-face-foreground 'git-gutter+-added    "SeaGreen")
  (set-face-foreground 'git-gutter+-deleted  "IndianRed")
  (validate-setq git-gutter-fr+-side 'left-fringe))

(use-package org-plus-contrib
  :bind (("C-c l" . org-store-link)
         ("C-c a" . org-agenda) 
         ("C-c c" . org-capture))
  :config
  (add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\|txt\\)$" . org-mode)))

(add-hook 'org-mode-hook (lambda ()
                           (auto-fill-mode 1)))

(validate-setq org-agenda-files (file-expand-wildcards "~/.org/gtd/[a-zA-z]*.org"))

(defvar jethro/delete-frame-after-capture nil
  "Whether to delete the last frame after the current capture")

(defun jethro/delete-frame-if-neccessary (&rest r)
  (if (or (equal "capture" (frame-parameter nil 'name))
          jethro/delete-frame-after-capture)
      (progn
        (validate-setq jethro/delete-frame-after-capture nil)
        (delete-frame))
    (validate-setq jethro/delete-frame-after-capture nil)))

(defadvice org-switch-to-buffer-other-window
    (after supress-window-splitting activate)
  "Delete the extra window if we're in a capture frame"
  (if (equal "capture" (frame-parameter nil 'name))
      (delete-other-windows)))

(defun make-capture-frame ()
  "Create a new frame and run org-capture."
  (interactive)
  (select-frame
   (make-frame '((window-system . x)
                 (name . "capture")
                 (width . 120)
                 (height . 15)))) 
  (validate-setq word-wrap 1)
  (validate-setq truncate-lines nil)
  (validate-setq jethro/delete-frame-after-capture t)
  (org-capture nil "t"))

(advice-add 'org-capture-finalize :after 'jethro/delete-frame-if-neccessary)
(advice-add 'org-capture-kill :after 'jethro/delete-frame-if-neccessary)
(advice-add 'org-capture-refile :after 'jethro/delete-frame-if-neccessary)

; Targets include this file and any file contributing to the agenda - up to 9 levels deep
(validate-setq org-refile-targets (quote ((nil :maxlevel . 9)
                                 (org-agenda-files :maxlevel . 9))))

; Use full outline paths for refile targets - we file directly with IDO
(validate-setq org-refile-use-outline-path t)

; Targets complete directly with IDO
(validate-setq org-outline-path-complete-in-steps nil)

; Allow refile to create parent tasks with confirmation
(validate-setq org-refile-allow-creating-parent-nodes 'confirm)

(validate-setq org-completion-use-ido t)

;;;; Refile settings
(defun jethro/verify-refile-target ()
  "Exclude todo keywords with a done state from refile targets"
  (not (member (nth 2 (org-heading-components)) org-done-keywords)))

(validate-setq org-refile-target-verify-function 'jethro/verify-refile-target)

;; Do not dim blocked tasks
(validate-setq org-agenda-dim-blocked-tasks nil)

;; Compact the block agenda view
(validate-setq org-agenda-compact-blocks t)

;; Custom agenda command definitions
(validate-setq org-agenda-custom-commands
      '((" " "Agenda"
         ((agenda "" nil)
          (tags "REFILE"
                ((org-agenda-overriding-header "Tasks to Refile")
                 (org-tags-match-list-sublevels nil)))
          (todo "NEXT"
                ((org-agenda-overriding-header "School Next Tasks")
                 (org-agenda-todo-ignore-scheduled t)
                 (org-agenda-todo-ignore-deadlines t)
                 (org-agenda-files '("~/.org/gtd/school.org"))))
          (todo "TODO"
                ((org-agenda-overriding-header "School Todos")
                 (org-agenda-todo-ignore-scheduled t)
                 (org-agenda-todo-ignore-deadlines t) 
                 (org-agenda-files '("~/.org/gtd/school.org"))))
          (tags-todo "-CANCELLED/!"
                     ((org-agenda-overriding-header "Stuck Projects")
                      (org-agenda-skip-function 'bh/skip-non-stuck-projects)
                      (org-agenda-sorting-strategy '(category-keep)))) 
          (tags-todo "-HOLD-CANCELLED/!"
                     ((org-agenda-overriding-header "Projects")
                      (org-agenda-skip-function 'bh/skip-non-projects)
                      (org-tags-match-list-sublevels 'indented)
                      (org-agenda-sorting-strategy '(category-keep))))
          (tags-todo "-CANCELLED/!NEXT"
                     ((org-agenda-overriding-header (concat "Project Next Tasks"
                                                            (if bh/hide-scheduled-and-waiting-next-tasks
                                                                ""
                                                              " (including WAITING and SCHEDULED tasks)")))
                      (org-agenda-skip-function 'bh/skip-projects-and-habits-and-single-tasks)
                      (org-tags-match-list-sublevels t)
                      (org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
                      (org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)
                      (org-agenda-todo-ignore-with-date bh/hide-scheduled-and-waiting-next-tasks)
                      (org-agenda-sorting-strategy
                       '(todo-state-down effort-up category-keep))))
          (tags-todo "-REFILE-CANCELLED-WAITING-HOLD/!"
                     ((org-agenda-overriding-header (concat "Project Subtasks"
                                                            (if bh/hide-scheduled-and-waiting-next-tasks
                                                                ""
                                                              " (including WAITING and SCHEDULED tasks)")))
                      (org-agenda-skip-function 'bh/skip-non-project-tasks)
                      (org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
                      (org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)
                      (org-agenda-todo-ignore-with-date bh/hide-scheduled-and-waiting-next-tasks)
                      (org-agenda-sorting-strategy
                       '(category-keep))))
          (tags-todo "-REFILE-CANCELLED-WAITING-HOLD/!"
                     ((org-agenda-overriding-header (concat "Standalone Tasks"
                                                            (if bh/hide-scheduled-and-waiting-next-tasks
                                                                ""
                                                              " (including WAITING and SCHEDULED tasks)")))
                      (org-agenda-files (remove "~/.org/gtd/school.org" org-agenda-files))
                      (org-agenda-skip-function 'bh/skip-project-tasks)
                      (org-agenda-todo-ignore-scheduled t)
                      (org-agenda-todo-ignore-deadlines t)
                      (org-agenda-todo-ignore-with-date t)
                      (org-agenda-sorting-strategy
                       '(category-keep))))
          (tags-todo "-CANCELLED+WAITING|HOLD/!"
                     ((org-agenda-overriding-header (concat "Waiting and Postponed Tasks"
                                                            (if bh/hide-scheduled-and-waiting-next-tasks
                                                                ""
                                                              " (including WAITING and SCHEDULED tasks)")))
                      (org-agenda-skip-function 'bh/skip-non-tasks)
                      (org-tags-match-list-sublevels nil)
                      (org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
                      (org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks))) 
          (tags "-REFILE/"
                ((org-agenda-overriding-header "Tasks to Archive")
                 (org-agenda-skip-function 'bh/skip-non-archivable-tasks)
                 (org-tags-match-list-sublevels nil))))
         nil)))

(defun bh/org-auto-exclude-function (tag)
  "Automatic task exclusion in the agenda with / RET"
  (and (cond
        ((string= tag "hold")
         t)
        ((string= tag "farm")
         t))
       (concat "-" tag)))

(validate-setq org-agenda-auto-exclude-function 'bh/org-auto-exclude-function)

; Tags with fast selection keys
(validate-setq org-tag-alist (quote ((:startgroup)
                            ("@errand" . ?e)
                            ("@office" . ?o)
                            ("@home" . ?H)
                            ("@farm" . ?f)
                            (:endgroup)
                            ("WAITING" . ?w)
                            ("HOLD" . ?h)
                            ("PERSONAL" . ?P)
                            ("WORK" . ?W)
                            ("FARM" . ?F)
                            ("ORG" . ?O)
                            ("NORANG" . ?N)
                            ("crypt" . ?E)
                            ("NOTE" . ?n)
                            ("CANCELLED" . ?c)
                            ("FLAGGED" . ??))))

; Allow setting single tags without the menu
(validate-setq org-fast-tag-selection-single-key (quote expert))

; For tag searches ignore tasks with scheduled and deadline dates
(validate-setq org-agenda-tags-todo-honor-ignore-options t)

(validate-setq org-agenda-span 'day)

(validate-setq org-stuck-projects (quote ("" nil nil "")))

(defun bh/is-project-p ()
  "Any task with a todo keyword subtask"
  (save-restriction
    (widen)
    (let ((has-subtask)
          (subtree-end (save-excursion (org-end-of-subtree t)))
          (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
      (save-excursion
        (forward-line 1)
        (while (and (not has-subtask)
                    (< (point) subtree-end)
                    (re-search-forward "^\*+ " subtree-end t))
          (when (member (org-get-todo-state) org-todo-keywords-1)
            (validate-setq has-subtask t))))
      (and is-a-task has-subtask))))

(defun bh/is-project-subtree-p ()
  "Any task with a todo keyword that is in a project subtree.
Callers of this function already widen the buffer view."
  (let ((task (save-excursion (org-back-to-heading 'invisible-ok)
                              (point))))
    (save-excursion
      (bh/find-project-task)
      (if (equal (point) task)
          nil
        t))))

(defun bh/is-task-p ()
  "Any task with a todo keyword and no subtask"
  (save-restriction
    (widen)
    (let ((has-subtask)
          (subtree-end (save-excursion (org-end-of-subtree t)))
          (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
      (save-excursion
        (forward-line 1)
        (while (and (not has-subtask)
                    (< (point) subtree-end)
                    (re-search-forward "^\*+ " subtree-end t))
          (when (member (org-get-todo-state) org-todo-keywords-1)
            (validate-setq has-subtask t))))
      (and is-a-task (not has-subtask)))))

(defun bh/is-subproject-p ()
  "Any task which is a subtask of another project"
  (let ((is-subproject)
        (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
    (save-excursion
      (while (and (not is-subproject) (org-up-heading-safe))
        (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
          (validate-setq is-subproject t))))
    (and is-a-task is-subproject)))

(defun bh/list-sublevels-for-projects-indented ()
  "Set org-tags-match-list-sublevels so when restricted to a subtree we list all subtasks.
  This is normally used by skipping functions where this variable is already local to the agenda."
  (if (marker-buffer org-agenda-restrict-begin)
      (validate-setq org-tags-match-list-sublevels 'indented)
    (validate-setq org-tags-match-list-sublevels nil))
  nil)

(defun bh/list-sublevels-for-projects ()
  "Set org-tags-match-list-sublevels so when restricted to a subtree we list all subtasks.
  This is normally used by skipping functions where this variable is already local to the agenda."
  (if (marker-buffer org-agenda-restrict-begin)
      (validate-setq org-tags-match-list-sublevels t)
    (validate-setq org-tags-match-list-sublevels nil))
  nil)

(defvar bh/hide-scheduled-and-waiting-next-tasks t)

(defun bh/toggle-next-task-display ()
  (interactive)
  (validate-setq bh/hide-scheduled-and-waiting-next-tasks (not bh/hide-scheduled-and-waiting-next-tasks))
  (when  (equal major-mode 'org-agenda-mode)
    (org-agenda-redo))
  (message "%s WAITING and SCHEDULED NEXT Tasks" (if bh/hide-scheduled-and-waiting-next-tasks "Hide" "Show")))

(defun bh/skip-stuck-projects ()
  "Skip trees that are not stuck projects"
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (if (bh/is-project-p)
          (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
                 (has-next ))
            (save-excursion
              (forward-line 1)
              (while (and (not has-next) (< (point) subtree-end) (re-search-forward "^\\*+ NEXT " subtree-end t))
                (unless (member "WAITING" (org-get-tags-at))
                  (validate-setq has-next t))))
            (if has-next
                nil
              next-headline)) ; a stuck project, has subtasks but no next task
        nil))))

(defun bh/skip-non-stuck-projects ()
  "Skip trees that are not stuck projects"
  ;; (bh/list-sublevels-for-projects-indented)
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (if (bh/is-project-p)
          (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
                 (has-next ))
            (save-excursion
              (forward-line 1)
              (while (and (not has-next) (< (point) subtree-end) (re-search-forward "^\\*+ NEXT " subtree-end t))
                (unless (member "WAITING" (org-get-tags-at))
                  (validate-setq has-next t))))
            (if has-next
                next-headline
              nil)) ; a stuck project, has subtasks but no next task
        next-headline))))

(defun bh/skip-non-projects ()
  "Skip trees that are not projects"
  ;; (bh/list-sublevels-for-projects-indented)
  (if (save-excursion (bh/skip-non-stuck-projects))
      (save-restriction
        (widen)
        (let ((subtree-end (save-excursion (org-end-of-subtree t))))
          (cond
           ((bh/is-project-p)
            nil)
           ((and (bh/is-project-subtree-p) (not (bh/is-task-p)))
            nil)
           (t
            subtree-end))))
    (save-excursion (org-end-of-subtree t))))

(defun bh/skip-non-tasks ()
  "Show non-project tasks.
Skip project and sub-project tasks, and project related tasks."
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (cond
       ((bh/is-task-p)
        nil)
       (t
        next-headline)))))

(defun bh/skip-project-trees-and-habits ()
  "Skip trees that are projects"
  (save-restriction
    (widen)
    (let ((subtree-end (save-excursion (org-end-of-subtree t))))
      (cond
       ((bh/is-project-p)
        subtree-end) 
       (t
        nil)))))

(defun bh/skip-projects-and-habits-and-single-tasks ()
  "Skip trees that are projects, tasks that are habits, single non-project tasks"
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (cond 
       ((and bh/hide-scheduled-and-waiting-next-tasks
             (member "WAITING" (org-get-tags-at)))
        next-headline)
       ((bh/is-project-p)
        next-headline)
       ((and (bh/is-task-p) (not (bh/is-project-subtree-p)))
        next-headline)
       (t
        nil)))))

(defun bh/skip-project-tasks-maybe ()
  "Show tasks related to the current restriction.
When restricted to a project, skip project and sub project tasks, habits, NEXT tasks, and loose tasks.
When not restricted, skip project and sub-project tasks, habits, and project related tasks."
  (save-restriction
    (widen)
    (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
           (next-headline (save-excursion (or (outline-next-heading) (point-max))))
           (limit-to-project (marker-buffer org-agenda-restrict-begin)))
      (cond
       ((bh/is-project-p)
        next-headline) 
       ((and (not limit-to-project)
             (bh/is-project-subtree-p))
        subtree-end)
       ((and limit-to-project
             (bh/is-project-subtree-p)
             (member (org-get-todo-state) (list "NEXT")))
        subtree-end)
       (t
        nil)))))

(defun bh/skip-project-tasks ()
  "Show non-project tasks.
Skip project and sub-project tasks, and project related tasks."
  (save-restriction
    (widen)
    (let* ((subtree-end (save-excursion (org-end-of-subtree t))))
      (cond
       ((bh/is-project-p)
        subtree-end) 
       ((bh/is-project-subtree-p)
        subtree-end)
       (t
        nil)))))

(defun bh/skip-non-project-tasks ()
  "Show project tasks.
Skip project and sub-project tasks, habits, and loose non-project tasks."
  (save-restriction
    (widen)
    (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
           (next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (cond
       ((bh/is-project-p) next-headline) 
       ((and (bh/is-project-subtree-p)
             (member (org-get-todo-state) (list "NEXT")))
        subtree-end)
       ((not (bh/is-project-subtree-p))
        subtree-end)
       (t
        nil)))))

(defun bh/skip-projects-and-habits ()
  "Skip trees that are projects and tasks that are habits"
  (save-restriction
    (widen)
    (let ((subtree-end (save-excursion (org-end-of-subtree t))))
      (cond
       ((bh/is-project-p)
        subtree-end) 
       (t
        nil)))))

(defun bh/skip-non-subprojects ()
  "Skip trees that are not projects"
  (let ((next-headline (save-excursion (outline-next-heading))))
    (if (bh/is-subproject-p)
        nil
      next-headline)))

(defun bh/find-project-task ()
  "Move point to the parent (project) task if any"
  (save-restriction
    (widen)
    (let ((parent-task (save-excursion (org-back-to-heading 'invisible-ok) (point))))
      (while (org-up-heading-safe)
        (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
          (validate-setq parent-task (point))))
      (goto-char parent-task)
      parent-task)))

(validate-setq org-global-properties (quote (("Effort_ALL" . "0 0:10 0:20 0:30 1:00 1:30 2:00 3:00 4:00 6:00 8:00 10:00 20:00"))))

(validate-setq org-archive-mark-done nil)
(validate-setq org-archive-location "%s_archive::* Archived Tasks")

(defun bh/skip-non-archivable-tasks ()
  "Skip trees that are not available for archiving"
  (save-restriction
    (widen)
    ;; Consider only tasks with done todo headings as archivable candidates
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max))))
          (subtree-end (save-excursion (org-end-of-subtree t))))
      (if (member (org-get-todo-state) org-todo-keywords-1)
          (if (member (org-get-todo-state) org-done-keywords)
              (let* ((daynr (string-to-int (format-time-string "%d" (current-time))))
                     (a-month-ago (* 60 60 24 (+ daynr 1)))
                     (last-month (format-time-string "%Y-%m-" (time-subtract (current-time) (seconds-to-time a-month-ago))))
                     (this-month (format-time-string "%Y-%m-" (current-time)))
                     (subtree-is-current (save-excursion
                                           (forward-line 1)
                                           (and (< (point) subtree-end)
                                                (re-search-forward (concat last-month "\\|" this-month) subtree-end t)))))
                (if subtree-is-current
                    subtree-end ; Has a date in this month or last month, skip it
                  nil))  ; available to archive
            (or subtree-end (point-max)))
        next-headline))))

(use-package org-alert
  :config
  (org-alert-enable)
  (validate-setq alert-default-style 'libnotify))

(use-package htmlize
  :config
  (require 'htmlize))

(require 'org-notmuch)

(defun jethro/auto-git-commit-and-push (dir)
  (shell-command (format "cd %s && git add -A && git commit -m \"%s\" && git push origin master" dir "New changes: $(date)")))

(validate-setq jethro/emacsd-site-dir "~/Documents/Code/emacsd_site/")

(validate-setq org-publish-project-alist
      '(("emacs.d"
         :publishing-function org-html-publish-to-html
         :publishing-directory jethro/emacsd-site-dir
         :base-directory "~/.emacs.d/"
         :exclude ".*"
         :include ["init.org"]
         :completion-function (lambda () (let ((htmlfile (concat jethro/emacsd-site-dir
                                                                 "init.html")))
                                           (if (file-exists-p htmlfile)
                                               (progn
                                                 (rename-file htmlfile
                                                              (concat jethro/emacsd-site-dir
                                                                      "index.html") t)
                                                 (jethro/auto-git-commit-and-push jethro/emacsd-site-dir)))))
         :with-emphasize t
         :with-title nil
         :with-toc t
         :html-head "<link rel=\"stylesheet\" href=\"/css/emacsd.css\" type=\"text/css\">"
         :html-preamble t)))

(use-package ox-reveal
  :config
  (require 'ox-reveal))

(validate-setq org-latex-pdf-process
        '("pdflatex -shell-escape -interaction nonstopmode %f"
          "pdflatex -shell-escape -interaction nonstopmode %f"))
  (require 'ox-latex)
  (validate-setq org-latex-default-table-environment "tabular")
  (validate-setq org-latex-tables-booktabs t)
  (validate-setq org-latex-listings 'minted)
  (validate-setq org-format-latex-options (plist-put org-format-latex-options :scale 2.0))
  (validate-setq org-latex-classes
        '(("article"
           "\\documentclass[8pt]{article}
\\usepackage[margin={0.8in,1in}, a4paper]{geometry}
\\usepackage{booktabs}
\\usepackage{hyperref}
\\usepackage{minted}
\\usepackage{tabularx}
\\usepackage{parskip}
\\setlength\\columnsep{10pt}
\\setlength{\\columnseprule}{1pt}
\\usepackage[compact]{titlesec}
\\titlespacing{\\section}{0pt}{*2}{*0}
\\titlespacing{\\subsection}{0pt}{*2}{*0}
\\titlespacing{\\subsubsection}{0pt}{*2}{*0}
\\titleformat*{\\section}{\\large\\bfseries}
\\titleformat*{\\subsection}{\\normalsize\\bfseries}
\\titleformat*{\\subsubsection}{\\normalsize\\bfseries}"
           ("\\section{%s}" . "\\section*{%s}")
           ("\\subsection{%s}" . "\\subsection*{%s}")
           ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
           ("\\paragraph{%s}" . "\\paragraph*{%s}")
           ("\\subparagraph{%s}" . "\\subparagraph*{%s}")) 
          ("book"
           "\\documentclass[10pt]{memoir}
                        \\usepackage{charter}
                        \\usepackage[T1]{fontenc}
                        \\usepackage{booktabs}
                        \\usepackage{amsmath}
                        \\usepackage{minted}
                        \\usemintedstyle{borland}
                        \\usepackage{color}
                        \\usepackage{epigraph}
                        \\usepackage{enumitem}
                        \\setlist{nosep}
                        \\setlength\\epigraphwidth{13cm}
                        \\setlength\\epigraphrule{0pt}
                        \\usepackage{fontspec}
                        \\usepackage{graphicx}
                        \\usepackage{hyperref}
                        \\hypersetup {colorlinks = true, allcolors = red}
                        \\title{}
                        [NO-DEFAULT-PACKAGES]
                        [NO-PACKAGES]"
           ("\\chapter{%s}" . "\\chapter*{%s}")
           ("\\section{%s}" . "\\section*{%s}")
           ("\\subsection{%s}" . "\\subsection*{%s}")
           ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
           ("\\paragraph{%s}" . "\\paragraph*{%s}")
           ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
          ("latex-notes"
           "\\documentclass[8pt]{article}
  \\usepackage[margin={0.3in,0.3in}, a4paper,landscape]{geometry}
  \\usepackage{hyperref}
  \\usepackage{amsmath}
  \\usepackage{multicol}
  \\usepackage{booktabs}
  \\usepackage{enumitem}
  \\usepackage[compact]{titlesec}
  \\titlespacing{\\section}{0pt}{*2}{*0}
  \\titlespacing{\\subsection}{0pt}{*2}{*0}
  \\titlespacing{\\subsubsection}{0pt}{*2}{*0}
  \\titleformat*{\\section}{\\large\\bfseries}
  \\titleformat*{\\subsection}{\\normalsize\\bfseries}
  \\titleformat*{\\subsubsection}{\\normalsize\\bfseries}
  \\setlist[itemize]{leftmargin=*}
  \\setlist[enumerate]{leftmargin=*}
  \\setlength\\columnsep{10pt}
  \\setlength{\\columnseprule}{1pt}       
  \\setlist{nosep}         
  \\usepackage{minted}
  \\usemintedstyle{bw}
  \\usemintedstyle[java]{bw}
  \\setminted[]{frame=none,fontsize=\\footnotesize,linenos=false}
  "
           ("\\section{%s}" . "\\section*{%s}")
           ("\\subsection{%s}" . "\\subsection*{%s}")
           ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
           ("\\paragraph{%s}" . "\\paragraph*{%s}")
           ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))

  (defun jethro/org-multicol-to-latex (async subtreep visible-only body-only)
    (let ((contents (buffer-string))
          (buffer-name (file-name-sans-extension buffer-file-name)))
      (with-temp-buffer
        (insert "#+LATEX_CLASS: latex-notes\n")
        (insert contents)
        (goto-char (point-min))
        (org-next-visible-heading 1)
        (insert "#+BEGIN_EXPORT latex\n\\begin{multicols*}{4}\n#+END_EXPORT\n")
        (goto-char (point-max))
        (insert "#+BEGIN_EXPORT latex\n\\end{multicols*}\n#+END_EXPORT")
        (org-export-to-file 'latex (format "%s.tex" buffer-name)
          async subtreep visible-only body-only nil))))

  (defun jethro/org-multicol-to-pdf (async subtreep visible-only body-only)
    (let ((contents (buffer-string))
          (buffer-name (file-name-sans-extension buffer-file-name)))
      (with-temp-buffer
        (insert "#+LATEX_CLASS: latex-notes\n")
        (insert contents)
        (goto-char (point-min))
        (org-next-visible-heading 1)
        (insert "#+BEGIN_EXPORT latex\n\\begin{multicols*}{4}\n#+END_EXPORT\n")
        (goto-char (point-max))
        (insert "#+BEGIN_EXPORT latex\n\\end{multicols*}\n#+END_EXPORT")
        (org-export-to-file 'latex (format "%s.tex" buffer-name)
          async subtreep visible-only body-only nil
          (lambda (file) (org-latex-compile file))))))

  (org-export-define-derived-backend 'latex-notes 'latex
    :menu-entry
    '(?L "Export to LaTeX notes"
         ((?l "Export to LaTeX" jethro/org-multicol-to-latex)
          (?p "Export to PDF" jethro/org-multicol-to-pdf))))

(use-package org-download
  :config
  (setq-default org-download-image-dir "./pictures")
  (setq-default org-download-heading-lvl nil))

(defun jethro/org-sort-books ()
    (interactive)
    (let ((old-point (point)))
      (beginning-of-buffer)
      (org-sort-entries t ?a)
      (beginning-of-buffer)
      (org-sort-entries t ?o)
      (show-all)
      (org-global-cycle)
      (goto-char old-point)))

(defun jethro/org-after-save-books ()
  (org-publish "books"))

(use-package deft
  :bind* (("C-c d" . deft)
          ("C-x C-g" . deft-find-file)) 
  :bind (("C-c C-r" . deft-rename-file))
  :config
  (validate-setq deft-directory "~/.org/deft/"
        deft-extensions '("org")
        deft-use-filename-as-title t
        deft-default-extension "org"
        deft-use-filter-string-for-filename t
        deft-file-naming-rules '((noslash . "_")
                                 (nospace . "_")
                                 (case-fn . downcase))))

(use-package epresent
  :bind ("<f5>"))

(use-package smerge-mode
  :config
  (global-set-key (kbd "C-c h s")
                  (defhydra hydra-smerge (:pre (smerge-mode 1) :color red :post (smerge-mode -1))
                    "Smerge mode"
                    ("<down>" smerge-next        "Next conflict")
                    ("<up>"   smerge-prev        "Previous conflict")
                    ("M-a"    smerge-keep-all    "Keep all")
                    ("M-m"    smerge-keep-mine   "Keep mine")
                    ("M-o"    smerge-keep-other  "Keep other"))))

(use-package magit  
  :bind (("s-g" . magit-status)
         ("s-G" . magit-blame))
  :init (validate-setq magit-auto-revert-mode nil)
  :config
  ;; (use-package magithub)
  (add-hook 'magit-mode-hook 'hl-line-mode))

(use-package projectile
  :demand t
  :init (projectile-global-mode 1)
  :bind-keymap* ("C-x p" . projectile-command-map)
  :config
  (require 'projectile)
  (use-package counsel-projectile 
    :bind (("s-f" . counsel-projectile-find-file)
           ("s-b" . counsel-projectile-switch-to-buffer))
    :config
    (counsel-projectile-on))
  (validate-setq projectile-use-git-grep t)
  (validate-setq projectile-create-missing-test-files t)
  (validate-setq projectile-completion-system 'ivy))

(validate-setq projectile-switch-project-action
      #'projectile-commander)
(def-projectile-commander-method ?S
  "Run a search in the project"
  (counsel-projectile-rg))
(def-projectile-commander-method ?s
  "Open a *eshell* buffer for the project."
  (projectile-run-eshell))
(def-projectile-commander-method ?c
  "Run `compile' in the project."
  (projectile-compile-project nil))
(def-projectile-commander-method ?\C-?
  "Go back to project selection."
  (projectile-switch-project))
(def-projectile-commander-method ?d
  "Open project root in dired."
  (projectile-dired))
(def-projectile-commander-method ?F
  "Git fetch."
  (magit-status)
  (call-interactively #'magit-fetch-all))
(def-projectile-commander-method ?j
  "Jack-in."
  (let* ((opts (projectile-current-project-files))
         (file (ivy-read
                "Find file: " 
                opts)))
    (find-file (expand-file-name
                file (projectile-project-root)))
    (run-hooks 'projectile-find-file-hook)
    (cider-jack-in)))

(use-package esup
  :defer t)

(use-package keyfreq
  :config
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1))

(use-package which-key
  :diminish which-key-mode
  :config (add-hook 'after-init-hook 'which-key-mode))

(use-package nameless
  :diminish nameless-mode
  :config
  (add-hook 'emacs-lisp-mode-hook 'nameless-mode-from-hook)
  (validate-setq nameless-global-aliases
        '(("fl" . "font-lock")
          ("s" . "seq")
          ("me" . "macroexp")
          ("c" . "cider")
          ("q" . "queue"))))

(use-package firestarter
  :bind ("C-c M s" . firestarter-mode)
  :init (put 'firestarter 'safe-local-variable 'identity))

(use-package paradox
  :commands paradox-list-packages)

(use-package darkroom
  :bind (("C-c M d" . darkroom-mode)
         ("C-c M t" . darkroom-tentative-mode)))

(use-package bury-successful-compilation
  :config
  (bury-successful-compilation 1))
