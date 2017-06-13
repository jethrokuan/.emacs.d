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

(defun reload-init ()
  (interactive)
  (load-file "~/.emacs.d/init.el"))

(global-set-key (kbd "<f11>") 'reload-init)

(setq gc-cons-threshold 50000000)
(setq large-file-warning-threshold 100000000)

(setq user-full-name "Jethro Kuan"
      user-mail-address "jethrokuan95@gmail.com")

(global-auto-revert-mode 1)

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

(add-to-list 'initial-frame-alist
             '(font . "Iosevka-12"))
(add-to-list 'default-frame-alist
             '(font . "Iosevka-12"))

(tooltip-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)

(defalias 'yes-or-no-p 'y-or-n-p)

(delete-selection-mode 1)

(setq sentence-end-double-space nil)

(setq-default tab-width 2)
(setq-default js-indent-level 2)
(setq-default indent-tabs-mode nil)

(setq-default truncate-lines t)

(defun truncate-lines-hook ()
  (setq truncate-lines nil))

(add-hook 'text-mode-hook 'truncate-lines-hook)

(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(load "~/.emacs.d/secrets.el" t)

(use-package exec-path-from-shell 
  :config
  (exec-path-from-shell-initialize))

(use-package zenburn-theme
    :init
    (load-theme 'zenburn t))

(defun jethro/nuke-all-buffers ()
  (interactive)
  (mapcar 'kill-buffer (buffer-list))
  (delete-other-windows))

(bind-key* "C-c !" 'jethro/nuke-all-buffers)

(bind-key* "C-x m" 'eshell)

(bind-key* "M-p" 'mark-paragraph)

(bind-key* "<f9>" (lambda ()
                    (interactive)
                    (setq-local compilation-read-command nil)
                    (call-interactively 'compile)))

(use-package hydra)

(use-package flx)

(use-package flx-isearch
  :bind (("C-M-s" . flx-isearch-forward)
         ("C-M-r" . flx-isearch-backward)))

(use-package counsel
  :diminish ivy-mode
  :bind*
  (("C-c C-r" . ivy-resume)
   ("M-a" . counsel-M-x)
   ("C-c i" . counsel-imenu)
   ("C-x C-f" . counsel-find-file)
   ("C-x j" . counsel-dired-jump)
   ("C-x l" . counsel-locate)
   ("C-c j" . counsel-git)
   ("C-c s" . counsel-projectile-rg)
   ("C-c f" . counsel-recentf)
   ("M-y" . counsel-yank-pop))
  :bind ((:map help-map
               ("f" . counsel-describe-function)
               ("v" . counsel-describe-variable)
               ("l" . counsel-info-lookup-symbol))
         (:map ivy-minibuffer-map
               ("C-d" . ivy-dired)
               ("C-o" . ivy-occur))
         (:map read-expression-map
               ("C-r" . counsel-expression-history))
         (:map ivy-minibuffer-map
               ("<return>" . ivy-alt-done)
               ("M-<return" . ivy-immediate-done)))
  :init
  (add-hook 'after-init-hook (lambda () (ivy-mode 1)))
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
  (setq counsel-find-file-at-point t)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-display-style 'fancy)
  (setq ivy-initial-inputs-alist nil)
  (setq ivy-re-builders-alist
        '((ivy-switch-buffer . ivy--regex-plus)
          (swiper . ivy--regex-plus)
          (t . ivy--regex-fuzzy))) 
  (ivy-set-actions
   t
   '(("I" insert "insert"))))

(require 'whitespace)
(setq whitespace-line-column 80) ;; limit line length
(setq whitespace-style '(face lines-tail))

(add-hook 'prog-mode-hook 'whitespace-mode)

(use-package page-break-lines)

(use-package smart-mode-line
  :init
  (add-hook 'after-init-hook 'sml/setup)
  :config 
  (setq sml/theme 'respectful)
  (setq sml/name-width 30)
  (setq sml/shorten-directory t)
  (setq sml/shorten-modes t)
  (setq sml/mode-width 'full)
  (setq sml/replacer-regexp-list
                 '(("^~/.org/" ":O:")
                   ("^~/\\.emacs\\.d/" ":ED")))
  (setq rm-blacklist
                 (format "^ \\(%s\\)$"
                         (mapconcat #'identity
                                    '("FlyC.*"
                                      "Projectile.*"
                                      "GitGutter"
                                      "ivy"
                                      "company"
                                      ""
                                      "doom"
                                      ","
                                      "ElDoc")
                                    "\\|"))))

(display-time-mode 1)
(eval-after-load "display-time-mode"
  (setq display-time-24hr-format t))

(defhydra hydra-zoom (global-map "<f2>")
  "zoom"
  ("i" text-scale-increase "in")
  ("o" text-scale-decrease "out"))

(use-package beacon
  :diminish beacon-mode
  :init
  (add-hook 'after-init-hook 'beacon-mode)
  :config 
  (setq beacon-push-mark 10))

(show-paren-mode 1)
(setq show-paren-delay 0)

(use-package golden-ratio
  :diminish golden-ratio-mode
  :init
  (add-hook 'after-init-hook 'golden-ratio-mode))

(use-package volatile-highlights
  :diminish volatile-highlights-mode
  :init
  (add-hook 'after-init-hook 'volatile-highlights-mode))

(use-package git-gutter-fringe+
  :diminish git-gutter+-mode
  :config
  (global-git-gutter+-mode)
  (set-face-foreground 'git-gutter+-modified "gold1")
  (set-face-foreground 'git-gutter+-added    "SeaGreen")
  (set-face-foreground 'git-gutter+-deleted  "IndianRed")
  (setq git-gutter-fr+-side 'left-fringe))

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

(use-package anzu
  :diminish anzu-mode
  :init
  (add-hook 'after-init-hook 'global-anzu-mode)
  :config
  (define-key isearch-mode-map [remap isearch-query-replace]  #'anzu-isearch-query-replace)
  (define-key isearch-mode-map [remap isearch-query-replace-regexp] #'anzu-isearch-query-replace-regexp))

(use-package avy
  :bind* (("C-'" . avy-goto-char)
          ("C-," . avy-goto-char-2))
  :config
  (setq avy-keys '(?h ?t ?n ?s)))

(use-package windmove 
  :config
  ;; use command key on Mac
  (windmove-default-keybindings 'super)
  ;; wrap around at edges
  (setq windmove-wrap-around t))

(require 'dired)

(let ((gls "/usr/local/bin/gls"))
  (if (file-exists-p gls) (setq insert-directory-program gls)))

(setq delete-by-moving-to-trash t)

(require 'find-dired)
(setq find-ls-option '("-print0 | xargs -0 ls -ld" . "-ld"))

(setq dired-listing-switches "-aBhl  --group-directories-first")

(setq dired-recursive-copies (quote always))
(setq dired-recursive-deletes (quote top))

(require 'dired-x)

(use-package wdired
  :config
  (setq wdired-allow-to-change-permissions t))

(use-package dired-k
  :config
  (define-key dired-mode-map (kbd "K") 'dired-k)
  (setq dired-k-style 'git))

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

(defun jethro/align-repeat (start end regexp &optional justify-right after)
  "Repeat alignment with respect to the given regular expression.
If JUSTIFY-RIGHT is non nil justify to the right instead of the
left. If AFTER is non-nil, add whitespace to the left instead of
the right."
  (interactive "r\nsAlign regexp: ")
  (let* ((ws-regexp (if (string-empty-p regexp)
                        "\\(\\s-+\\)"
                      "\\(\\s-*\\)"))
         (complete-regexp (if after
                              (concat regexp ws-regexp)
                            (concat ws-regexp regexp)))
         (group (if justify-right -1 1)))
    (message "%S" complete-regexp)
    (align-regexp start end complete-regexp group 1 t)))

;; Modified answer from http://emacs.stackexchange.com/questions/47/align-vertical-columns-of-numbers-on-the-decimal-point
(defun jethro/align-repeat-decimal (start end)
  "Align a table of numbers on decimal points and dollar signs (both optional)"
  (interactive "r")
  (require 'align)
  (align-region start end nil
                '((nil (regexp . "\\([\t ]*\\)\\$?\\([\t ]+[0-9]+\\)\\.?")
                       (repeat . t)
                       (group 1 2)
                       (spacing 1 1)
                       (justify nil t)))
                nil))

(defmacro jethro/create-align-repeat-x (name regexp &optional justify-right default-after)
  (let ((new-func (intern (concat "jethro/align-repeat-" name))))
    `(defun ,new-func (start end switch)
       (interactive "r\nP")
       (let ((after (not (eq (if switch t nil) (if ,default-after t nil)))))
         (jethro/align-repeat start end ,regexp ,justify-right after)))))

(jethro/create-align-repeat-x "comma" "," nil t)
(jethro/create-align-repeat-x "semicolon" ";" nil t)
(jethro/create-align-repeat-x "colon" ":" nil t)
(jethro/create-align-repeat-x "equal" "=")
(jethro/create-align-repeat-x "math-oper" "[+\\-*/]")
(jethro/create-align-repeat-x "ampersand" "&")
(jethro/create-align-repeat-x "bar" "|")
(jethro/create-align-repeat-x "left-paren" "(")
(jethro/create-align-repeat-x "right-paren" ")" t)
(jethro/create-align-repeat-x "backslash" "\\\\")

(defvar align-regexp-map nil "keymap for `align-regexp'")

(setq align-regexp-map (make-sparse-keymap))
(define-key align-regexp-map (kbd "&") 'jethro/align-repeat-ampersand)
(define-key align-regexp-map (kbd "(") 'jethro/align-repeat-left-paren)
(define-key align-regexp-map (kbd ")") 'jethro/align-repeat-right-paren)
(define-key align-regexp-map (kbd ",") 'jethro/align-repeat-comma)
(define-key align-regexp-map (kbd ".") 'jethro/align-repeat-decimal)
(define-key align-regexp-map (kbd ":") 'jethro/align-repeat-colon)
(define-key align-regexp-map (kbd ";") 'jethro/align-repeat-semicolon)
(define-key align-regexp-map (kbd "=") 'jethro/align-repeat-equal)
(define-key align-regexp-map (kbd "\\") 'jethro/align-repeat-backslash)
(define-key align-regexp-map (kbd "a") 'align)
(define-key align-regexp-map (kbd "c") 'align-current)
(define-key align-regexp-map (kbd "m") 'jethro/align-repeat-math-oper)
(define-key align-regexp-map (kbd "r") 'jethro/align-repeat)
(define-key align-regexp-map (kbd "|") 'jethro/align-repeat-bar)

(bind-key* "C-x a" align-regexp-map)

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
  :init
  (add-hook 'prog-mode-hook 'global-flycheck-mode)
  :config
  (global-set-key (kbd "C-c h f")
                  (defhydra hydra-flycheck
                    (:pre (progn (setq hydra-lv t) (flycheck-list-errors))
                          :post (progn (setq hydra-lv nil) (quit-windows-on "*Flycheck errors*"))
                          :hint nil)
                    "Errors"
                    ("f"  flycheck-error-list-set-filter                            "Filter")
                    ("n"  flycheck-next-error                                       "Next")
                    ("p"  flycheck-previous-error                                   "Previous")
                    ("<" flycheck-first-error                                      "First")
                    (">"  (progn (goto-char (point-max)) (flycheck-previous-error)) "Last")
                    ("q"  nil)))
  (use-package flycheck-pos-tip
    :init
    (add-hook 'flycheck-mode-hook 'flycheck-pos-tip-mode))
  (use-package flycheck-color-mode-line
    :init
    (add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode)))

(use-package yasnippet
  :diminish yas-global-mode yas-minor-mode
  :init (add-hook 'after-init-hook 'yas-global-mode)
  :config (setq yas-snippet-dirs '("~/.emacs.d/snippets/")))

(use-package company
  :diminish company-mode
  :init
  (add-hook 'after-init-hook 'global-company-mode)
  :config
  (require 'company-dabbrev)
  (require 'company-dabbrev-code)
  (setq company-dabbrev-ignore-case nil
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
  (add-hook 'text-mode-hook 'flyspell-mode))

(add-hook 'text-mode-hook 'auto-fill-mode)

(defun endless/fill-or-unfill ()
  "Like `fill-paragraph', but unfill if used twice."
  (interactive)
  (let ((fill-column
         (if (eq last-command 'endless/fill-or-unfill)
             (progn (setq this-command nil)
                    (point-max))
           fill-column)))
    (call-interactively #'fill-paragraph)))

(global-set-key [remap fill-paragraph]
                #'endless/fill-or-unfill)

(use-package direnv
  :config
  (direnv-mode)
  (setq direnv-always-show-summary t))

(use-package slime
  :config
  (setq inferior-lisp-program "sbcl")
  (setq slime-contribs '(slime-fancy))
  (use-package slime-company
    :config
    (slime-setup '(slime-company))))

(bind-key "C-c C-k" 'eval-buffer emacs-lisp-mode-map)

(use-package nix-mode
  :config
  (add-hook 'nix-mode-hook (lambda ()
                             (aggressive-indent-mode -1)))
  (use-package company-nixos-options
    :config
    (add-to-list 'company-backends 'company-nixos-options)))

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
    (setq python-remove-cwd-from-path t)))

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
  :commands
  (py-isort-buffer py-isort-region))

(use-package yapfify 
  :init
  (add-hook 'python-mode-hook 'yapf-mode))

(use-package pytest
  :bind (:map python-mode-map
              ("C-c a" . pytest-all)
              ("C-c m" . pytest-module)
              ("C-c ." . pytest-one)
              ("C-c d" . pytest-directory)
              ("C-c p a" . pytest-pdb-all)
              ("C-c p m" . pytest-pdb-module)
              ("C-c p ." . pytest-pdb-one)))

(use-package highlight-indent-guides
  :init
  (add-hook 'python-mode-hook 'highlight-indent-guides-mode)
  :config
  (setq highlight-indent-guides-method 'character))

(use-package web-mode
  :mode (("\\.html\\'" . web-mode)
         ("\\.html\\.erb\\'" . web-mode)
         ("\\.mustache\\'" . web-mode)
         ("\\.jinja\\'" . web-mode)
         ("\\.njk\\'" . web-mode)
         ("\\.php\\'" . web-mode))
  :config
  (setq web-mode-enable-css-colorization t)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-markup-indent-offset 2))

(use-package emmet-mode
  :diminish emmet-mode
  :config
  (add-hook 'web-mode-hook 'emmet-mode)
  (add-hook 'vue-mode-hook 'emmet-mode))

(use-package scss-mode
  :mode "\\.scss\\'" 
  :config (progn
            (setq scss-compile-at-save nil)))

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
    (setq js-switch-indent-offset 2)
    (add-hook 'js2-mode-hook 'tern-mode) 
    (use-package company-tern
      :config
      (add-to-list 'company-backends 'company-tern))))

(use-package js-doc
  :config
  (setq js-doc-mail-address "jethrokuan95@gmail.com"
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
                                      (setq js-indent-level 2))))

(use-package markdown-mode
  :mode ("\\.md\\'" . markdown-mode)
  :config (progn
            (setq markdown-command "multimarkdown")
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
  (setq nrepl-log-messages t                  
        cider-repl-display-in-current-window t
        cider-repl-use-clojure-font-lock t    
        cider-prompt-save-file-on-load 'always-save
        cider-font-lock-dynamically '(macro core function var)
        nrepl-hide-special-buffers t
        cider-show-error-buffer nil
        cider-overlays-use-font-lock t
        cider-repl-result-prefix ";; => ")
  (setq cider-cljs-lein-repl "(do (use 'figwheel-sidecar.repl-api) (start-figwheel!) (cljs-repl))")
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
  (setq TeX-auto-save t
        TeX-parse-self t
        TeX-syntactic-comment t
        ;; Synctex support
        TeX-source-correlate-start-server nil
        ;; Don't insert line-break at inline math
        LaTeX-fill-break-at-separators nil)
  (setq TeX-view-program-list '(("Evince" "evince --page-index=%(outpage) %o")
                                ("qpdfview" "qpdfview %o#%(outpage)")))
  (setq TeX-view-program-selection '((output-pdf "qpdfview")
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
  :init
  (add-hook 'magit-mode-hook 'hl-line-mode)
  :config
  (setq magit-auto-revert-mode nil))

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
  (setq projectile-use-git-grep t)
  (setq projectile-create-missing-test-files t)
  (setq projectile-completion-system 'ivy)

  (setq projectile-switch-project-action
                 #'projectile-commander)
  (def-projectile-commander-method ?S
    "Run a search in the project"
    (counsel-projectile-rg))
  (def-projectile-commander-method ?s
    "Open a *eshell* buffer for the project."
    (projectile-run-eshell))
  (def-projectile-commander-method ?d
    "Open project root in dired."
    (projectile-dired))
  (def-projectile-commander-method ?g
    "Show magit status."
    (magit-status))
  (def-projectile-commander-method ?j
    "Jack-in."
    (let* ((opts (projectile-current-project-files))
           (file (ivy-read
                  "Find file: " 
                  opts)))
      (find-file (expand-file-name
                  file (projectile-project-root)))
      (run-hooks 'projectile-find-file-hook)
      (cider-jack-in))))

(use-package sos
  :commands (sos))

(use-package which-key
  :diminish which-key-mode
  :init
  (add-hook 'after-init-hook 'which-key-mode))

(use-package darkroom
  :bind (("C-c M d" . darkroom-mode)
         ("C-c M t" . darkroom-tentative-mode)))

(use-package bury-successful-compilation
  :init
  (add-hook 'after-init-hook 'bury-successful-compilation))
