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

(setq user-full-name "Jethro Kuan"
      user-mail-address "jethrokuan95@gmail.com")

(use-package no-littering
  :config
  (require 'recentf)
  (add-to-list 'recentf-exclude no-littering-var-directory)
  (add-to-list 'recentf-exclude no-littering-etc-directory)
  (setq auto-save-file-name-transforms
        `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))))

(defvar jethro-mode-map (make-sparse-keymap)
  "Keymap for `jethro-mode'.")

(define-minor-mode jethro-mode
  "A minor mode so that my key settings override annoying major modes."
  ;; If init-value is not set to t, this mode does not get enabled in
  ;; `fundamental-mode' buffers even after doing \"(global-jethro-mode 1)\".
  ;; More info: http://emacs.stackexchange.com/q/16693/115
  :init-value t
  :lighter    " j"
  :keymap     jethro-mode-map)

(define-globalized-minor-mode global-jethro-mode jethro-mode jethro-mode)

(add-to-list 'emulation-mode-map-alists `((jethro-mode . ,jethro-mode-map)))

;; Turn off the minor mode in the minibuffer
(defun turn-off-jethro-mode ()
  "Turn off jethro-mode."
  (jethro-mode -1))

(add-hook 'minibuffer-setup-hook #'turn-off-jethro-mode)

(defun reload-init ()
  (interactive)
  (load-file "~/.emacs.d/init.el"))

(bind-key "<f11>" 'reload-init jethro-mode-map)

(setq gc-cons-threshold 50000000)
(setq large-file-warning-threshold 100000000)

(diminish 'auto-revert-mode)
(global-auto-revert-mode 1)

(setq custom-file "~/.emacs.d/custom.el")

(defalias 'yes-or-no-p 'y-or-n-p)

(add-hook 'after-init-hook 'delete-selection-mode)

(require 'recentf)
(run-at-time (* 5 60) nil
	     (lambda ()
	 (let ((inhibit-message t))
		 (recentf-save-list))))

(setq sentence-end-double-space nil)

(setq-default tab-width 2)
(setq-default js-indent-level 2)
(setq-default indent-tabs-mode nil)

(setq-default truncate-lines t)

(defun jethro/truncate-lines-hook ()
  (setq truncate-lines nil))

(add-hook 'text-mode-hook 'jethro/truncate-lines-hook)

(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(load "~/.emacs.d/secrets.el" t)

(defun jethro/nuke-all-buffers ()
  (interactive)
  (mapcar 'kill-buffer (buffer-list))
  (delete-other-windows))

(bind-key "C-c !" 'jethro/nuke-all-buffers jethro-mode-map)

(defun jethro/compile ()
  (interactive)
  (setq-local compilation-read-command nil)
  (call-interactively 'compile))

(bind-key "<f9>" 'jethro/compile jethro-mode-map)

(custom-set-faces
 '(fixed-pitch ((t (:family "Iosevka"))))
 '(variable-pitch ((t (:family "Georgia")))))

(tooltip-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)

(use-package zenburn-theme
    :init
    (load-theme 'zenburn t))

(use-package rainbow-delimiters
  :ensure t 
  :init
  (add-hook 'after-init-hook 'rainbow-delimiters-mode)
  :config
  (set-face-attribute 'rainbow-delimiters-unmatched-face nil
                      :foreground 'unspecified
                      :inherit 'error))

(blink-cursor-mode 0)

(require 'eshell)

(setq-default explicit-shell-file-name "/bin/bash")
(setq-default shell-file-name "/bin/bash")

(require 'em-smart)
(setq eshell-glob-case-insensitive nil
      eshell-error-if-no-glob nil
      eshell-scroll-to-bottom-on-input nil
      eshell-where-to-jump 'begin
      eshell-review-quick-commands nil
      eshell-smart-space-goes-to-end t)

(use-package eshell-git-prompt
  :config
  (eshell-git-prompt-use-theme 'powerline))

(defun jethro/eshell-here ()
  "Opens up a new shell in projectile root. If a prefix argument is
passed, use the buffer's directory."
  (interactive) 
  (let* ((projectile-name (projectile-project-name))
         (current-directory (car
                             (last
                              (split-string
                               (if (buffer-file-name)
                                   (file-name-directory (buffer-file-name))
                                 default-directory) "/" t)))))
    (split-window-vertically)
    (other-window 1)
    (if (equal projectile-name "-")
        (progn
          (eshell "new")
          (rename-buffer (concat "*eshell: " current-directory "*")))
      (projectile-with-default-dir (projectile-project-root)
        (eshell "new")
        (rename-buffer (concat "*eshell: " projectile-name "*"))))))

(bind-key "C-x m" 'jethro/eshell-here jethro-mode-map)

(defun eshell/x ()
  (unless (one-window-p)
    (delete-window))
  (eshell/exit))

(bind-key "C-s" 'eshell-isearch-forward eshell-mode-map)
(bind-key "C-r" 'eshell-isearch-backward eshell-mode-map)

(defun eshell/x ()
  (delete-window)
  (eshell/exit))

(use-package with-editor
  :ensure t
  :init
  (progn
    (add-hook 'shell-mode-hook  'with-editor-export-editor)
    (add-hook 'eshell-mode-hook 'with-editor-export-editor)))

(use-package eww
  :defer t
  :init
  (setq browse-url-browser-function
        '((".*google.*maps.*" . browse-url-generic)
          ;; Github goes to firefox, but not gist
          ("http.*\/\/github.com" . browse-url-generic)
          ("groups.google.com" . browse-url-generic)
          ("docs.google.com" . browse-url-generic)
          ("melpa.org" . browse-url-generic)
          ("build.*\.elastic.co" . browse-url-generic)
          (".*-ci\.elastic.co" . browse-url-generic)
          ("internal-ci\.elastic\.co" . browse-url-generic)
          ("zendesk\.com" . browse-url-generic)
          ("salesforce\.com" . browse-url-generic)
          ("stackoverflow\.com" . browse-url-generic)
          ("apache\.org\/jira" . browse-url-generic)
          ("thepoachedegg\.net" . browse-url-generic)
          ("zoom.us" . browse-url-generic)
          ("t.co" . browse-url-generic)
          ("twitter.com" . browse-url-generic)
          ("\/\/a.co" . browse-url-generic)
          ("youtube.com" . browse-url-generic)
          ("." . eww-browse-url)))
  (setq shr-external-browser 'browse-url-generic)
  (setq browse-url-generic-program (executable-find "firefox"))
  (add-hook 'eww-mode-hook #'toggle-word-wrap)
  (add-hook 'eww-mode-hook #'visual-line-mode)
  :config
  (use-package s :ensure t)
  (define-key eww-mode-map "o" 'eww)
  (define-key eww-mode-map "O" 'eww-browse-with-external-browser)
  (define-key eww-mode-map "j" 'next-line)
  (define-key eww-mode-map "k" 'previous-line)

  (use-package eww-lnum 
    :bind (:map eww-mode-map
                ("f" . eww-lnum-follow)
                ("U" . eww-lnum-universal))))

(use-package elfeed
  :bind ("<f6>" . elfeed))

(use-package dash)

(use-package hydra)

(use-package flx)

(use-package counsel
  :diminish ivy-mode
  :bind
  (:map jethro-mode-map
        ("C-c C-r" . ivy-resume)
        ("M-a" . counsel-M-x)
        ("C-s" . counsel-grep-or-swiper)
        ("C-r" . counsel-grep-or-swiper)
        ("C-c i" . counsel-imenu)
        ("C-x C-f" . counsel-find-file)
        ("C-x j" . counsel-dired-jump)
        ("C-x l" . counsel-locate)
        ("C-c j" . counsel-git)
        ("C-c f" . counsel-recentf)
        ("M-y" . counsel-yank-pop)
        :map swiper-map
        ("C-r" . ivy-previous-line)
        :map help-map
        ("f" . counsel-describe-function)
        ("v" . counsel-describe-variable)
        ("l" . counsel-info-lookup-symbol)
        :map ivy-minibuffer-map
        ("C-d" . ivy-dired)
        ("C-o" . ivy-occur)
        ("<return>" . ivy-alt-done)
        ("M-<return>" . ivy-immediate-done)
        :map read-expression-map
        ("C-r" . counsel-expression-history))
  :init
  (add-hook 'after-init-hook 'ivy-mode)
  :config
  (setq counsel-grep-swiper-limit 20000)
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
  (setq counsel-grep-base-command
        "rg -i -M 120 --no-heading --line-number --color never '%s' %s")
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

(use-package wgrep)

(use-package rg
  :bind (:map jethro-mode-map
              ("M-s" . rg)))

(require 'whitespace)
(setq whitespace-line-column 80) ;; limit line length
(setq whitespace-style '(face lines-tail))

(add-hook 'prog-mode-hook 'whitespace-mode)

(custom-set-faces
 '(mode-line ((t (:background "#2B2B2B" :foreground "#DCDCCC" :box (:line-width 4 :color "#2B2B2B"))))))

(with-eval-after-load 'hydra
  (defhydra jethro/hydra-zoom ()
    "zoom"
    ("i" text-scale-increase "in")
    ("o" text-scale-decrease "out"))

  (bind-key "C-c h z" 'jethro/hydra-zoom/body jethro-mode-map))

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

(use-package diff-hl
  :bind (:map jethro-mode-map 
              ("C-c h v" . jethro/hydra-diff-hl/body))
  :init 
  (defconst jethro/diff-hl-mode-hooks '(emacs-lisp-mode-hook
                                        conf-space-mode-hook ;.tmux.conf
                                        markdown-mode-hook
                                        css-mode-hook
                                        web-mode-hook
                                        sh-mode-hook
                                        python-mode-hook
                                        yaml-mode-hook ;tmuxp yaml configs
                                        c-mode-hook)
    "List of hooks of major modes in which diff-hl-mode should be enabled.")

  (dolist (hook jethro/diff-hl-mode-hooks)
    (add-hook hook #'diff-hl-mode))

  (defhydra jethro/hydra-diff-hl (:color red)
    "diff-hl"
    ("=" diff-hl-diff-goto-hunk "goto hunk")
    ("<RET>" diff-hl-diff-goto-hunk "goto hunk")
    ("u" diff-hl-revert-hunk "revert hunk")
    ("[" diff-hl-previous-hunk "prev hunk")
    ("p" diff-hl-previous-hunk "prev hunk")
    ("]" diff-hl-next-hunk "next hunk")
    ("n" diff-hl-next-hunk "next hunk") 
    ("q" nil "cancel"))

  (add-hook 'dired-mode-hook #'diff-hl-dired-mode))

(use-package eyebrowse
  :bind (:map jethro-mode-map
              ("M-0" . eyebrowse-switch-to-window-config-0)
              ("M-1" . eyebrowse-switch-to-window-config-1)
              ("M-2" . eyebrowse-switch-to-window-config-2)
              ("M-3" . eyebrowse-switch-to-window-config-3)
              ("M-4" . eyebrowse-switch-to-window-config-4)
              ("M-5" . eyebrowse-switch-to-window-config-5)
              ("M-6" . eyebrowse-switch-to-window-config-6)
              ("M-7" . eyebrowse-switch-to-window-config-7)
              ("M-8" . eyebrowse-switch-to-window-config-8)
              ("M-9" . eyebrowse-switch-to-window-config-9))
  :init
  (add-hook 'after-init-hook 'eyebrowse-mode))

(use-package guru-mode
  :diminish guru-mode
  :init
  (add-hook 'after-init-hook 'guru-global-mode))

(use-package crux 
  :bind (:map jethro-mode-map
              ("C-c o" . crux-open-with)
              ("C-c n" . crux-cleanup-buffer-or-region)
              ("C-c D" . crux-delete-file-and-buffer)
              ("C-a" . crux-move-beginning-of-line)
              ("M-o" . crux-smart-open-line)
              ("C-c r" . crux-rename-file-and-buffer)
              ("M-d" . crux-duplicate-current-line-or-region)
              ("M-D" . crux-duplicate-and-comment-current-line-or-region)
              ("s-o" . crux-smart-open-line-above)))

(use-package avy
  :bind
  (:map jethro-mode-map
        ("C-'" . avy-goto-char)
        ("C-," . avy-goto-char-2))
  :config
  (setq avy-keys '(?h ?t ?n ?s ?m ?w ?v ?z)))

(use-package dumb-jump
  :bind (("M-g o" . dumb-jump-go-other-window)
         ("M-g j" . dumb-jump-go)
         ("M-g i" . dumb-jump-go-prompt)
         ("M-g x" . dumb-jump-go-prefer-external)
         ("M-g z" . dumb-jump-go-prefer-external-other-window))
  :config (setq dumb-jump-selector 'ivy)
  :ensure)

(use-package windmove 
  :config
  ;; use command key on Mac
  (windmove-default-keybindings 'super)
  ;; wrap around at edges
  (setq windmove-wrap-around t))

(require 'dired)

(let ((gls "/usr/local/bin/gls"))
  (if (file-exists-p gls)
      (setq insert-directory-program gls)))

(setq delete-by-moving-to-trash t)

(require 'find-dired)
(setq find-ls-option '("-print0 | xargs -0 ls -ld" . "-ld"))

(use-package peep-dired
  :bind (:map peep-dired-mode-map
              ("SPC" . nil)
              ("<backspace>" . nil))
  (setq peep-dired-cleanup-eagerly t))

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

(defhydra jethro/window-movement ()
  ("h" windmove-left)
  ("s" windmove-right)
  ("t" windmove-down)
  ("n" windmove-up)
  ("y" other-window "other") 
  ("f" find-file "file")
  ("F" find-file-other-window "other file")
  ("v" (progn (split-window-right) (windmove-right)))
  ("o" delete-other-windows :color blue)
  ("d" delete-window "delete")
  ("q" nil))

(bind-key "M-'" 'jethro/window-movement/body jethro-mode-map)

(use-package ibuffer
  :bind (:map jethro-mode-map
              ([remap list-buffers] . ibuffer))
  :config 
  (setq ibuffer-expert t))

(use-package shackle
  :diminish shackle-mode
  :if (not (bound-and-true-p disable-pkg-shackle))
  :config
  (shackle-mode 1) 
  (setq shackle-rules 
        '((compilation-mode :select nil)
          ("*undo-tree*" :size 0.25 :align right)
          ("*eshell*" :select t :size 0.3 :align t)
          ("*Shell Command Output*" :select nil)
          ("\\*Async Shell.*\\*" :regexp t :ignore t)
          (occur-mode :select nil :align t)
          ("*Help*" :select t :inhibit-window-quit t :other t)
          ("*Completions*" :size 0.3 :align t)
          ("*Messages*" :select nil :inhibit-window-quit t :other t)
          ("\\*[Wo]*Man.*\\*" :regexp t :select t :inhibit-window-quit t :other t) 
          ("*Calendar*" :select t :size 0.3 :align below)
          ("*info*" :select t :inhibit-window-quit t :same t)
          (magit-status-mode :select t :inhibit-window-quit t :same t)
          (magit-log-mode :select t :inhibit-window-quit t :same t))))

(defun goto-match-paren (arg)
  "Go to the matching parenthesis if on parenthesis, otherwise insert %.
vi style of % jumping to matching brace."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
        (t (self-insert-command (or arg 1)))))

(bind-key "C-%" 'goto-match-paren jethro-mode-map)

(bind-key "C-c C-o" 'occur jethro-mode-map)

(use-package easy-kill
  :config
  (global-set-key [remap kill-ring-save] 'easy-kill))

(use-package visual-regexp
  :bind (:map jethro-mode-map
              ("C-M-%" . vr/query-replace)
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

(bind-key "C-x a" 'align-regexp-map jethro-mode-map)

(use-package aggressive-indent
  :diminish aggressive-indent-mode
  :config (add-hook 'prog-mode-hook 'aggressive-indent-mode))

(use-package multiple-cursors
  :bind (:map jethro-mode-map
              ("C-M-c" . mc/edit-lines)
              ("C->" . mc/mark-next-like-this)
              ("C-<" . mc/mark-previous-like-this)
              ("C-c C-<" . mc/mark-all-like-this)))

(use-package expand-region
  :bind (:map jethro-mode-map
              ("C-=" . er/expand-region)))

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
  (add-hook 'after-init-hook 'smartparens-global-strict-mode)
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

(bind-key "M-z" 'zap-up-to-char jethro-mode-map)

(use-package move-text
  :bind (:map jethro-mode-map
              ("M-<up>" . move-text-up)
              ("M-<down>" . move-text-down)))

(use-package flycheck
  :bind (:map jethro-mode-map
              ("C-c h f" . jethro/hydra-flycheck/body))
  :init
  (add-hook 'prog-mode-hook 'flycheck-mode)
  :config
  (setq-default flycheck-disabled-checkers
                (append flycheck-disabled-checkers
                        '(javascript-jshint)))
  (defhydra jethro/hydra-flycheck
    (:pre (progn (setq hydra-lv t) (flycheck-list-errors))
          :post (progn (setq hydra-lv nil) (quit-windows-on "*Flycheck errors*"))
          :hint nil)
    "Errors"
    ("f"  flycheck-error-list-set-filter                            "Filter")
    ("n"  flycheck-next-error                                       "Next")
    ("p"  flycheck-previous-error                                   "Previous")
    ("<" flycheck-first-error                                      "First")
    (">"  (progn (goto-char (point-max)) (flycheck-previous-error)) "Last")
    ("q"  nil))
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
  :bind (:map company-active-map
              ("M-n" . nil)
              ("M-p" . nil)
              ("C-n" . company-select-next)
              ("C-p" . company-select-previous))
  :init
  (add-hook 'after-init-hook 'global-company-mode)
  :config
  (setq company-dabbrev-ignore-case nil
        company-dabbrev-code-ignore-case nil
        company-dabbrev-downcase nil
        company-idle-delay 0
        company-minimum-prefix-length 2
        company-require-match nil
        company-begin-commands '(self-insert-command)
        company-transformers '(company-sort-by-occurrence))
  (use-package company-quickhelp
    :bind (:map company-active-map
                ("M-h" . company-quickhelp-manual-begin))
    :config (company-quickhelp-mode 1))
  (defun company-mode/backend-with-yas (backend)
    (if (and (listp backend) (member 'company-yasnippet backend))
        backend
      (append (if (consp backend) backend (list backend))
              '(:with company-yasnippet))))

  (setq company-backends (mapcar #'company-mode/backend-with-yas company-backends)))

(use-package flyspell 
  :ensure f 
  :diminish flyspell-mode
  :init
  (setenv "DICTIONARY" "en_GB")
  :config   
  (add-hook 'text-mode-hook 'flyspell-mode))

(add-hook 'text-mode-hook 'auto-fill-mode)

(bind-key "M-/" 'hippie-expand)

(setq hippie-expand-try-functions-list
      '(yas-hippie-try-expand
        try-expand-all-abbrevs
        try-complete-file-name-partially
        try-complete-file-name
        try-expand-dabbrev
        try-expand-dabbrev-from-kill
        try-expand-dabbrev-all-buffers
        try-expand-list
        try-expand-line
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol))

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

(defhydra jethro/hydra-draw-box (:color pink)
  "Draw box with IBM single line box characters (ESC to Quit)."
  ("ESC" nil :color blue) ;; Esc to exit.
  ("'" (lambda () (interactive) (insert "┌")) "top left ┌")
  ("," (lambda () (interactive) (insert "┬")) "top ┬")
  ("." (lambda () (interactive) (insert "┐")) "top right ┐")
  ("a" (lambda () (interactive) (insert "├")) "left ├")
  ("o" (lambda () (interactive) (insert "┼")) "center ┼")
  ("e" (lambda () (interactive) (insert "┤")) "right ┤")
  (";" (lambda () (interactive) (insert "└")) "bottom left └")
  ("q" (lambda () (interactive) (insert "┴")) "bottom ┴")
  ("j" (lambda () (interactive) (insert "┘")) "bottom right ┘")
  ("k" (lambda () (interactive) (insert "─")) "horizontal ─")
  ("x" (lambda () (interactive) (insert "│")) "vertical │"))

(bind-key "C-c h d" 'jethro/hydra-draw-box/body jethro-mode-map)

(use-package direnv
  :init
  (add-hook 'after-init-hook 'direnv-mode)
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
  :config
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
                                      (company-mode)))))

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

(use-package yapfify)

(use-package pytest
  :bind (:map python-mode-map
              ("C-c a" . pytest-all)
              ("C-c m" . pytest-module)
              ("C-c ." . pytest-one)
              ("C-c d" . pytest-directory)
              ("C-c p a" . pytest-pdb-all)
              ("C-c p m" . pytest-pdb-module)
              ("C-c p ." . pytest-pdb-one)))

(use-package realgud)

(use-package highlight-indent-guides
  :init
  (add-hook 'python-mode-hook 'highlight-indent-guides-mode)
  :config
  (setq highlight-indent-guides-method 'character))

(use-package isend-mode
  :bind
  (:map isend-mode-map
        ("C-M-e" . isend-send-defun))
  :init
  (add-hook 'isend-mode-hook 'isend-default-python-setup))

(use-package web-mode
  :mode (("\\.html\\'" . web-mode)
         ("\\.html\\.erb\\'" . web-mode)
         ("\\.mustache\\'" . web-mode)
         ("\\.jinja\\'" . web-mode)
         ("\\.njk\\'" . web-mode)
         ("\\.php\\'" . web-mode))
  :config
  (setq web-mode-enable-css-colorization t)
  (setq-default css-indent-offset 2
                web-mode-markup-indent-offset 2
                web-mode-css-indent-offset 2
                web-mode-code-indent-offset 2
                web-mode-attr-indent-offset 2))

(use-package emmet-mode
  :diminish emmet-mode
  :config
  (add-hook 'web-mode-hook 'emmet-mode)
  (add-hook 'vue-mode-hook 'emmet-mode))

(use-package scss-mode
  :mode "\\.scss\\'" 
  :config (progn
            (setq scss-compile-at-save nil)))

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

(use-package indium)

(require 'flycheck)
(flycheck-add-mode 'javascript-eslint 'js2-mode)
(flycheck-add-mode 'javascript-eslint 'web-mode)

(use-package skewer-mode  
  :bind (:map skewer-mode-map
              ("C-c C-k" . skewer-load-buffer))
  :config
  (add-hook 'js2-mode-hook 'skewer-mode))

(use-package js-doc
  :bind (:map js2-mode-map
              ("C-c i" . js-doc-insert-function-doc)
              ("@" . js-doc-insert-tag))
  :config
  (setq js-doc-mail-address "jethrokuan95@gmail.com"
        js-doc-author (format "Jethro Kuan <%s>" js-doc-mail-address)
        js-doc-url "http://www.jethrokuan.com/"
        js-doc-license "MIT"))

(use-package js2-refactor
  :config
  (add-hook 'js2-mode-hook #'js2-refactor-mode)
  (js2r-add-keybindings-with-prefix "C-c C-j"))

(use-package vue-mode
  :mode "\\.vue\\'")

(defun jethro/setup-rjsx-mode ()  
  (setq-local emmet-expand-jsx-className? t)
  (setq-local web-mode-enable-auto-quoting nil))

(use-package rjsx-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.jsx\\'" . rjsx-mode))
  (add-to-list 'auto-mode-alist '("\\.react.js\\'" . rjsx-mode))
  (add-to-list 'auto-mode-alist '("\\index.android.js\\'" . rjsx-mode))
  (add-to-list 'auto-mode-alist '("\\index.ios.js\\'" . rjsx-mode))
  (add-to-list 'magic-mode-alist '("/\\*\\* @jsx React\\.DOM \\*/" . rjsx-mode))
  (add-to-list 'magic-mode-alist '("^import React" . rjsx-mode))
  (add-hook 'rjsx-mode-hook 'jethro/setup-rjsx-mode)
  (add-hook 'rjsx-mode-hook 'tern-mode)
  (add-hook 'rjsx-mode-hook 'emmet-mode)
  :config
  (with-eval-after-load 'flycheck
    (dolist (checker '(javascript-eslint javascript-standard))
      (flycheck-add-mode checker 'rjsx-mode)))
  (defun jethro/line-align-closing-bracket ()
    "Workaround sgml-mode and align closing bracket with opening bracket"
    (save-excursion
      (beginning-of-line)
      (when (looking-at-p "^ +\/?> *$")
        (delete-char sgml-basic-offset))))
  (advice-add #'js-jsx-indent-line
              :after
              #'jethro/line-align-closing-bracket))

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

(use-package org-plus-contrib
  :bind
  (:map jethro-mode-map
        ("C-c l" . org-store-link)
        ("C-c a" . org-agenda)
        ("C-c b" . org-iswitchb)
        ("C-c c" . org-capture)))

(add-to-list 'org-structure-template-alist '("el" "#+BEGIN_SRC emacs-lisp :tangle yes?\n\n#+END_SRC"))

(setq org-agenda-files '("~/.org/gtd/inbox.org"
                         "~/.org/gtd/someday.org"
                         "~/.org/gtd/projects.org"
                         "~/.org/gtd/tickler.org"))

(setq org-capture-templates
      `(("i" "inbox" entry (file "~/.org/gtd/inbox.org")
         "* TODO %?
Captured %<%Y-%m-%d %H:%M>")
        ("w" "Web site" entry (file "~/.org/deft/websites.org")
         "* %c\n" :immediate-finish t)))

(require 'org-agenda)
(setq jethro/org-agenda-inbox-view
      `("i" "Inbox" todo ""
        ((org-agenda-files '("~/.org/gtd/inbox.org")))))

(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
        (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)")))

(setq org-log-done 'time)
(setq org-log-into-drawer t)
(setq org-log-state-notes-insert-after-drawers nil)

(setq org-tag-alist (quote ((:startgroup)
                            ("@errand" . ?e)
                            ("@office" . ?o)
                            ("@home" . ?h)
                            ("@school" . ?s)
                            (:endgroup)
                            ("WAITING" . ?w)
                            ("HOLD" . ?H)
                            ("CANCELLED" . ?c))))

(setq org-fast-tag-selection-single-key nil)

;; https://github.com/syl20bnr/spacemacs/issues/3094
(setq org-refile-use-outline-path 'file
      org-outline-path-complete-in-steps nil)
(setq org-refile-allow-creating-parent-nodes 'confirm)
(setq org-refile-targets '(("someday.org" :maxlevel . 1)
                           ("projects.org" :maxlevel . 2)))

(defun jethro/org-rename-item ()
  (interactive)
  (save-excursion
    (when (org-at-heading-p)
      (let* ((hl-text (nth 4 (org-heading-components)))
             (new-header (read-string "New Text: " nil nil hl-text)))
        (unless (or (null hl-text)
                    (org-string-match-p "^[ \t]*:[^:]+:$" hl-text))
          (beginning-of-line)
          (search-forward hl-text (point-at-eol))
          (replace-string
           hl-text
           new-header
           nil (- (point) (length hl-text)) (point)))))))

(defun jethro/org-agenda-process-inbox-item (&optional goto rfloc no-update)
  (interactive "P") 
  (org-with-wide-buffer   
   (org-agenda-set-tags) 
   (org-agenda-refile nil nil t)
   (org-mark-ring-push)
   (org-refile-goto-last-stored)
   (jethro/org-rename-item)
   (org-mark-ring-goto)
   (org-agenda-redo)))

(defun jethro/org-inbox-capture ()
  "Capture a task in agenda mode."
  (interactive)
  (org-capture nil "i"))

(define-key org-agenda-mode-map "r" 'jethro/org-agenda-process-inbox-item)
(define-key org-agenda-mode-map "c" 'jethro/org-inbox-capture)

(setq jethro/org-agenda-todo-view
      `(" " "Agenda"
        ((agenda "" nil)
         (tags-todo "@school"
                    ((org-agenda-overriding-header "School")
                     (org-agenda-skip-function #'jethro/org-agenda-skip-all-siblings-but-first)))
         (tags-todo "@home"
                    ((org-agenda-overriding-header "Home")
                     (org-agenda-skip-function #'jethro/org-agenda-skip-all-siblings-but-first)))
         (tags-todo "@office"
                    ((org-agenda-overriding-header "Office")
                     (org-agenda-skip-function #'jethro/org-agenda-skip-all-siblings-but-first))) 
         (tags-todo "@errand"
                    ((org-agenda-overriding-header "Errands")
                     (org-agenda-skip-function #'jethro/org-agenda-skip-all-siblings-but-first)))
         nil)))

(setq org-agenda-custom-commands
      `(,jethro/org-agenda-inbox-view
        ,jethro/org-agenda-todo-view))

(defun jethro/org-agenda-skip-all-siblings-but-first ()
  "Skip all but the first non-done entry."
  (let (should-skip-entry)
    (unless (or (org-current-is-todo)
                (not (org-get-scheduled-time (point))))
      (setq should-skip-entry t))
    (save-excursion
      (while (and (not should-skip-entry) (org-goto-sibling t))
        (when (org-current-is-todo)
          (setq should-skip-entry t))))
    (when should-skip-entry
      (or (outline-next-heading)
          (goto-char (point-max))))))

(defun org-current-is-todo ()
  (string= "TODO" (org-get-todo-state)))

(use-package deft
  :bind
  (:map jethro-mode-map
        ("C-c n" . deft))
  :config
  (setq deft-extension "org")
  (setq deft-text-mode 'org-mode)
  (setq deft-directory "~/.org/deft/")
  (setq deft-use-filename-as-title t))

(defun jethro/org-export-deft-file (file)
  (interactive)
  (org-html-export-to-html t t))

(define-derived-mode blog-mode org-mode "blog")
(add-to-list 'auto-mode-alist '("\\.blog\\'" . blog-mode))

(bind-key "C-c C-c" 'jethro/org-hugo-export blog-mode-map)
(bind-key "C-c TAB" 'jethro/insert-blog-props blog-mode-map)

(defun jethro/org-kwds ()
  "parse the buffer and return a cons list of (property . value)
from lines like:
#+PROPERTY: value"
  (org-element-map (org-element-parse-buffer 'element) 'keyword
    (lambda (keyword) (cons (org-element-property :key keyword)
                            (org-element-property :value keyword)))))

(defun jethro/org-kwd (KEYWORD)
  "get the value of a KEYWORD in the form of #+KEYWORD: value"
  (cdr (assoc KEYWORD (jethro/org-kwds))))

(defun now-is ()
  (concat (format-time-string "%Y-%m-%dT%T")
          ((lambda (x) (concat (substring x 0 3) ":" (substring x 3 5)))
           (format-time-string "%z"))))

(defun jethro/promote-everything () 
  "Promote all subtrees in buffer"
  (interactive)
  (save-excursion
    (save-match-data 
      (goto-char (point-min)) 
      (while (search-forward-regexp "^\\*+" nil t)
        (delete-backward-char 1)))))

;; http://whyarethingsthewaytheyare.com/setting-up-the-blog/#workflow
(defun jethro/org-hugo-export ()
  "Export current subheading to the hugo blog."
  (interactive)
  ;; Save cursor position
  (save-excursion
    ;; Go to top level heading for subtree (you can change the number 10
    ;; if you /really/ need more than 10 sublevels...)
    ;; (unless (eq (org-current-level) 1)
    ;;   (outline-up-heading 10)) 
    (let* ((hl (org-element-at-point)) 
           (title (org-element-property :title hl)) 
           (slug (org-element-property :SLUG hl))
           (filename (concat (jethro/org-kwd "HUGO_CONTENT_ROOT")
                             (format "%s.org" slug)))
           (date (org-element-property :DATE hl))
           (tags
            (format "%s"
                    (mapconcat 'identity (org-get-tags) " "))))
      ;; Make the export
      (org-copy-subtree)
      (with-temp-buffer (generate-new-buffer filename) 
                        (goto-char (point-min))
                        (org-yank)
                        (goto-char (point-min))
                        (condition-case nil
                            (while (org-promote-subtree)) 
                          (error nil))
                        (goto-char (point-min))
                        (let ((end (search-forward ":END:")))
                          (delete-region (point-min) end))
                        (jethro/promote-everything)
                        (insert "#+TITLE: " title)
                        (insert "\n#+DATE: " date)
                        (insert "\n#+SLUG: " slug)
                        (insert "\n#+TAGS: " tags)
                        (write-file filename)))))

(defun jethro/get-post-title (title)
  "Get post title from TITLE"
  (replace-regexp-in-string " " "-" (replace-regexp-in-string "[^a-zA-Z0-9 ]" ""
                                                              (downcase title))))
(defun jethro/insert-blog-props ()
  (interactive)
  (let* ((title (cdr (assoc "ITEM" (org-entry-properties))))
         (slug (jethro/get-post-title title))
         (date (now-is))
         (str (format ":PROPERTIES:
:SLUG:     %s
:DATE:     %s
:END:" slug date)))
    (insert str)))

(use-package vc
  :bind (:map jethro-mode-map
              ("C-x v =" . jethro/vc-diff)
              ("C-x v H" . vc-region-history)) ; New command in emacs 25.x
  :config
  (progn
    (defun jethro/vc-diff (no-whitespace)
      "Call `vc-diff' as usual if buffer is not modified.
If the buffer is modified (yet to be saved), call `diff-buffer-with-file'.
If NO-WHITESPACE is non-nil, ignore all white space when doing diff."
      (interactive "P")
      (let* ((no-ws-switch '("-w"))
             (vc-git-diff-switches (if no-whitespace
                                       no-ws-switch
                                     vc-git-diff-switches))
             (vc-diff-switches (if no-whitespace
                                   no-ws-switch
                                 vc-diff-switches))
             (diff-switches (if no-whitespace
                                no-ws-switch
                              diff-switches))
             ;; Set `current-prefix-arg' to nil so that the HISTORIC arg
             ;; of `vc-diff' stays nil.
             current-prefix-arg)
        (if (buffer-modified-p)
            (diff-buffer-with-file (current-buffer))
          (call-interactively #'vc-diff))))))

(use-package smerge-mode
  :bind (:map jethro-mode-map
              ("C-c h s" . jethro/hydra-smerge/body))
  :init
  (progn
    (defun jethro/enable-smerge-maybe ()
      "Auto-enable `smerge-mode' when merge conflict is detected."
      (save-excursion
        (goto-char (point-min))
        (when (re-search-forward "^<<<<<<< " nil :noerror)
          (smerge-mode 1))))
    (add-hook 'find-file-hook #'jethro/enable-smerge-maybe :append))
  :config 
  (defalias 'smerge-keep-upper 'smerge-keep-mine)
  (defalias 'smerge-keep-lower 'smerge-keep-other)
  (defalias 'smerge-diff-base-upper 'smerge-diff-base-mine)
  (defalias 'smerge-diff-upper-lower 'smerge-diff-mine-other)
  (defalias 'smerge-diff-base-lower 'smerge-diff-base-other)

  (defhydra jethro/hydra-smerge (:color pink
                                        :hint nil
                                        :pre (smerge-mode 1)
                                        ;; Disable `smerge-mode' when quitting hydra if
                                        ;; no merge conflicts remain.
                                        :post (smerge-auto-leave))
    "
   ^Move^       ^Keep^               ^Diff^                 ^Other^
   ^^-----------^^-------------------^^---------------------^^-------
   _n_ext       _b_ase               _<_: upper/base        _C_ombine
   _p_rev       _u_pper              _=_: upper/lower       _r_esolve
   ^^           _l_ower              _>_: base/lower        _k_ill current
   ^^           _a_ll                _R_efine
   ^^           _RET_: current       _E_diff
   "
    ("n" smerge-next)
    ("p" smerge-prev)
    ("b" smerge-keep-base)
    ("u" smerge-keep-upper)
    ("l" smerge-keep-lower)
    ("a" smerge-keep-all)
    ("RET" smerge-keep-current)
    ("\C-m" smerge-keep-current)
    ("<" smerge-diff-base-upper)
    ("=" smerge-diff-upper-lower)
    (">" smerge-diff-base-lower)
    ("R" smerge-refine)
    ("E" smerge-ediff)
    ("C" smerge-combine-with-next)
    ("r" smerge-resolve)
    ("k" smerge-kill-current)
    ("q" nil "cancel" :color blue)))

(use-package magit
  :bind (:map jethro-mode-map
              ("s-g" . magit-status)
              ("C-c g" . magit-status)
              ("s-G" . magit-blame)
              ("C-c G" . magit-blame))
  :init
  (add-hook 'magit-mode-hook 'hl-line-mode)
  :config
  (setq magit-auto-revert-mode nil))

(use-package projectile
  :demand t
  :init
  (setq projectile-keymap-prefix (kbd "C-x p"))
  (add-hook 'after-init-hook 'projectile-mode)
  :config
  (require 'projectile)
  (use-package counsel-projectile
    :bind (:map jethro-mode-map
                ("s-f" . counsel-projectile-find-file)
                ("s-b" . counsel-projectile-switch-to-buffer)
                ("C-c s" . jethro/counsel-projectile-rg))
    :config
    (defun jethro/counsel-projectile-rg (&optional options)
      "Ivy version of `projectile-rg'."
      (interactive)
      (if (projectile-project-p)
          (let* ((options
                  (if current-prefix-arg
                      (read-string "options: ")
                    options))
                 (ignored
                  (unless (eq (projectile-project-vcs) 'git)
                    ;; rg supports git ignore files
                    (append
                     (cl-union (projectile-ignored-files-rel) grep-find-ignored-files)
                     (cl-union (projectile-ignored-directories-rel) grep-find-ignored-directories))))
                 (options
                  (concat options " "
                          (mapconcat (lambda (i)
                                       (concat "--ignore-file " (shell-quote-argument i)))
                                     ignored
                                     " "))))
            (counsel-rg (ivy-thing-at-point)
                        (projectile-project-root)
                        options
                        (projectile-prepend-project-name "rg")))
        (user-error "You're not in a project")))
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
  :bind (:map jethro-mode-map
              ("C-c M d" . darkroom-mode)
              ("C-c M t" . darkroom-tentative-mode)))

(use-package bury-successful-compilation
  :init
  (add-hook 'after-init-hook 'bury-successful-compilation))

(load custom-file)
