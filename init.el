;;;; -*- lexical-binding: t -*-

(setq straight-repository-branch "develop")

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq straight-use-package-by-default t)
(straight-use-package 'use-package)
(straight-use-package 'diminish)

(setq user-full-name "Jethro Kuan"
      user-mail-address "jethrokuan95@gmail.com")

(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))

(setq ad-redefinition-action 'accept)

(use-package no-littering
  :config
  (require 'recentf)
  (add-to-list 'recentf-exclude no-littering-var-directory)
  (add-to-list 'recentf-exclude no-littering-etc-directory)
  :config
  (setq auto-save-file-name-transforms
        `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))))

(setq gc-cons-threshold 50000000)
(setq large-file-warning-threshold 100000000)

(use-package autorevert
  :straight nil
  :diminish t
  :hook
  (dired-mode . auto-revert-mode)
  :config
  (global-auto-revert-mode +1)
  :custom
  (auto-revert-verbose nil))

(setq custom-file "~/.emacs.d/custom.el")

(defalias 'yes-or-no-p 'y-or-n-p)

(delete-selection-mode 1)

(setq sentence-end-double-space nil)

(setq-default tab-width 2)
(setq-default js-indent-level 2)
(setq-default indent-tabs-mode nil)

(setq-default truncate-lines t)

(defun jethro/truncate-lines-hook ()
  (setq truncate-lines nil))

(add-hook 'text-mode-hook 'jethro/truncate-lines-hook)

(setq create-lockfiles nil)

(use-package goto-addr
  :hook ((compilation-mode . goto-address-mode)
         (prog-mode . goto-address-prog-mode)
         (eshell-mode . goto-address-mode)
         (shell-mode . goto-address-mode))
  :bind (:map goto-address-highlight-keymap
              ("<RET>" . goto-address-at-point)
              ("M-<RET>" . newline))
  :commands (goto-address-prog-mode
             goto-address-mode))

(bind-key "C-z" 'bury-buffer)

(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

(use-package notmuch
  :preface (setq-default notmuch-command (executable-find "notmuch"))
  :if (executable-find "notmuch")
  :bind (("<f2>" . notmuch)
         :map notmuch-search-mode-map
         ("t" . jethro/notmuch-toggle-read)
         ("r" . notmuch-search-reply-to-thread)
         ("R" . notmuch-search-reply-to-thread-sender)
         :map notmuch-show-mode-map
         ("l" . jethro/notmuch-show-jump-to-latest)
         ("<tab>" . org-next-link)
         ("<backtab>". org-previous-link)
         ("C-<return>" . browse-url-at-point))
  :config
  (defun jethro/notmuch-toggle-read ()
    "toggle read status of message"
    (interactive)
    (if (member "unread" (notmuch-search-get-tags))
        (notmuch-search-tag (list "-unread"))
      (notmuch-search-tag (list "+unread"))))
  (defun jethro/notmuch-show-jump-to-latest ()
    "Jump to the message in the current thread with the latest
timestamp."
    (interactive)
    (let ((timestamp 0)
          latest)
      (notmuch-show-mapc
       (lambda () (let ((ts (notmuch-show-get-prop :timestamp)))
                    (when (> ts timestamp)
                      (setq timestamp ts
                            latest (point))))))
      (if latest
          (goto-char latest)
        (error "Cannot find latest message."))))
  :custom
  (message-auto-save-directory "~/.mail/drafts/")
  (message-send-mail-function 'message-send-mail-with-sendmail)
  (sendmail-program (executable-find "msmtp"))

  ;; We need this to ensure msmtp picks up the correct email account
  (message-sendmail-envelope-from 'header)
  (mail-envelope-from 'header)
  (mail-specify-envelope-from t)
  (message-sendmail-f-is-evil nil)
  (message-kill-buffer-on-exit t)
  (notmuch-always-prompt-for-sender t)
  (notmuch-archive-tags '("-inbox" "-unread"))
  (notmuch-crypto-process-mime t)
  (notmuch-hello-sections '(notmuch-hello-insert-saved-searches))
  (notmuch-labeler-hide-known-labels t)
  (notmuch-search-oldest-first nil)
  (notmuch-archive-tags '("-inbox" "-unread"))
  (notmuch-message-headers '("To" "Cc" "Subject" "Bcc"))
  (notmuch-saved-searches '((:name "unread" :query "tag:unread")
                            (:name "to-me" :query "tag:to-me")
                            (:name "sent" :query "tag:sent")
                            (:name "personal" :query "tag:personal")
                            (:name "nushackers" :query "tag:nushackers")
                            (:name "nus" :query "tag:nus")
                            (:name "drafts" :query "tag:draft"))))

(use-package org-notmuch
  :straight nil
  :after org notmuch
  :bind
  (:map notmuch-show-mode-map
        ("C" . jethro/org-capture-email))
  :config
  (defun jethro/org-capture-email ()
    (interactive)
    (org-capture nil "e")))

(setq default-frame-alist '((font . "Iosevka-14")))

(when (fboundp 'tooltip-mode)
  (tooltip-mode -1))
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'menu-bar-mode)
  (menu-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)

(use-package tao-theme
  :init
  (load-theme 'tao-yang t))

(use-package rainbow-delimiters
  :hook
  (prog-mode . rainbow-delimiters-mode)
  :config
  (rainbow-delimiters-mode +1)
  (set-face-attribute 'rainbow-delimiters-unmatched-face nil
                      :foreground 'unspecified
                      :inherit 'error))


;; From https://with-emacs.com/posts/editing/show-matching-lines-when-parentheses-go-off-screen/
;; we will call `blink-matching-open` ourselves...
(remove-hook 'post-self-insert-hook
             #'blink-paren-post-self-insert-function)
;; this still needs to be set for `blink-matching-open` to work
(setq blink-matching-paren 'show)

(let ((ov nil)) ; keep track of the overlay
  (advice-add
   #'show-paren-function
   :after
   (defun show-paren--off-screen+ (&rest _args)
     "Display matching line for off-screen paren."
     (when (overlayp ov)
       (delete-overlay ov))
     ;; check if it's appropriate to show match info,
     ;; see `blink-paren-post-self-insert-function'
     (when (and (overlay-buffer show-paren--overlay)
                (not (or cursor-in-echo-area
                         executing-kbd-macro
                         noninteractive
                         (minibufferp)
                         this-command))
                (and (not (bobp))
                     (memq (char-syntax (char-before)) '(?\) ?\$)))
                (= 1 (logand 1 (- (point)
                                  (save-excursion
                                    (forward-char -1)
                                    (skip-syntax-backward "/\\")
                                    (point))))))
       ;; rebind `minibuffer-message' called by
       ;; `blink-matching-open' to handle the overlay display
       (cl-letf (((symbol-function #'minibuffer-message)
                  (lambda (msg &rest args)
                    (let ((msg (apply #'format-message msg args)))
                      (setq ov (display-line-overlay+
                                (window-start) msg ))))))
         (blink-matching-open))))))

(defun display-line-overlay+ (pos str &optional face)
  "Display line at POS as STR with FACE.

FACE defaults to inheriting from default and highlight."
  (let ((ol (save-excursion
              (goto-char pos)
              (make-overlay (line-beginning-position)
                            (line-end-position)))))
    (overlay-put ol 'display str)
    (overlay-put ol 'face
                 (or face '(:inherit default :inherit highlight)))
    ol))

(setq show-paren-style 'paren
      show-paren-delay 0.03
      show-paren-highlight-openparen t
      show-paren-when-point-inside-paren nil
      show-paren-when-point-in-periphery t)
(show-paren-mode 1)

(blink-cursor-mode 0)

(use-package hl-todo
  :config
  (global-hl-todo-mode))

(use-package elfeed
  :bind
  (("<f6>" . elfeed))
  :custom
  (shr-width 80))

(use-package elfeed-org
  :after elfeed
  :bind
  (:map elfeed-show-mode-map
        ("C" . jethro/org-capture-elfeed-link))
  (:map elfeed-search-mode-map
        ("C" . jethro/org-capture-elfeed-link))
  :config
  (require 'elfeed-link)
  (elfeed-org)
  (defun jethro/org-capture-elfeed-link ()
    (interactive)
    (org-capture nil "z"))
  :custom
  (rmh-elfeed-org-files '("~/.org/deft/feeds.org")))

(use-package counsel
  :hook
  (after-init . ivy-mode)
  :diminish ivy-mode
  :bind
  (("C-c C-r" . ivy-resume)
   ("M-x" . counsel-M-x)
   ("C-c i" . counsel-imenu)
   ("C-x b" . ivy-switch-buffer)
   ("C-x B" . ivy-switch-buffer-other-window)
   ("C-x k" . kill-buffer)
   ("C-x C-f" . counsel-find-file)
   ("C-x l" . counsel-locate)
   ("C-c j" . counsel-git)
   ("M-y" . counsel-yank-pop)
   :map help-map
   ("f" . counsel-describe-function)
   ("v" . counsel-describe-variable)
   ("l" . counsel-info-lookup-symbol)
   :map ivy-minibuffer-map
   ("C-o" . ivy-occur)
   ("<return>" . ivy-alt-done)
   ("M-<return>" . ivy-immediate-done)
   :map read-expression-map
   ("C-r" . counsel-minibuffer-history))
  :custom
  (counsel-find-file-at-point t)
  (ivy-use-virtual-buffers t)
  (ivy-display-style 'fancy)
  (ivy-use-selectable-prompt t)
  (ivy-re-builders-alist
   '((t . ivy--regex-plus)))
  :config
  (ivy-set-actions
   t
   '(("I" insert "insert")))
  (ivy-set-occur 'ivy-switch-buffer 'ivy-switch-buffer-occur))

(use-package counsel-fd
  :straight (:host github :repo "jethrokuan/counsel-fd")
  :after counsel
  :bind
  (("C-x j" . counsel-fd-dired-jump)
   ("C-x f" . counsel-fd-file-jump)))

(use-package amx
  :after ivy)

(use-package swiper
  :bind
  (("C-s" . swiper-isearch)
   ("C-r" . swiper-isearch)
   ("C-c C-s" . counsel-grep-or-swiper)
   :map swiper-map
   ("M-q" . swiper-query-replace)
   ("C-l". swiper-recenter-top-bottom)
   ("C-'" . swiper-avy))
  :custom
  (counsel-grep-swiper-limit 20000)
  (counsel-rg-base-command
   "rg -i -M 120 --no-heading --line-number --color never %s .")
  (counsel-grep-base-command
   "rg -i -M 120 --no-heading --line-number --color never '%s' %s"))

(use-package wgrep
  :commands
  wgrep-change-to-wgrep-mode
  ivy-wgrep-change-to-wgrep-mode)

(use-package deadgrep
  :if (executable-find "rg")
  :bind* (("M-s" . deadgrep)))

(use-package hydra
  :config
  (defhydra jethro/hydra-zoom ()
    "zoom"
    ("i" text-scale-increase "in")
    ("o" text-scale-decrease "out"))

  (bind-key "C-c h z" 'jethro/hydra-zoom/body))

(use-package whitespace
  :straight nil
  :diminish whitespace-mode
  :hook (prog-mode . whitespace-mode)
  :custom
  (whitespace-line-column 80)
  (whitespace-style '(face lines-tail)))

(use-package moody
  :config
  (setq x-underline-at-descent-line t)
  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode)
  :custom-face
  (mode-line ((t
               (:height 1.0 :box nil :foreground "#292617" :background "#ECE9E0")))))

(use-package minions
  :config
  (minions-mode +1))

(use-package beacon
  :diminish beacon-mode
  :custom
  (beacon-push-mark 10)
  :config
  (beacon-mode +1))

(show-paren-mode 1)
(setq show-paren-delay 0)

(use-package volatile-highlights
  :diminish volatile-highlights-mode
  :config
  (volatile-highlights-mode +1))

(use-package diff-hl
  :hook
  (dired-mode . diff-hl-dired-mode)
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
    (add-hook hook #'diff-hl-flydiff-mode)))

(use-package diff-hl-hydra
  :straight nil
  :after hydra
  :no-require t
  :config
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

  (bind-key "C-c h v" #'jethro/hydra-diff-hl/body))

(use-package eyebrowse
  :bind* (("M-0" . eyebrowse-switch-to-window-config-0)
          ("M-1" . eyebrowse-switch-to-window-config-1)
          ("M-2" . eyebrowse-switch-to-window-config-2)
          ("M-3" . eyebrowse-switch-to-window-config-3)
          ("M-4" . eyebrowse-switch-to-window-config-4)
          ("M-5" . eyebrowse-switch-to-window-config-5)
          ("M-6" . eyebrowse-switch-to-window-config-6)
          ("M-7" . eyebrowse-switch-to-window-config-7)
          ("M-8" . eyebrowse-switch-to-window-config-8)
          ("M-9" . eyebrowse-switch-to-window-config-9))
  :config
  (eyebrowse-mode +1))

(use-package crux
  :bind (("C-c o" . crux-open-with)
         ("C-c D" . crux-delete-file-and-buffer)
         ("C-a" . crux-move-beginning-of-line)
         ("M-o" . crux-smart-open-line)
         ("C-c r" . crux-rename-file-and-buffer)
         ("M-D" . crux-duplicate-and-comment-current-line-or-region)
         ("s-o" . crux-smart-open-line-above)))

(use-package avy
  :bind*
  (("C-'" . avy-goto-char-timer))
  :custom
  (avy-keys '(?h ?t ?n ?s ?m ?w ?v ?z)))

(use-package smart-jump
  :config
  (smart-jump-setup-default-registers))

(let ((gls "/usr/local/bin/gls"))
  (if (file-exists-p gls)
      (setq insert-directory-program gls)))

(setq dired-listing-switches "-aBhl  --group-directories-first")

(setq dired-dwim-target t)

(setq dired-recursive-copies (quote always))
(setq dired-recursive-deletes (quote top))

(use-package wdired
  :commands wdired-mode wdired-change-to-wdired-mode
  :custom
  (wdired-allow-to-change-permissions t))

(use-package dired-narrow
  :bind (:map dired-mode-map
              ("N" . dired-narrow-fuzzy)))

(use-package dired-git-info
  :bind (:map dired-mode-map
              (")" . dired-git-info-mode)))

(use-package ibuffer
  :bind (([remap list-buffers] . ibuffer))
  :custom
  (ibuffer-expert t))

(use-package shackle
  :diminish shackle-mode
  :if (not (bound-and-true-p disable-pkg-shackle))
  :custom
  (shackle-rules
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
     (magit-log-mode :select t :inhibit-window-quit t :same t)))
  :config
  (shackle-mode +1))

(use-package easy-kill
  :bind*
  (([remap kill-ring-save] . easy-kill)))

(use-package aggressive-indent
  :diminish aggressive-indent-mode
  :config
  (global-aggressive-indent-mode +1)
  :custom
  (aggressive-indent-excluded-modes
   '(bibtex-mode
     cider-repl-mode
     c-mode
     c++-mode
     coffee-mode
     comint-mode
     conf-mode
     Custom-mode
     diff-mode
     doc-view-mode
     dos-mode
     erc-mode
     jabber-chat-mode
     haml-mode
     intero-mode
     haskell-mode
     interative-haskell-mode
     haskell-interactive-mode
     image-mode
     makefile-mode
     makefile-gmake-mode
     minibuffer-inactive-mode
     nix-mode
     netcmd-mode
     python-mode
     sass-mode
     slim-mode
     special-mode
     shell-mode
     snippet-mode
     eshell-mode
     tabulated-list-mode
     term-mode
     TeX-output-mode
     text-mode
     yaml-mode
     scala-mode)))

(use-package multiple-cursors
  :bind (("C-M-c" . mc/edit-lines)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)))

(use-package expand-region
  :bind (("C-=" . er/expand-region)))

(use-package smartparens
  :hook
  (after-init . smartparens-global-strict-mode)
  :bind (:map smartparens-mode-map
              ("C-M-f" . sp-forward-sexp)
              ("C-M-b" . sp-backward-sexp)
              ("C-M-u" . sp-backward-up-sexp)
              ("C-M-d" . sp-down-sexp)
              ("C-M-p" . sp-backward-down-sexp)
              ("C-M-n" . sp-up-sexp)
              ("C-M-s" . sp-splice-sexp)
              ("C-M-<up>" . sp-splice-sexp-killing-backward)
              ("C-M-<down>" . sp-splice-sexp-killing-forward)
              ("C-M-r" . sp-splice-sexp-killing-around)
              ("C-)" . sp-forward-slurp-sexp)
              ("C-<right>" . sp-forward-slurp-sexp)
              ("C-}" . sp-forward-barf-sexp)
              ("C-<left>" . sp-forward-barf-sexp)
              ("C-(" . sp-backward-slurp-sexp)
              ("C-M-<left>" . sp-backward-slurp-sexp)
              ("C-{" . sp-backward-barf-sexp)
              ("C-M-<right>" . sp-backward-barf-sexp)
              ("M-S" . sp-split-sexp))
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

(bind-key "M-z" 'zap-up-to-char)

(use-package ws-butler
  :diminish 'ws-butler-mode
  :hook
  (prog-mode . ws-butler-mode))

(use-package flycheck
  :config
  (global-flycheck-mode +1)

  (setq-default flycheck-check-syntax-automatically '(save
                                                      idle-change
                                                      mode-enabled))

  ;; Temporary workaround: Direnv needs to load PATH before flycheck looks
  ;; for linters
  (setq flycheck-executable-find
        (lambda (cmd)
          (direnv-update-environment default-directory)
          (executable-find cmd))))

(use-package flycheck-hydra
  :straight nil
  :no-require t
  :after flycheck hydra
  :config
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

  (bind-key "C-c h f" #'jethro/hydra-flycheck/body))

(use-package flycheck-pos-tip
  :after flycheck
  :hook
  (flycheck-mode . flycheck-pos-tip-mode))

(flycheck-add-mode 'proselint 'org-mode)

(use-package yasnippet
  :diminish yas-global-mode yas-minor-mode
  :config
  (yas-global-mode +1)
  :custom
  (yas-snippet-dirs '("~/.emacs.d/snippets/snippets/")))

(use-package company
  :diminish company-mode
  :bind (:map company-active-map
              ("M-n" . nil)
              ("M-p" . nil)
              ("C-n" . company-select-next)
              ("C-p" . company-select-previous))
  :custom
  (company-dabbrev-ignore-case nil)
  (company-dabbrev-code-ignore-case nil)
  (company-dabbrev-downcase nil)
  (company-idle-delay 0.5)
  (company-minimum-prefix-length 2)
  (company-require-match nil)
  (company-begin-commands '(self-insert-command))
  (company-transformers '(company-sort-by-occurrence))
  :config
  (defun company-mode/backend-with-yas (backend)
    (if (and (listp backend) (member 'company-yasnippet backend))
        backend
      (append (if (consp backend) backend (list backend))
              '(:with company-yasnippet))))

  (setq company-backends (mapcar #'company-mode/backend-with-yas company-backends))
  (global-company-mode +1))

(use-package company-quickhelp
  :after company
  :bind (:map company-active-map
              ("M-h" . company-quickhelp-manual-begin))
  :hook
  (company-mode . company-quickhelp-mode))

(use-package flyspell
  :straight nil
  :diminish flyspell-mode
  :init
  (setenv "DICTIONARY" "en_GB")
  :hook
  (text-mode . flyspell-mode)
  :custom
  (flyspell-abbrev-p t))

(use-package flyspell-correct
  :bind
  (:map flyspell-mode-map
        (("C-;" . flyspell-correct-wrapper))))

(use-package flyspell-correct-ivy
  :after flyspell-correct)

(add-hook 'text-mode-hook 'auto-fill-mode)
(add-hook 'message-mode-hook (lambda ()
                               (auto-fill-mode -1)))
(diminish 'auto-fill-mode)

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

(use-package dtrt-indent
  :diminish t
  :config
  (dtrt-indent-mode +1))

(use-package direnv
  :if (executable-find "direnv")
  :custom
  (direnv-always-show-summary nil)
  :config
  (direnv-mode +1))

(use-package lsp-mode
  :commands lsp
  :hook
  (lsp-after-open-hook . lsp-enable-imenu)
  :custom
  (lsp-message-project-root-warning t)
  :init
  (require 'lsp-clients)
  (add-hook 'python-mode-hook #'lsp)
  (add-hook 'c++-mode-hook #'lsp)
  (add-hook 'c-mode-hook #'lsp))

(use-package lsp-ui
  :after lsp-mode
  :commands lsp-ui-mode
  :hook
  (lsp-mode . lsp-ui-mode)
  :config
  (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references))

(use-package company-lsp
  :after company lsp-mode
  :config
  (add-to-list 'company-backends 'company-lsp))

(bind-key "C-c C-k" 'eval-buffer emacs-lisp-mode-map)

(use-package elixir-mode
  :mode "\\.ex[s]?\\'")

(use-package alchemist
  :after elixir-mode)

(use-package docker
  :commands docker-mode)

(use-package dockerfile-mode
  :mode "Dockerfile\\'")

(use-package nix-mode
  :mode ("\\.nix\\'" "\\.nix.in\\'")
  :custom
  (nix-indent-function #'nix-indent-line))

(use-package nix-update
  :after nix-mode
  :bind
  (:map nix-mode-map
        ("C-. u" . nix-update)))

(use-package haskell-mode
  :mode ("\\.hs\\'" . haskell-mode)
  :init
  (add-hook 'haskell-mode-hook
            (lambda ()
              (setq compile-command "stack build --fast --test --bench --no-run-tests --no-run-benchmarks"))))

(use-package intero
  :after haskell-mode
  :hook
  (haskell-mode . intero-mode))

(use-package cc-mode
  :ensure nil
  :mode
  ("\\.c\\'" . c-mode)
  ("\\.cpp\\'" . c++-mode)
  ("\\.h\\'" . c++-mode)
  ("\\.hpp\\'" . c++-mode))

(add-hook 'c++-mode-hook
          (lambda ()
            (unless (file-exists-p "Makefile")
              (set (make-local-variable 'compile-command)
                   (let ((file (file-name-nondirectory buffer-file-name)))
                     (format "g++ -Wall -s -pedantic-errors %s -o %s --std=c++14"
                             file
                             (file-name-sans-extension file)))))))

(use-package ccls
  :after lsp-mode
  :custom
  (ccls-executable "ccls"))

(use-package fish-mode
  :mode ("\\.fish\\'" . fish-mode))

(use-package rust-mode
  :mode ("\\.rs\\'" . rust-mode))

(eval-after-load "python-mode"
  (lambda ()
    (setq python-remove-cwd-from-path t)))

(use-package py-isort
  :commands
  (py-isort-buffer py-isort-region))

(use-package blacken
  :hook
  (python-mode . blacken-mode))

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
  :hook
  (python-mode . highlight-indent-guides-mode)
  :custom
  (highlight-indent-guides-method 'character))

(use-package isend-mode
  :bind
  (:map isend-mode-map
        ("C-M-e" . isend-send-defun))
  :hook
  (isend-mode. isend-default-python-setup))

(use-package web-mode
  :mode (("\\.html\\'" . web-mode)
         ("\\.html\\.erb\\'" . web-mode)
         ("\\.mustache\\'" . web-mode)
         ("\\.jinja\\'" . web-mode)
         ("\\.njk\\'" . web-mode)
         ("\\.php\\'" . web-mode)
         ("\\.js[x]?\\'" . web-mode))
  :custom
  (web-mode-enable-css-colorization t)
  (web-mode-content-types-alist
   '(("jsx" . "\\.js[x]?\\'")))
  :config
  (setq-default css-indent-offset 2
                web-mode-markup-indent-offset 2
                web-mode-css-indent-offset 2
                web-mode-code-indent-offset 2
                web-mode-attr-indent-offset 2))

(use-package emmet-mode
  :diminish emmet-mode
  :hook
  (web-mode . emmet-mode)
  (vue-mode . emmet-mode))

(use-package rainbow-mode
  :diminish rainbow-mode
  :hook
  (css-mode . rainbow-mode)
  (scss-mode . rainbow-mode))

(use-package scss-mode
  :mode "\\.scss\\'"
  :custom
  (scss-compile-at-save nil))

(use-package js2-mode
  :hook
  (web-mode-hook . js2-minor-mode)
  :config
  (setq-default flycheck-disabled-checkers
                (append flycheck-disabled-checkers
                        '(javascript-jshint)))
  :custom
  (js-switch-indent-offset 2))

(use-package indium
  :after js2-mode
  :bind (:map js2-mode-map
              ("C-c C-l" . indium-eval-buffer))
  :hook
  ((js2-mode . indium-interaction-mode)))

(use-package js-doc
  :after js2-mode
  :bind (:map js2-mode-map
              ("C-c i" . js-doc-insert-function-doc)
              ("@" . js-doc-insert-tag))
  :custom
  (js-doc-mail-address "jethrokuan95@gmail.com")
  (js-doc-author (format "Jethro Kuan <%s>" js-doc-mail-address))
  (js-doc-url "http://www.jethrokuan.com/")
  (js-doc-license "MIT"))

(use-package prettier-js
  :hook
  (js2-minor-mode . prettier-js-mode))

(use-package lsp-java
  :after lsp-mode
  :hook
  (java-mode . lsp))

(use-package typescript-mode
  :mode "\\.ts\\'")

(use-package tide
  :after typescript-mode
  :hook
  (before-save . tide-format-before-save)
  (typescript-mode . (lambda ()
                       (tide-setup)
                       (flycheck-mode +1)
                       (tide-hl-identifier-mode +1)
                       (company-mode +1))))

(use-package json-mode
  :mode "\\.json\\'"
  :hook
  (json-mode . (lambda ()
                 (make-local-variable 'js-indent-level)
                 (setq js-indent-level 2))))

(use-package markdown-mode
  :mode ("\\.md\\'" . markdown-mode)
  :commands (markdown-mode gfm-mode)
  :custom
  (markdown-fontify-code-blocks-natively t)
  (markdown-command "multimarkdown --snippet --smart --notes")
  (markdown-enable-wiki-links t)
  (markdown-indent-on-enter 'indent-and-new-item)
  (markdown-asymmetric-header t)
  (markdown-live-preview-delete-export 'delete-on-destroy))

(use-package adoc-mode
  :mode ("\\.adoc\\'" . adoc-mode))

(use-package auctex
  :mode ("\\.tex\\'" . latex-mode)
  :custom
  (TeX-auto-save t)
  (TeX-parse-self t)
  (TeX-syntactic-comment t)
  ;; Synctex Support
  (TeX-source-correlate-start-server nil)
  ;; Don't insert line-break at inline math
  (LaTeX-fill-break-at-separators nil)
  (TeX-view-program-list '(("zathura" "zathura --page=%(outpage) %o")))
  (TeX-view-program-selection '((output-pdf "zathura")))
  :config
  (setq-default TeX-engine 'luatex)
  (add-hook 'LaTeX-mode-hook
            (lambda ()
              (company-mode)
              (setq TeX-PDF-mode t)
              (setq TeX-source-correlate-method 'synctex)
              (setq TeX-source-correlate-start-server t)))
  (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
  (add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode)
  (add-hook 'LaTeX-mode-hook 'TeX-PDF-mode))

(use-package company-auctex
  :after auctex company-mode)

(use-package yaml-mode
  :mode ("\\.yaml\\'" . yaml-mode))

(use-package ensime
  :commands ensime ensime-mode)

(use-package ess)

(straight-use-package 'org-plus-contrib)

(require 'org)

(use-package org
  :mode ("\\.org\\'" . org-mode)
  :bind
  (("C-c l" . org-store-link)
   ("C-c a" . org-agenda)
   ("C-c b" . org-iswitchb)
   ("C-c c" . org-capture))
  :bind
  (:map org-mode-map
        ("M-n" . outline-next-visible-heading)
        ("M-p" . outline-previous-visible-heading))
  :custom
  (org-src-window-setup 'current-window)
  (org-return-follows-link t)
  (org-agenda-diary-file "~/.org/diary.org")
  (org-babel-load-languages
   '((emacs-lisp . t)
     (python . t)
     (dot . t)))
  (org-confirm-babel-evaluate nil)
  (org-use-speed-commands t)
  (org-catch-invisible-edits 'show)
  :custom-face
  (variable-pitch ((t (:family "iA Writer Duospace" :height 0.9))))
  (org-document-title ((t (:weight bold :height 1.5))))
  (org-done ((t (:strike-through t :weight bold))))
  (org-headline-done ((t (:strike-through t))))
  (org-level-1 ((t (:height 1.3 :weight bold))))
  (org-level-2 ((t (:height 1.2 :weight bold))))
  (org-level-3 ((t (:height 1.1 :weight bold))))
  (org-image-actual-width (/ (display-pixel-width) 2))
  :custom
  (org-structure-template-alist '(("a" . "export ascii")
                                  ("c" . "center")
                                  ("C" . "comment")
                                  ("e" . "example")
                                  ("E" . "export")
                                  ("h" . "export html")
                                  ("l" . "export latex")
                                  ("q" . "quote")
                                  ("s" . "src")
                                  ("v" . "verse")
                                  ("el" . "src emacs-lisp")
                                  ("d" . "definition")
                                  ("t" . "theorem")))
  :config
  (require 'org-habit)
  (require 'org-tempo))

(add-hook 'org-mode-hook
          '(lambda ()
             (setq line-spacing 0.2) ;; Add more line padding for readability
             (variable-pitch-mode 1) ;; All fonts with variable pitch.
             (mapc
              (lambda (face) ;; Other fonts with fixed-pitch.
                (set-face-attribute face nil :inherit 'fixed-pitch))
              (list 'org-code
                    'org-link
                    'org-block
                    'org-table
                    'org-verbatim
                    'org-block-begin-line
                    'org-block-end-line
                    'org-meta-line
                    'org-document-info-keyword))))

(setq org-startup-indented t
      org-hide-emphasis-markers t
      org-pretty-entities nil)

(defun org-archive-done-tasks ()
  (interactive)
  (org-map-entries 'org-archive-subtree "/DONE" 'file))

(use-package password-store)

;; (use-package org-gcal
;;   :custom
;;   (require 'password-store)
;;   (require 'auth-source-pass)
;;   (org-gcal-client-id "1025518578318-g5llmkeftf20ct2s7j0b4pmu7tr6am1r.apps.googleusercontent.com")
;;   (org-gcal-client-secret `,(auth-source-pass-get 'secret "gmail/org-gcal"))
;;   (jethro/org-gcal-directory "~/.org/gtd/calendars/")
;;   :config
;;   (defun jethro/get-gcal-file-location (loc)
;;     (concat (file-name-as-directory jethro/org-gcal-directory) loc))
;;   (setq org-gcal-file-alist `(("jethrokuan95@gmail.com" . ,(jethro/get-gcal-file-location "personal.org"))
;;                               ("62ad47vpojb2uqb53hpnqsuv5o@group.calendar.google.com" . ,(jethro/get-gcal-file-location "school.org"))
;;                               ("15rmvcq9uehc0e4ccorj5hbm8o@group.calendar.google.com" . ,(jethro/get-gcal-file-location "6101.org")))))

;; (run-at-time (* 60 60) nil
;;              (lambda ()
;;                (let ((inhibit-message t))
;;                  (org-gcal-refresh-token)
;;                  (org-gcal-fetch))))

(require 'find-lisp)
(setq jethro/org-agenda-directory "~/.org/gtd/")
(setq org-agenda-files
      (find-lisp-find-files jethro/org-agenda-directory "\.org$"))

(setq org-capture-templates
      `(("i" "inbox" entry (file ,(concat jethro/org-agenda-directory "inbox.org"))
         "* TODO %?")
        ("e" "email" entry (file+headline ,(concat jethro/org-agenda-directory "emails.org") "Emails")
         "* TODO [#A] Reply: %a :@home:@school:" :immediate-finish t)
        ("l" "link" entry (file ,(concat jethro/org-agenda-directory "inbox.org"))
         "* TODO %(org-cliplink-capture)" :immediate-finish t)
        ("w" "Weekly Review" entry (file+olp+datetree ,(concat jethro/org-agenda-directory "reviews.org"))
         (file ,(concat jethro/org-agenda-directory "templates/weekly_review.org")))))

(setq jethro/org-agenda-reading-view
      `("r" "Reading" todo ""
        ((org-agenda-files '(,(concat jethro/org-agenda-directory "reading.org"))))))

(add-to-list 'org-agenda-custom-commands `,jethro/org-agenda-reading-view)

(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
        (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)")))

(setq org-log-done 'time)
(setq org-log-into-drawer t)
(setq org-log-state-notes-insert-after-drawers nil)

(setq org-tag-alist (quote (("@errand" . ?e)
                            ("@office" . ?o)
                            ("@home" . ?h)
                            ("@school" . ?s)
                            (:newline)
                            ("WAITING" . ?w)
                            ("HOLD" . ?H)
                            ("CANCELLED" . ?c))))

(setq org-fast-tag-selection-single-key nil)

;; https://github.com/syl20bnr/spacemacs/issues/3094
(setq org-refile-use-outline-path 'file
      org-outline-path-complete-in-steps nil)
(setq org-refile-allow-creating-parent-nodes 'confirm)
(setq org-refile-targets '(("next.org" :level . 0)
                           ("someday.org" :level . 0)
                           ("reading.org" :level . 1)
                           ("projects.org" :maxlevel . 1)))

(defvar jethro/org-agenda-bulk-process-key ?f
  "Default key for bulk processing inbox items.")

(defun jethro/org-process-inbox ()
  "Called in org-agenda-mode, processes all inbox items."
  (interactive)
  (org-agenda-bulk-mark-regexp "inbox:")
  (jethro/bulk-process-entries))

(defvar jethro/org-current-effort "1:00" "Current effort for agenda items.")

(defun jethro/my-org-agenda-set-effort (effort)
  "Set the effort property for the current headline."
  (interactive
   (list (read-string (format "Effort [%s]: " jethro/org-current-effort) nil nil jethro/org-current-effort)))
  (setq jethro/org-current-effort effort)
  (org-agenda-check-no-diary)
  (let* ((hdmarker (or (org-get-at-bol 'org-hd-marker)
                       (org-agenda-error)))
         (buffer (marker-buffer hdmarker))
         (pos (marker-position hdmarker))
         (inhibit-read-only t)
         newhead)
    (org-with-remote-undo buffer
      (with-current-buffer buffer
        (widen)
        (goto-char pos)
        (org-show-context 'agenda)
        (funcall-interactively 'org-set-effort nil jethro/org-current-effort)
        (end-of-line 1)
        (setq newhead (org-get-heading)))
      (org-agenda-change-all-lines newhead hdmarker))))

(defun jethro/org-agenda-process-inbox-item ()
  "Process a single item in the org-agenda."
  (org-with-wide-buffer
   (org-agenda-set-tags)
   (org-agenda-priority)
   (call-interactively 'jethro/my-org-agenda-set-effort)
   (org-agenda-refile nil nil t)))

(defun jethro/bulk-process-entries ()
  (if (not (null org-agenda-bulk-marked-entries))
      (let ((entries (reverse org-agenda-bulk-marked-entries))
            (processed 0)
            (skipped 0))
        (dolist (e entries)
          (let ((pos (text-property-any (point-min) (point-max) 'org-hd-marker e)))
            (if (not pos)
                (progn (message "Skipping removed entry at %s" e)
                       (cl-incf skipped))
              (goto-char pos)
              (let (org-loop-over-headlines-in-active-region) (funcall 'jethro/org-agenda-process-inbox-item))
              ;; `post-command-hook' is not run yet.  We make sure any
              ;; pending log note is processed.
              (when (or (memq 'org-add-log-note (default-value 'post-command-hook))
                        (memq 'org-add-log-note post-command-hook))
                (org-add-log-note))
              (cl-incf processed))))
        (org-agenda-redo)
        (unless org-agenda-persistent-marks (org-agenda-bulk-unmark-all))
        (message "Acted on %d entries%s%s"
                 processed
                 (if (= skipped 0)
                     ""
                   (format ", skipped %d (disappeared before their turn)"
                           skipped))
                 (if (not org-agenda-persistent-marks) "" " (kept marked)")))
    ))



(defun jethro/org-inbox-capture ()
  (interactive)
  "Capture a task in agenda mode."
  (org-capture nil "i"))

(setq org-agenda-bulk-custom-functions `((,jethro/org-agenda-bulk-process-key jethro/org-agenda-process-inbox-item)))

(define-key org-agenda-mode-map "i" 'org-agenda-clock-in)
(define-key org-agenda-mode-map "r" 'jethro/org-process-inbox)
(define-key org-agenda-mode-map "R" 'org-agenda-refile)
(define-key org-agenda-mode-map "c" 'jethro/org-inbox-capture)

(defun jethro/set-todo-state-next ()
  "Visit each parent task and change NEXT states to TODO"
  (org-todo "NEXT"))

(add-hook 'org-clock-in-hook 'jethro/set-todo-state-next 'append)

(use-package org-clock-convenience
  :bind (:map org-agenda-mode-map
              ("<S-up>" . org-clock-convenience-timestamp-up)
              ("<S-down>" . org-clock-convenience-timestamp-down)
              ("o" . org-clock-convenience-fill-gap)
              ("e" . org-clock-convenience-fill-gap-both)))

(setq org-agenda-block-separator nil)
(setq org-agenda-start-with-log-mode t)

(setq jethro/org-agenda-todo-view
      `(" " "Agenda"
        ((agenda ""
                 ((org-agenda-span 'day)
                  (org-deadline-warning-days 365)))
         (todo "TODO"
               ((org-agenda-overriding-header "To Refile")
                (org-agenda-files '(,(concat jethro/org-agenda-directory "inbox.org")))))
         (todo "TODO"
               ((org-agenda-overriding-header "Emails")
                (org-agenda-files '(,(concat jethro/org-agenda-directory "emails.org")))))
         (todo "NEXT"
               ((org-agenda-overriding-header "In Progress")
                (org-agenda-files '(,(concat jethro/org-agenda-directory "someday.org")
                                    ,(concat jethro/org-agenda-directory "projects.org")
                                    ,(concat jethro/org-agenda-directory "next.org")))
                ;; (org-agenda-skip-function '(org-agenda-skip-entry-if 'deadline 'scheduled))
                ))
         (todo "TODO"
               ((org-agenda-overriding-header "Projects")
                (org-agenda-files '(,(concat jethro/org-agenda-directory "projects.org")))
                ;; (org-agenda-skip-function #'jethro/org-agenda-skip-all-siblings-but-first)
                ))
         (todo "TODO"
               ((org-agenda-overriding-header "One-off Tasks")
                (org-agenda-files '(,(concat jethro/org-agenda-directory "next.org")))
                (org-agenda-skip-function '(org-agenda-skip-entry-if 'deadline 'scheduled))))
         nil)))

(add-to-list 'org-agenda-custom-commands `,jethro/org-agenda-todo-view)

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

(defun jethro/switch-to-agenda ()
  (interactive)
  (org-agenda nil " ")
  (delete-other-windows))

(bind-key "<f1>" 'jethro/switch-to-agenda)

(setq org-columns-default-format "%40ITEM(Task) %Effort(EE){:} %CLOCKSUM(Time Spent) %SCHEDULED(Scheduled) %DEADLINE(Deadline)")

(use-package org-pomodoro
  :after org
  :bind
  (:map org-agenda-mode-map
        (("I" . org-pomodoro)))
  :custom
  (org-pomodoro-format "%s"))

(use-package org-cliplink
  :bind
  ("C-c C" . 'jethro/org-capture-link)
  :config
  (defun jethro/org-capture-link ()
    "Captures a link, and stores it in inbox."
    (interactive)
    (org-capture nil "l")))

(use-package deft
  :after org
  :bind
  ("C-c n d" . deft)
  ("C-c n l" . jethro/get-linked-files)
  ("C-c n i" . org-insert-zettel)
  :custom
  (deft-use-filter-string-for-filename t)
  (deft-default-extension "org")
  (deft-directory "~/.org/braindump/org")
  (deft-use-filename-as-title t)
  :config
  (setq zettel-indicator "§")
  (defun jethro/deft-insert-boilerplate ()
    (interactive)
    (when (= (buffer-size (current-buffer)) 0)
      (let ((title (s-join " " (-map #'capitalize (split-string (file-name-sans-extension (buffer-name)) "_")))))
        (insert "#+SETUPFILE:./hugo_setup.org\n")
        (insert "#+TITLE: ")
        (insert title)
        (goto-char (point-max)))))
  (defun org-insert-zettel (file-name)
    "Finds a file, inserts it as a link with the base file name as the link name, and adds the zd-link-indicator I use to the front."
    (interactive (list (completing-read "File: " (deft-find-all-files-no-prefix))))
    (org-insert-link nil (concat "file:" (file-name-base file-name) "." (file-name-extension file-name)) (concat zettel-indicator (file-name-base file-name))))
  (defun jethro/get-linked-files ()
    "Show links to this file."
    (interactive)
    (let* ((search-term (file-name-nondirectory buffer-file-name))
           (files deft-all-files)
	         (tnames (mapcar #'file-truename files)))
      (multi-occur
       (mapcar (lambda (x)
	               (with-current-buffer
		                 (or (get-file-buffer x) (find-file-noselect x))
		               (widen)
		               (current-buffer)))
	             files)
       search-term
       3))))

(use-package org-download
  :after org
  :bind
  (:map org-mode-map
        (("s-Y" . org-download-screenshot)
         ("s-y" . org-download-yank)))
  :config
  (if (memq window-system '(mac ns))
      (setq org-download-screenshot-method "screencapture -i %s")
    (setq org-download-screenshot-method "gnome-screenshot -a -f %s"))
  (defun my-org-download-method (link)
    "This is a helper function for org-download.
It creates a folder in the root directory (~/.org/img/) named after the
org filename (sans extension) and puts all images from that file in there.
Inspired by https://github.com/daviderestivo/emacs-config/blob/6086a7013020e19c0bc532770e9533b4fc549438/init.el#L701"
    (let ((filename
           (file-name-nondirectory
            (car (url-path-and-query
                  (url-generic-parse-url link)))))
          ;; Create folder name with current buffer name, and place in root dir
          (dirname (concat "./images/"
                           (replace-regexp-in-string " " "_" (downcase (file-name-base buffer-file-name))))))

      ;; Add timestamp to filename
      (setq filename-with-timestamp (format "%s%s.%s"
                                            (file-name-sans-extension filename)
                                            (format-time-string org-download-timestamp)
                                            (file-name-extension filename)))
      ;; Create folder if necessary
      (unless (file-exists-p dirname)
        (make-directory dirname))
      (expand-file-name filename-with-timestamp dirname)))
  (setq org-download-method 'my-org-download-method))

(use-package org-journal
  :custom
  (org-journal-dir "~/.org/journal/"))

(use-package ox-hugo
  :after org
  :custom
  (org-hugo-auto-set-lastmod t))

(use-package ox-latex
  :after org
  :straight nil
  :config
  :custom
  (org-latex-prefer-user-labels t)
  (org-latex-pdf-process
   (list "latexmk -shell-escape -bibtex -f -pdf %f"
         "latexmk -shell-escape -bibtex -f -pdf %f"))
  (org-latex-default-table-environment "tabular")
  (org-latex-tables-booktabs t)
  (org-latex-listings 'minted)
  (org-latex-minted-options '(("linenos" "true") ("breaklines" "true")))
  (org-latex-hyperref-template "\\hypersetup{colorlinks=true,allcolors=.,urlcolor=blue}")
  (org-format-latex-options (plist-put org-format-latex-options :scale 2.0))
  (org-latex-classes
   '(("beamer"
      "\\documentclass[12pt]{beamer}

% \\includeonlyframes{current}

\\mode<beamer>{\\usetheme[sectionpage=none, subsectionpage=progressbar, progressbar=foot, numbering=fraction]{metropolis}}

\\setbeamertemplate{footline}{%
  \\begin{beamercolorbox}[wd=\\textwidth, sep=1.5ex]{footline}% <- default 3ex
    \\usebeamerfont{page number in head/foot}%
    \\usebeamertemplate*{frame footer}
    \\hfill%
    \\usebeamertemplate*{frame numbering}
  \\end{beamercolorbox}%
}
\\makeatother

\\makeatletter
\\setbeamertemplate{headline}{
  \\begin{beamercolorbox}{upper separation line head}
  \\end{beamercolorbox}
  \\begin{beamercolorbox}{section in head/foot}
    \\vskip2pt\\insertsectionnavigationhorizontal{\\paperwidth}{}{}\\vskip2pt
  \\end{beamercolorbox}
  \\begin{beamercolorbox}{lower separation line head}
  \\end{beamercolorbox}
}
\\makeatother
\\setbeamercolor{section in head/foot}{fg=normal text.bg, bg=structure.fg}

\\setbeamertemplate{itemize items}[square]
\\usepackage{minted}
\\setminted{fontsize=\\footnotesize}
"
      ("\\section{%s}" . "\\section*{%s}")
      ("\\subsection{%s}" . "\\subsection*{%s}")
      ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
      ("\\paragraph{%s}" . "\\paragraph*{%s}")
      ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
     ("article"
      "\\documentclass{article}
\\usepackage[margin=1in]{geometry}
\\usepackage{amsmath,amsthm,amssymb}
\\newtheorem{definition}{Definition}
\\newtheorem{theorem}{Theorem}

\\usepackage{booktabs}
\\usepackage{hyperref}
\\usepackage{minted}
\\usepackage{tabularx}
\\usepackage{parskip}
\\linespread{1.1}"
      ("\\section{%s}" . "\\section*{%s}")
      ("\\subsection{%s}" . "\\subsection*{%s}")
      ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
      ("\\paragraph{%s}" . "\\paragraph*{%s}")
      ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
     ("socreport"
      "\\documentclass[fyp]{socreport}
\\usepackage[square,numbers]{natbib}
\\usepackage{fullpage}
\\usepackage{xcolor}
\\usepackage{color}
\\usepackage{tabularx}
\\usepackage{booktabs}
  "
      ("\\chapter{%s}" . "\\chapter*{%s}")
      ("\\section{%s}" . "\\section*{%s}")
      ("\\subsection{%s}" . "\\subsection*{%s}")
      ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
      ("\\paragraph{%s}" . "\\paragraph*{%s}"))
     ("latex-notes"
      "\\documentclass[8pt]{article}
  \\usepackage[margin={0.1in,0.1in}, a4paper,landscape]{geometry}
  \\usepackage{hyperref}
  \\usepackage{amsmath}
  \\usepackage{multicol}
  \\usepackage{booktabs}
  \\usepackage{enumitem}
  \\usepackage[compact]{titlesec}
  \\renewcommand\\maketitle{}
  \\titlespacing{\\section}{0pt}{*2}{*0}
  \\titlespacing{\\subsection}{0pt}{*2}{*0}
  \\titlespacing{\\subsubsection}{0pt}{*2}{*0}
  \\titleformat*{\\section}{\\large\\bfseries}
  \\titleformat*{\\subsection}{\\normalsize\\bfseries}
  \\titleformat*{\\subsubsection}{\\normalsize\\bfseries}
  \\setlist[itemize]{leftmargin=*}
  \\setlist[enumerate]{leftmargin=*}
  \\setlength\\columnsep{5pt}
  \\setlength{\\columnseprule}{1pt}
  \\setlength{\\parindent}{0cm}
  \\usepackage{setspace}
  \\singlespacing
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
  :config
  (defvar-local jethro/org-multicol-latex-column-count
    3
    "Column count for multicolumn export.")

  (defun jethro/org-multicol-to-pdf (async subtreep visible-only body-only)
    (let ((contents (buffer-string))
          (buffer-name (file-name-sans-extension buffer-file-name))
          (col-count jethro/org-multicol-latex-column-count))
      (with-temp-buffer
        (insert "#+LATEX_CLASS: latex-notes\n")
        (insert contents)
        (goto-char (point-min))
        (org-next-visible-heading 1)
        (insert
         (format "#+BEGIN_EXPORT latex\n\\begin{multicols*}{%s}\n#+END_EXPORT\n" col-count))
        (goto-char (point-max))
        (insert "#+BEGIN_EXPORT latex\n\\end{multicols*}\n#+END_EXPORT")
        (org-export-to-file 'latex (format "%s.tex" buffer-name)
          async subtreep visible-only body-only nil (lambda (file) (org-latex-compile file))))))

  (org-export-define-derived-backend 'latex-notes 'latex
    :menu-entry
    '(?L "Export to LaTeX notes"
         ((?p "Export to PDF" jethro/org-multicol-to-pdf)))))

(use-package vc
  :bind (("C-x v =" . jethro/vc-diff)
         ("C-x v H" . vc-region-history)) ; New command in emacs 25.x
  :config
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
        (call-interactively #'vc-diff)))))

(use-package smerge-mode
  :bind (("C-c h s" . jethro/hydra-smerge/body))
  :init
  (defun jethro/enable-smerge-maybe ()
    "Auto-enable `smerge-mode' when merge conflict is detected."
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward "^<<<<<<< " nil :noerror)
        (smerge-mode 1))))
  (add-hook 'find-file-hook #'jethro/enable-smerge-maybe :append)
  :config
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
  :straight (magit :type git
                   :files ("lisp/magit*.el" "lisp/git*.el"
                           "Documentation/magit.texi" "Documentation/AUTHORS.md"
                           "COPYING" (:exclude "lisp/magit-popup.el"))
                   :host github :repo "magit/magit")
  :bind (("s-g" . magit-status)
         ("C-c g" . magit-status)
         ("s-G" . magit-blame-addition)
         ("C-c G" . magit-blame-addition))
  :hook
  (magit-mode . hl-line-mode)
  :custom
  (magit-auto-revert-mode nil)
  (magit-log-arguments '("-n100" "--graph" "--decorate"))
  :config
  (transient-append-suffix 'magit-log "a"
    '("w" "Wip" magit-wip-log-current))
  (magit-define-popup-switch 'magit-log-popup
    ?m "Omit merge commits" "--no-merges")
  (transient-append-suffix 'magit-log "-A"
    '("-m" "Omit merge commits" "--no-merges")))

(use-package git-link
  :commands
  (git-link git-link-commit git-link-homepage)
  :custom
  (git-link-use-commit t))

(use-package projectile
  :custom
  (projectile-use-git-grep t)
  (projectile-create-missing-test-files t)
  (projectile-completion-system 'ivy)
  (projectile-switch-project-action  #'projectile-commander)
  :config
  (define-key projectile-mode-map (kbd "C-x p") 'projectile-command-map)
  (projectile-mode +1)
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
    (magit-status)))

(use-package counsel-projectile
  :after ivy projectile
  :bind (("s-f" . counsel-projectile-find-file)
         ("s-b" . counsel-projectile-switch-to-buffer)
         ("C-c s" . counsel-projectile-rg)))

(use-package bury-successful-compilation
  :hook
  (prog-mode . bury-successful-compilation))

(use-package org-ref
  :after org)

(use-package org-ref-ox-hugo
  :straight (:host github :repo "jethrokuan/org-ref-ox-hugo")
  :after org org-ref ox-hugo
  :config
  (add-to-list 'org-ref-formatted-citation-formats
               '("md"
                 ("article" . "${author}, *${title}*, ${journal}, *${volume}(${number})*, ${pages} (${year}). ${doi}")
                 ("inproceedings" . "${author}, *${title}*, In ${editor}, ${booktitle} (pp. ${pages}) (${year}). ${address}: ${publisher}.")
                 ("book" . "${author}, *${title}* (${year}), ${address}: ${publisher}.")
                 ("phdthesis" . "${author}, *${title}* (Doctoral dissertation) (${year}). ${school}, ${address}.")
                 ("inbook" . "${author}, *${title}*, In ${editor} (Eds.), ${booktitle} (pp. ${pages}) (${year}). ${address}: ${publisher}.")
                 ("incollection" . "${author}, *${title}*, In ${editor} (Eds.), ${booktitle} (pp. ${pages}) (${year}). ${address}: ${publisher}.")
                 ("proceedings" . "${editor} (Eds.), _${booktitle}_ (${year}). ${address}: ${publisher}.")
                 ("unpublished" . "${author}, *${title}* (${year}). Unpublished manuscript.")
                 ("misc" . "${author} (${year}). *${title}*. Retrieved from [${howpublished}](${howpublished}). ${note}.")
                 (nil . "${author}, *${title}* (${year})."))))

(use-package pdf-tools
  :config
  (pdf-tools-install))
