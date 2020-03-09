;;;; -*- lexical-binding: t -*-
;;; Straight setup
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

;;; Use-package Setup
(straight-use-package 'use-package)
(use-package blackout
  :straight (blackout :host github :repo "raxod502/blackout")
  :demand t)

(use-package el-patch
  :straight (:host github
                   :repo "raxod502/el-patch"
                   :branch "develop"))
(eval-when-compile
  (require 'el-patch))

(use-package use-package-company
  :straight (use-package-company :host github :repo "akirak/use-package-company"))

;;; EXWM setup
(add-to-list 'load-path (expand-file-name "elisp" user-emacs-directory))
(require 'emacsql-sqlite)
(require 'exwm)
(when (featurep 'exwm)
  (require 'exwm-config))

;;; The Basics
(setq user-full-name "Jethro Kuan"
      user-mail-address "jethrokuan95@gmail.com")

(defun jethro/phone-p ()
  (and (equal (system-name) "localhost")
       (not (equal user-login-name "jethro"))))

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

(use-package autorevert
  :straight nil
  :blackout t
  :hook
  (dired-mode . auto-revert-mode)
  :config
  (global-auto-revert-mode +1)
  :custom
  (auto-revert-verbose nil))

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(defalias 'yes-or-no-p 'y-or-n-p)

(add-hook 'after-init-hook #'delete-selection-mode)

(setq sentence-end-double-space nil)

(setq-default tab-width 2)
(setq-default js-indent-level 2)
(setq-default indent-tabs-mode nil)

;; For when Emacs is started in GUI mode:
(setq create-lockfiles nil)
(setq browse-url-browser-function 'browse-url-xdg-open)

(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

;;; Theming

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

(use-package doom-themes
  :config
  (load-theme 'doom-tomorrow-night t)
  (doom-themes-visual-bell-config)
  (doom-themes-org-config))

(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :config
  (setq doom-modeline-enable-word-count t)
  (setq doom-modeline-buffer-encoding nil)
  (setq doom-modeline-github t))

(setq show-paren-style 'parenthesis
      show-paren-delay 0
      show-paren-highlight-openparen t
      show-paren-when-point-inside-paren nil
      show-paren-when-point-in-periphery t)
(show-paren-mode 1)

(blink-cursor-mode 0)

(use-package isearch
  :straight nil
  :custom
  (search-highlight t)
  (search-whitespace-regexp ".*?")
  (isearch-lax-whitespace t)
  (isearch-regexp-lax-whitespace nil)
  (isearch-lazy-highlight t)
  (isearch-lazy-count t)
  (lazy-count-prefix-format " (%s/%s) ")
  (lazy-count-suffix-format nil)
  (isearch-yank-on-move 'shift)
  (isearch-allow-scroll 'unlimited))

(add-hook 'after-init-hook #'column-number-mode)

(use-package subword
  :straight nil
  :hook (prog-mode . subword-mode))

(use-package hl-todo
  :config
  (global-hl-todo-mode))

;;; Navigation
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

(use-package beacon
  :blackout beacon-mode
  :custom
  (beacon-push-mark 10)
  :config
  (beacon-mode +1))

(use-package volatile-highlights
  :blackout volatile-highlights-mode
  :config
  (volatile-highlights-mode +1))

(use-package hydra
  :config
  (defhydra jethro/hydra-zoom ()
    "zoom"
    ("i" text-scale-increase "in")
    ("o" text-scale-decrease "out"))

  (bind-key "C-c h z" 'jethro/hydra-zoom/body))

;;; Mail (notmuch)
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
         ("C-<return>" . browse-url-at-point)
         :map notmuch-show-mode-map
         ("C" . jethro/org-capture-email))
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
  (notmuch-saved-searches '((:name "unread" :query "tag:inbox and tag:unread")
                            (:name "org-roam" :query "tag:inbox and tag:roam")
                            (:name "personal" :query "tag:inbox and tag:personal")
                            (:name "nushackers" :query "tag:inbox and tag:nushackers")
                            (:name "nus" :query "tag:inbox and tag:nus")
                            (:name "drafts" :query "tag:draft")))
  :config
  (defun jethro/org-capture-email ()
    (interactive)
    (org-capture nil "e")))

(use-package org-msg
  ;; Only for mu4e
  :disabled t
  :hook (message-mode . org-msg-mode)
  :custom
  (org-msg-options "html-postamble:nil H:5 num:nil ^:{} toc:nil")
  (org-msg-greeting-name-limit 3)
  (org-msg-greeting-fmt "\nHi *%s*,\n\n")
  (org-msg-startup "hidestars indent inlineimages")
  (org-msg-signature "\n\nRegards,\nJethro Kuan"))

;;; Dired
(let ((gls (executable-find "gls")))
  (when gls
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
  :demand t
  :bind (:map dired-mode-map
              ("/" . dired-narrow-fuzzy)))
;;; Utilities
(use-package crux
  :bind (("C-c o" . crux-open-with)
         ("C-a" . crux-move-beginning-of-line)
         ("C-c r" . crux-rename-file-and-buffer)))

;;; Ivy
(use-package counsel
  :demand t
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
   ("C-c s" . counsel-rg)
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
  (ivy-use-virtual-buffers t)
  (enable-recursive-minibuffers t)
  (ivy-display-style 'fancy)
  (ivy-use-selectable-prompt t)
  (ivy-re-builders-alist
   '((t . ivy--regex-plus)))
  :config
  (ivy-mode +1))

(use-package ivy-rich
  :init
  (setq ivy-rich-display-transformers-list ; max column width sum = (ivy-poframe-width - 1)
	      '(ivy-switch-buffer
          (:columns
           ((ivy-rich-candidate (:width 30))  ; return the candidate itself
            (ivy-rich-switch-buffer-size (:width 7))  ; return the buffer size
            (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right)); return the buffer indicators
            (ivy-rich-switch-buffer-major-mode (:width 12 :face warning))          ; return the major mode info
            (ivy-rich-switch-buffer-project (:width 15 :face success))             ; return project name using `projectile'
            (ivy-rich-switch-buffer-path (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3))))))  ; return file path relative to project root or `default-directory' if project is nil
           :predicate
           (lambda (cand) (get-buffer cand)))
          counsel-M-x
	        (:columns
	         ((counsel-M-x-transformer (:width 35))
	          (ivy-rich-counsel-function-docstring (:width 34 :face font-lock-doc-face))))
	        counsel-describe-function
	        (:columns
	         ((counsel-describe-function-transformer (:width 35))
	          (ivy-rich-counsel-function-docstring (:width 34 :face font-lock-doc-face))))
	        counsel-describe-variable
	        (:columns
	         ((counsel-describe-variable-transformer (:width 35))
	          (ivy-rich-counsel-variable-docstring (:width 34 :face font-lock-doc-face))))))
  :config
  (ivy-rich-mode +1)
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line))

;;; Project Management
(use-package projectile
  :custom
  (projectile-use-git-grep t)
  (projectile-create-missing-test-files t)
  (projectile-completion-system 'ivy)
  :config
  (define-key projectile-mode-map (kbd "C-x p") 'projectile-command-map)
  (projectile-mode +1))

(use-package wgrep
  :commands
  wgrep-change-to-wgrep-mode)

(use-package deadgrep
  :if (executable-find "rg")
  :bind* (("M-s" . deadgrep)))

(use-package ripgrep
  :if (executable-find "rg"))

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

;;; Version Control
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

;;; Text Editing
(autoload 'zap-up-to-char "misc"
  "Kill up to, but not including ARGth occurrence of CHAR.

  \(fn arg char)"
  'interactive)

(bind-key "M-z" 'zap-up-to-char)

(use-package ws-butler
  :blackout 'ws-butler-mode
  :hook
  (prog-mode . ws-butler-mode))

(use-package easy-kill
  :bind*
  (([remap kill-ring-save] . easy-kill)))

(use-package emojify
  :defer 10
  :custom
  (emojify-emoji-styles '(unicode))
  :bind (("C-c e" . emojify-insert-emoji))
  :config
  (global-emojify-mode +1))

(use-package whitespace
  :straight nil
  :blackout whitespace-mode
  :hook (prog-mode . whitespace-mode)
  :custom
  (whitespace-line-column 80)
  (whitespace-style '(face lines-tail)))

(use-package flyspell
  :straight nil
  :blackout flyspell-mode
  :hook
  (text-mode . flyspell-mode)
  :custom
  (flyspell-abbrev-p t)
  (ispell-extra-args '("--encoding=utf-8" "--sug-mode=ultra")))

(use-package flyspell-correct-ivy
  :bind
  (:map flyspell-mode-map
        (("C-;" . flyspell-correct-wrapper)))
  :custom
  (flyspell-correct-interface #'flyspell-correct-ivy))

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
;;; Writing
(use-package olivetti
  :commands (olivetti-mode))

;;; Programming Utilities
(use-package highlight-indent-guides
  :hook
  (python-mode . highlight-indent-guides-mode)
  (yaml-mode . highlight-indent-guides-mode)
  :custom
  (highlight-indent-guides-method 'character))

(use-package apheleia
  :straight (apheleia :host github :repo "raxod502/apheleia")
  :config
  (apheleia-global-mode +1))

(use-package aggressive-indent
  :blackout aggressive-indent-mode
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

(use-package expand-region
  :bind (("C-=" . er/expand-region)))

(use-package smartparens
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
  :init
  (smartparens-global-strict-mode +1)
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

(use-package bury-successful-compilation
  :hook
  (prog-mode . bury-successful-compilation))

(use-package direnv
  :demand t
  :if (executable-find "direnv")
  :custom
  (direnv-always-show-summary nil)
  :config
  (with-eval-after-load 'flycheck
    (setq flycheck-executable-find
          (lambda (cmd)
            (add-hook 'post-command-hook #'direnv--maybe-update-environment)
            (direnv-update-environment default-directory)
            (executable-find cmd))))
  (direnv-mode +1))

(use-package flycheck
  :commands (flycheck-mode
             flycheck-next-error
             flycheck-previous-error)
  :init
  (dolist (where '((emacs-lisp-mode-hook . emacs-lisp-mode-map)))
    (add-hook (car where)
              `(lambda ()
                 (bind-key "M-n" #'flycheck-next-error ,(cdr where))
                 (bind-key "M-p" #'flycheck-previous-error ,(cdr where)))))

  (when (executable-find "proselint")
    (add-hook 'text-mode-hook 'flycheck-mode)))

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

(use-package yasnippet
  :blackout ((yas-global-mode . t)
             (yas-minor-mode . t))
  :config
  (add-to-list 'load-path (expand-file-name "snippets" user-emacs-directory))
  (require 'yasnippet-snippets)
  (yas-global-mode +1)
  :custom
  (yas-snippet-dirs (list (expand-file-name "snippets/snippets" user-emacs-directory))))

(use-package company
  :blackout company-mode
  :defines (company-dabbrev-ignore-case company-dabbrev-downcase)
  :commands company-abort
  :bind (("M-/" . company-complete)
         ("C-/" . company-yasnippet)
         :map company-active-map
         ("M-n" . nil)
         ("M-p" . nil)
         ("C-n" . company-select-next)
         ("C-p" . company-select-previous))
  :custom
  (company-dabbrev-downcase nil)
  (company-idle-delay 0.5)
  (company-require-match nil)
  (company-minimum-prefix-length 2)
  (company-tooltip-align-annotations t)
  :init
  (global-company-mode +1))

(use-package company-quickhelp
  :after company
  :bind (:map company-active-map
              ("M-h" . company-quickhelp-manual-begin))
  :hook
  (company-mode . company-quickhelp-mode))

(use-package dtrt-indent
  :blackout t
  :config
  (dtrt-indent-mode +1))

(use-package lsp-mode
  :commands lsp
  :hook
  (lsp-after-open-hook . lsp-enable-imenu)
  (python-mode . lsp)
  (c++-mode . lsp)
  (c-mode . lsp)
  :custom
  (lsp-message-project-root-warning t))

(use-package lsp-ui
  :after lsp-mode
  :hook
  (lsp-mode . lsp-ui-mode)
  :config
  (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references))

(use-package company-lsp
  :after (company lsp)
  :company lsp-mode)

(use-package dap-mode)

(use-package outshine
  :hook
  (emacs-lisp-mode . outshine-mode)
  :straight (outshine :host github :repo "alphapapa/outshine"))

;;; Emacs Lisp
(use-package elisp-mode
  :straight nil
  :bind (:map emacs-lisp-mode-map
              ("C-c C-x" . ielm)
              ("C-c C-c" . eval-defun)
              ("C-c C-k" . eval-buffer)))

(use-package emacsql
  :straight nil
  :hook
  (emacs-mode . emacsql-fix-vector-indentation)
  :bind
  (:map emacs-lisp-mode-map
        (("C-c C-e" . emacsql-show-last-sql))))

(use-package helpful
  :defines (counsel-describe-function-function
            counsel-describe-variable-function)
  :commands helpful--buffer
  :bind (([remap describe-key] . helpful-key)
         ([remap describe-symbol] . helpful-symbol)
         ("C-c C-d" . helpful-at-point))
  :init
  (with-eval-after-load 'counsel
    (setq counsel-describe-function-function #'helpful-callable
          counsel-describe-variable-function #'helpful-variable))

  (with-eval-after-load 'apropos
    ;; patch apropos buttons to call helpful instead of help
    (dolist (fun-bt '(apropos-function apropos-macro apropos-command))
      (button-type-put
       fun-bt 'action
       (lambda (button)
         (helpful-callable (button-get button 'apropos-symbol)))))
    (dolist (var-bt '(apropos-variable apropos-user-option))
      (button-type-put
       var-bt 'action
       (lambda (button)
         (helpful-variable (button-get button 'apropos-symbol)))))))

;;; Docker
(use-package docker
  :commands docker-mode)

(use-package dockerfile-mode
  :mode "Dockerfile\\'")

;;; C/C++
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

;;; Fish
(use-package fish-mode
  :mode ("\\.fish\\'" . fish-mode))

;;; Rust
(use-package rust-mode
  :mode ("\\.rs\\'" . rust-mode))

(with-eval-after-load "python-mode"
  (setq python-remove-cwd-from-path t))

;;; Web
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
  :blackout emmet-mode
  :hook
  (web-mode . emmet-mode)
  (vue-mode . emmet-mode))

(use-package rainbow-mode
  :blackout rainbow-mode
  :hook
  (css-mode . rainbow-mode)
  (scss-mode . rainbow-mode))

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

;;; JSON
(use-package json-mode
  :mode "\\.json\\'"
  :hook
  (json-mode . (lambda ()
                 (make-local-variable 'js-indent-level)
                 (setq js-indent-level 2))))

;;; Markdown
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

;;; Nix
(use-package nix-mode
  :mode ("\\.nix\\'" . nix-mode))

(use-package nix-update
  :commands nix-update-fetch)

;;; LaTeX
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

;;; YAML
(use-package yaml-mode
  :mode ("\\.yaml\\'" . yaml-mode))

;;; R/ESS
(use-package ess)

;;; Org-mode
(straight-use-package 'org-plus-contrib)

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
     (dot . t)
     (R . t)))
  (org-confirm-babel-evaluate nil)
  (org-use-speed-commands t)
  (org-catch-invisible-edits 'show)
  (org-preview-latex-image-directory "/tmp/ltximg/")
  :custom-face
  (variable-pitch ((t (:family "Libre Baskerville"))))
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
  (org-startup-indented nil)
  (org-hide-leading-stars nil)
  (org-hide-emphasis-markers nil)
  (org-pretty-entities nil)
  (org-adapt-indentation nil)
  :init
  (with-eval-after-load 'flycheck
    (flycheck-add-mode 'proselint 'org-mode))
  :config
  (require 'org-habit)
  (require 'org-tempo)
  (require 'ol-notmuch))

(require 'org)

(defun jethro/style-org ()
  (setq line-spacing 0.2)
  (variable-pitch-mode +1)
  (mapc
   (lambda (face) ;; Other fonts with fixed-pitch.
     (set-face-attribute face nil :inherit 'fixed-pitch))
   (list 'org-code
         'org-block
         'org-table
         'org-verbatim
         'org-block-begin-line
         'org-block-end-line
         'org-meta-line
         'org-document-info-keyword)))

(add-hook 'org-mode-hook #'jethro/style-org)

(defun jethro/org-archive-done-tasks ()
  "Archive all done tasks."
  (interactive)
  (org-map-entries 'org-archive-subtree "/DONE" 'file))

(require 'find-lisp)
(setq jethro/org-agenda-directory "~/.org/gtd/")
(setq org-agenda-files
      (find-lisp-find-files jethro/org-agenda-directory "\.org$"))

;; Setup org-protocol
(require 'org-protocol)
(require 'org-capture)
(add-to-list 'org-capture-templates
             `("i" "inbox" entry (file ,(concat jethro/org-agenda-directory "inbox.org"))
               "* TODO %?"))
(add-to-list 'org-capture-templates
             `("e" "email" entry (file+headline ,(concat jethro/org-agenda-directory "emails.org") "Emails")
               "* TODO [#A] Reply: %a :@home:@school:"
               :immediate-finish t))
(add-to-list 'org-capture-templates
             `("c" "org-protocol-capture" entry (file ,(concat jethro/org-agenda-directory "inbox.org"))
               "* TODO [[%:link][%:description]]\n\n %i"
               :immediate-finish t))
(add-to-list 'org-capture-templates
             `("w" "Weekly Review" entry (file+olp+datetree ,(concat jethro/org-agenda-directory "reviews.org"))
               (file ,(concat jethro/org-agenda-directory "templates/weekly_review.org"))))

(add-to-list 'org-agenda-custom-commands
             `("r" "Reading" todo ""
               ((org-agenda-files '(,(concat jethro/org-agenda-directory "reading.org"))))))

(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
        (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)")))

(setq org-log-done 'time
      org-log-into-drawer t
      org-log-state-notes-insert-after-drawers nil)

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

(defvar jethro/org-current-effort "1:00"
  "Current effort for agenda items.")

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
                 (if (not org-agenda-persistent-marks) "" " (kept marked)")))))

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
                ))
         (todo "TODO"
               ((org-agenda-overriding-header "Projects")
                (org-agenda-files '(,(concat jethro/org-agenda-directory "projects.org")))
                ))
         (todo "TODO"
               ((org-agenda-overriding-header "One-off Tasks")
                (org-agenda-files '(,(concat jethro/org-agenda-directory "next.org")))
                (org-agenda-skip-function '(org-agenda-skip-entry-if 'deadline 'scheduled))))
         nil)))

(add-to-list 'org-agenda-custom-commands `,jethro/org-agenda-todo-view)

(defun jethro/switch-to-agenda ()
  (interactive)
  (org-agenda nil " "))

(bind-key "<f1>" 'jethro/switch-to-agenda)

(setq org-columns-default-format "%40ITEM(Task) %Effort(EE){:} %CLOCKSUM(Time Spent) %SCHEDULED(Scheduled) %DEADLINE(Deadline)")

(use-package org-cliplink
  :bind
  ("C-c C" . 'jethro/org-capture-link)
  :config
  (defun jethro/org-capture-link ()
    "Captures a link, and stores it in inbox."
    (interactive)
    (org-capture nil "l")))

(use-package deft
  :disabled t
  :after org
  :bind
  ("C-c n d" . deft)
  :custom
  (deft-recursive t)
  (deft-use-filter-string-for-filename t)
  (deft-default-extension "org")
  (deft-directory "/home/jethro/Dropbox/org/braindump/org/")
  :config/el-patch
  (defun deft-parse-title (file contents)
    "Parse the given FILE and CONTENTS and determine the title.
If `deft-use-filename-as-title' is nil, the title is taken to
be the first non-empty line of the FILE.  Else the base name of the FILE is
used as title."
    (el-patch-swap (if deft-use-filename-as-title
                       (deft-base-filename file)
                     (let ((begin (string-match "^.+$" contents)))
                       (if begin
                           (funcall deft-parse-title-function
                                    (substring contents begin (match-end 0))))))
                   (org-roam--get-title-or-slug file))))

(use-package org-roam
  :ensure nil
  :commands (org-roam-build-cache)
  :straight (:host github :repo "jethrokuan/org-roam")
  :hook
  (after-init . org-roam-mode)
  :bind (:map org-roam-mode-map
              (("C-c n l" . org-roam)
               ("C-c n f" . org-roam-find-file)
               ("C-c n g" . org-roam-show-graph)
               ("C-c n b" . org-roam-switch-to-buffer))
              :map org-mode-map
              (("C-c n i" . org-roam-insert)))
  :custom
  (org-roam-directory "/home/jethro/Dropbox/org/braindump/org/")
  :custom-face
  (org-roam-link ((t (:inherit org-link :foreground "#C991E1"))))
  :config
  (require 'org-roam-protocol)
  (defun jethro/conditional-hugo-enable ()
    (save-excursion
      (if (cdr (assoc "SETUPFILE" (org-roam--extract-global-props '("SETUPFILE"))))
          (org-hugo-auto-export-mode +1)
        (org-hugo-auto-export-mode -1))))

  (with-eval-after-load 'org
    (defun my/org-roam--backlinks-list (file)
      (if (org-roam--org-roam-file-p file)
          (--reduce-from
           (concat acc (format "- [[file:%s][%s]]\n"
                               (file-relative-name (car it) org-roam-directory)
                               (org-roam--get-title-or-slug (car it))))
           "" (org-roam-sql [:select [file-from]
                             :from file-links
                             :where (= file-to $s1)
                             :and file-from :not :like $s2] file "%private%"))
        ""))
    (defun my/org-export-preprocessor (_backend)
      (let ((links (my/org-roam--backlinks-list (buffer-file-name))))
        (unless (string= links "")
          (save-excursion
            (goto-char (point-max))
            (insert (concat "\n* Backlinks\n" links))))))
    (add-hook 'org-export-before-processing-hook 'my/org-export-preprocessor))
  (setq org-roam-capture-templates
        '(("d" "default" plain (function org-roam--capture-get-point)
           "%?"
           :file-name "${slug}"
           :head "#+SETUPFILE:./hugo_setup.org
#+HUGO_SECTION: zettels
#+HUGO_SLUG: ${slug}
#+TITLE: ${title}\n"
           :unnarrowed t)
          ("p" "private" plain (function org-roam--capture-get-point)
           "%?"
           :file-name "private-${slug}"
           :head "#+TITLE: ${title}\n"
           :unnarrowed t)))
  (setq org-roam-ref-capture-templates
        '(("r" "ref" plain (function org-roam--capture-get-point)
           "%?"
           :file-name "websites/${slug}"
           :head "#+SETUPFILE:./hugo_setup.org
#+ROAM_KEY: ${ref}
#+HUGO_SLUG: ${slug}
#+TITLE: ${title}

- source :: ${ref}"
           :unnarrowed t))))

(use-package org-fc
  :straight (:host github
                   :repo "l3kn/org-fc"
                   :files (:defaults "awk"))
  :bind
  ("C-c f" . org-fc-hydra/body)
  :custom
  (org-fc-directories '("/home/jethro/Dropbox/org/braindump/org/"))
  (org-fc-review-history-file "/home/jethro/Dropbox/org/braindump/org/org-fc.tsv")
  :config
  (require 'org-fc-hydra))

(use-package erefactor)

(use-package org-journal
  :bind
  ("C-c n j" . org-journal-new-entry)
  ("C-c n t" . org-journal-today)
  :custom
  (org-journal-date-prefix "#+TITLE: ")
  (org-journal-file-format "private-%Y-%m-%d.org")
  (org-journal-dir "/home/jethro/Dropbox/org/braindump/org/")
  (org-journal-carryover-items nil)
  (org-journal-date-format "%Y-%m-%d")
  :config
  (defun org-journal-today ()
    (interactive)
    (org-journal-new-entry t)))

(use-package org-download
  :after org
  :bind
  (:map org-mode-map
        (("s-Y" . org-download-screenshot)
         ("s-y" . org-download-yank)))
  :config
  (if (memq window-system '(mac ns))
      (setq org-download-screenshot-method "screencapture -i %s")
    (setq org-download-screenshot-method "maim -s %s"))
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

(use-package ox-hugo
  :if (executable-find "hugo")
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
  (org-preview-latex-default-process 'imagemagick)
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

(use-package org-ref
  :after org)

(use-package org-ref-ox-hugo
  :straight (:host github :repo "jethrokuan/org-ref-ox-hugo" :branch "custom/overrides")
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


(use-package password-store)

(use-package org-gcal
  :custom
  (org-gcal-client-id (password-store-get "gmail/org-gcal-client"))
  (org-gcal-client-secret (password-store-get "gmail/org-gcal"))
  (org-gcal-fetch-file-alist '(("jethrokuan95@gmail.com" . "~/.org/gtd/calendars/personal.org")
                               ("dckbhpq9bq13m03llerl09slgo@group.calendar.google.com" . "~/.org/gtd/calendars/lab.org"))))

;;; Others
(use-package mathpix.el
  :after password-store
  :straight (:host github :repo "jethrokuan/mathpix.el")
  :custom ((mathpix-screenshot-method "maim -s %s")
           (mathpix-app-id (password-store-get "mathpix/app-id"))
           (mathpix-app-key (password-store-get "mathpix/app-key")))
  :bind
  ("C-x m" . mathpix-screenshot))

(use-package slack
  :commands (slack-start)
  :bind (:map slack-mode-map
              (("@" . slack-message-embed-mention)
               ("#" . slack-message-embed-channel)))
  :custom
  (slack-buffer-emojify t)
  (slack-prefer-current-team t)
  :config
  (slack-register-team
   :name "crslab"
   :token (password-store-get "slack-tokens/crslab")
   :full-and-display-names t)
  (slack-register-team
   :name "orgroam"
   :default t
   :token (password-store-get "slack-tokens/orgroam")
   :subscribe '("general")
   :full-and-display-names t))

(use-package alert
  :commands (alert)
  :custom (alert-default-style 'message))

(use-package anki-editor)

(use-package gif-screencast
  :straight (:host gitlab :repo "ambrevar/emacs-gif-screencast")
  :bind
  ("<f12>" . gif-screencast-start-or-stop))

;; Local Variables:
;; outline-regexp: ";;;+ "
;; End:
