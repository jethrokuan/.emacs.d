(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-revert-verbose nil)
 '(mail-envelope-from (quote header))
 '(mail-specify-envelope-from t)
 '(message-auto-save-directory "~/.mail/drafts/")
 '(message-kill-buffer-on-exit t)
 '(message-send-mail-function (quote message-send-mail-with-sendmail))
 '(message-sendmail-envelope-from (quote header))
 '(message-sendmail-f-is-evil nil)
 '(notmuch-always-prompt-for-sender t)
 '(notmuch-archive-tags (quote ("-inbox" "-unread")))
 '(notmuch-crypto-process-mime t)
 '(notmuch-hello-sections (quote (notmuch-hello-insert-saved-searches)))
 '(notmuch-labeler-hide-known-labels t t)
 '(notmuch-message-headers (quote ("To" "Cc" "Subject" "Bcc")))
 '(notmuch-saved-searches
   (quote
    ((:name "unread" :query "tag:unread")
     (:name "to-me" :query "tag:to-me")
     (:name "sent" :query "tag:sent")
     (:name "personal" :query "tag:personal")
     (:name "nushackers" :query "tag:nushackers")
     (:name "nus" :query "tag:nus"))))
 '(notmuch-search-oldest-first nil)
 '(package-selected-packages
   (quote
    (prettier-js lsp-javascript-typescript yapfify yaml-mode ws-butler writegood-mode which-key wgrep web-mode volatile-highlights visual-regexp use-package tide tao-theme sx sphinx-doc smartparens smart-mode-line smart-jump slime-company skewer-mode shm shackle scss-mode rust-mode rg rainbow-mode rainbow-delimiters pytest py-isort peep-dired paradox ox-ioslide org-ref org-pomodoro org-plus-contrib org-journal org-gcal org-download org-bullets olivetti notmuch no-littering nix-mode moody minions meghanada magit lsp-ui lsp-python lsp-java js-doc ivy-bibtex isend-mode intero interleave indium iedit ibuffer-projectile highlight-indent-guides groovy-mode gradle-mode gorepl-mode google-c-style golint go-dlv gist flyspell-correct-ivy flycheck-pos-tip flycheck-color-mode-line flycheck-clojure flx fish-mode eyebrowse expand-region eww-lnum esup esh-autosuggest ensime emmet-mode elfeed-org easy-kill dtrt-indent dockerfile-mode docker direnv dired-subtree dired-ranger dired-narrow diminish diff-hl deft crux counsel-projectile company-tern company-quickhelp company-lsp company-go company-auctex company-anaconda color-identifiers-mode clj-refactor bury-successful-compilation beacon avy autodisass-java-bytecode alchemist aggressive-indent adoc-mode)))
 '(sendmail-program "/run/current-system/sw/bin/msmtp"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-document-title ((t (:foreground "#171717" :weight bold :height 1.5))))
 '(org-done ((t (:background "#E8E8E8" :foreground "#0E0E0E" :strike-through t :weight bold))))
 '(org-headline-done ((t (:foreground "#171717" :strike-through t))))
 '(org-image-actual-width (quote (600)))
 '(org-level-1 ((t (:foreground "#090909" :weight bold :height 1.3))))
 '(org-level-2 ((t (:foreground "#090909" :weight normal :height 1.2))))
 '(org-level-3 ((t (:foreground "#090909" :weight normal :height 1.1))))
 '(variable-pitch ((t (:family "ETBembo")))))
