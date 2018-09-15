(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(LaTeX-fill-break-at-separators nil t)
 '(TeX-auto-save t t)
 '(TeX-parse-self t t)
 '(TeX-source-correlate-start-server nil t)
 '(TeX-syntactic-comment t t)
 '(TeX-view-program-list (quote (("zathura" "zathura --page=%(outpage) %o"))) t)
 '(TeX-view-program-selection (quote ((output-pdf "zathura"))) t)
 '(aggressive-indent-excluded-modes
   (quote
    (bibtex-mode cider-repl-mode coffee-mode comint-mode conf-mode Custom-mode diff-mode doc-view-mode dos-mode erc-mode jabber-chat-mode haml-mode intero-mode haskell-mode interative-haskell-mode haskell-interactive-mode image-mode makefile-mode makefile-gmake-mode minibuffer-inactive-mode netcmd-mode python-mode sass-mode slim-mode special-mode shell-mode snippet-mode eshell-mode tabulated-list-mode term-mode TeX-output-mode text-mode yaml-mode)))
 '(auth-source-backend (quote (password-store)) t)
 '(auto-revert-verbose nil)
 '(avy-keys (quote (104 116 110 115 109 119 118 122)) t)
 '(beacon-push-mark 10 t)
 '(browse-url-browser-function (quote browse-url-firefox))
 '(browse-url-firefox-new-window-is-tab t)
 '(browse-url-new-window-flag t)
 '(cider-cljs-lein-repl
   "(do (use 'figwheel-sidecar.repl-api) (start-figwheel!) (cljs-repl))" t)
 '(cider-font-lock-dynamically (quote (macro core function var)))
 '(cider-overlays-use-font-lock t)
 '(cider-prompt-save-file-on-load (quote always-save) t)
 '(cider-repl-display-in-current-window t)
 '(cider-repl-result-prefix ";; => ")
 '(cider-repl-use-clojure-font-lock t)
 '(cider-show-error-buffer nil)
 '(company-begin-commands (quote (self-insert-command)) t)
 '(company-dabbrev-code-ignore-case nil t)
 '(company-dabbrev-downcase nil t)
 '(company-dabbrev-ignore-case nil t)
 '(company-idle-delay 0 t)
 '(company-minimum-prefix-length 2 t)
 '(company-require-match nil t)
 '(company-transformers (quote (company-sort-by-occurrence)) t)
 '(counsel-find-file-at-point t t)
 '(counsel-grep-base-command
   "rg -i -M 120 --no-heading --line-number --color never '%s' %s" t)
 '(counsel-grep-swiper-limit 20000 t)
 '(counsel-rg-base-command
   "rg -i -M 120 --no-heading --line-number --color never %s ." t)
 '(custom-safe-themes
   (quote
    ("c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" default)))
 '(deft-default-extension "org" t)
 '(deft-directory "~/.org/deft/" t)
 '(deft-use-filename-as-title t t)
 '(direnv-always-show-summary t)
 '(elfeed-db-directory "~/.elfeed" t)
 '(eshell-destroy-buffer-when-process-dies t t)
 '(eshell-error-if-no-glob nil t)
 '(eshell-glob-case-insensitive nil t)
 '(eshell-hist-ignoredups t t)
 '(eshell-prefer-lisp-functions nil t)
 '(eshell-review-quick-commands nil t)
 '(eshell-save-history-on-exit t t)
 '(eshell-scroll-to-bottom-on-input (quote all) t)
 '(eshell-smart-space-goes-to-end t t)
 '(eshell-where-to-jump (quote begin) t)
 '(highlight-indent-guides-method (quote character) t)
 '(ibuffer-expert t)
 '(inferior-lisp-program "sbcl" t)
 '(ivy-display-style (quote fancy) t)
 '(ivy-initial-inputs-alist nil t)
 '(ivy-re-builders-alist
   (quote
    ((ivy-switch-buffer . ivy--regex-plus)
     (swiper . ivy--regex-plus)
     (t . ivy--regex-fuzzy))) t)
 '(ivy-use-selectable-prompt t t)
 '(ivy-use-virtual-buffers t t)
 '(js-doc-author "Jethro Kuan <jethrokuan95@gmail.com>" t)
 '(js-doc-license "MIT" t)
 '(js-doc-mail-address "jethrokuan95@gmail.com" t)
 '(js-doc-url "http://www.jethrokuan.com/" t)
 '(js-switch-indent-offset 2 t)
 '(lsp-message-project-root-warning t t)
 '(magit-auto-revert-mode nil)
 '(mail-envelope-from (quote header))
 '(mail-interactive t)
 '(mail-specify-envelope-from t)
 '(markdown-asymmetric-header t t)
 '(markdown-command "multimarkdown --snippet --smart --notes" t)
 '(markdown-enable-wiki-links t t)
 '(markdown-fontify-code-blocks-natively t t)
 '(markdown-indent-on-enter (quote indent-and-new-item) t)
 '(markdown-live-preview-delete-export (quote delete-on-destroy) t)
 '(message-kill-buffer-on-exit t t)
 '(message-send-mail-function (quote message-send-mail-with-sendmail) t)
 '(message-sendmail-envelope-from (quote header) t)
 '(message-sendmail-f-is-evil nil t)
 '(notmuch-always-prompt-for-sender t t)
 '(notmuch-archive-tags (quote ("-inbox" "-unread")) t)
 '(notmuch-crypto-process-mime t t)
 '(notmuch-draft-save-plaintext t)
 '(notmuch-hello-sections (quote (notmuch-hello-insert-saved-searches)) t)
 '(notmuch-labeler-hide-known-labels t t)
 '(notmuch-message-headers (quote ("To" "Cc" "Subject" "Date")) t)
 '(notmuch-saved-searches
   (quote
    ((:name "personal" :query "tag:inbox AND tag:personal")
     (:name "unread" :query "tag:inbox AND tag:unread")
     (:name "nushackers" :query "tag:inbox AND tag:nushackers")
     (:name "nus" :query "tag:inbox AND tag:nus")
     (:name "untagged" :query "tag:untagged"))) t)
 '(notmuch-search-oldest-first nil t)
 '(nrepl-hide-special-buffers t)
 '(nrepl-log-messages t)
 '(olivetti-body-width 80 t)
 '(org-agenda-diary-file "~/.org/diary.org")
 '(org-babel-load-languages (quote ((emacs-lisp . t) (python . t))))
 '(org-bullets-bullet-list (quote (" ")) t)
 '(org-download-image-dir "./images")
 '(org-format-latex-options
   (quote
    (:foreground default :background default :scale 2.0 :html-foreground "Black" :html-background "Transparent" :html-scale 1.0 :matchers
                 ("begin" "$1" "$" "$$" "\\(" "\\["))))
 '(org-html-head-include-default-style nil t)
 '(org-html-htmlize-output-type nil t)
 '(org-journal-dir "~/.org/journal/")
 '(org-latex-classes
   (quote
    (("article" "\\documentclass{article}
\\usepackage[margin=1in]{geometry}
\\usepackage{amsmath,amsthm,amssymb}

\\newcommand{\\N}{\\mathbb{N}}
\\newcommand{\\Z}{\\mathbb{Z}}

\\usepackage{hyperref}
\\usepackage{minted}
\\usepackage{tabularx}
\\usepackage{parskip}
\\linespread{1.1}
\\renewcommand\\headrulewidth{0.4pt}
\\renewcommand\\footrulewidth{0.4pt}
\\setlength\\columnsep{10pt}
\\setlength{\\columnseprule}{1pt}"
      ("\\section{%s}" . "\\section*{%s}")
      ("\\subsection{%s}" . "\\subsection*{%s}")
      ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
      ("\\paragraph{%s}" . "\\paragraph*{%s}")
      ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
     ("book" "\\documentclass[10pt]{memoir}
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
     ("latex-notes" "\\documentclass[8pt]{article}
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
      ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))) t)
 '(org-latex-default-table-environment "tabular" t)
 '(org-latex-listings (quote minted) t)
 '(org-latex-pdf-process
   (quote
    ("pdflatex -shell-escape -interaction nonstopmode %f" "pdflatex -shell-escape -interaction nonstopmode %f")) t)
 '(org-latex-tables-booktabs t t)
 '(org-pomodoro-format "%s" t)
 '(org-publish-project-alist
   (quote
    (("org-notes-assets" :base-directory "~/.org/deft/css/" :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf" :publishing-directory "~/.org/deft/docs/css/" :recursive t :publishing-function org-publish-attachment)
     ("org-notes-images" :base-directory "~/.org/deft/images/" :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf" :publishing-directory "~/.org/deft/docs/images/" :recursive t :publishing-function org-publish-attachment)
     ("org-notes" :base-directory "~/.org/deft/" :base-extension "org" :publishing-directory "~/.org/deft/docs/" :recursive t :publishing-function org-html-publish-to-html :headline-levels 4 :auto-sitemap t :author "Jethro Kuan" :email "jethrokuan95@gmail.com" :sitemap-filename "index.org" :sitemap-title "Jethro's Braindump" :style "<link rel=\"stylesheet\" href=\"https://unpkg.com/sakura.css/css/sakura.css\" type=\"text/css\">")
     ("org" :components
      ("org-notes-assets" "org-notes-images")))) t)
 '(org-return-follows-link t)
 '(package-selected-packages
   (quote
    (auto-revert latex tex-site sx zenburn-theme yapfify yaml-mode ws-butler writegood-mode which-key wgrep web-mode volatile-highlights visual-regexp use-package tide tao-theme sphinx-doc sos smartparens smart-mode-line smart-jump slime-company skewer-mode shm shackle scss-mode rust-mode rjsx-mode rg rainbow-mode rainbow-delimiters pytest py-isort peep-dired paradox ox-ioslide org-ref org-pomodoro org-plus-contrib org-journal org-gcal org-download org-bullets olivetti notmuch no-littering nix-mode meghanada magit lsp-ui lsp-python lsp-java js2-refactor js-doc ivy-bibtex isend-mode intero interleave indium iedit ibuffer-projectile highlight-indent-guides groovy-mode gradle-mode gorepl-mode google-c-style golint go-dlv gist flycheck-pos-tip flycheck-color-mode-line flycheck-clojure flx fish-mode eyebrowse expand-region eww-lnum esup esh-autosuggest ensime emmet-mode elfeed-org easy-kill dockerfile-mode docker direnv dired-subtree dired-ranger dired-narrow diminish diff-hl deft crux counsel-projectile company-tern company-quickhelp company-lsp company-go company-auctex company-anaconda color-identifiers-mode clj-refactor bury-successful-compilation beacon avy autodisass-java-bytecode alchemist aggressive-indent adoc-mode)))
 '(pdf-view-resize-factor 1.1 t)
 '(projectile-completion-system (quote ivy))
 '(projectile-create-missing-test-files t)
 '(projectile-switch-project-action (quote projectile-commander))
 '(projectile-use-git-grep t)
 '(rmh-elfeed-org-files (quote ("~/.org/deft/feeds.org")) t)
 '(safe-local-variable-values
   (quote
    ((org-download-image-dir . "./images/linear_algebra/")
     (org-download-image-dir . "./images/computer_vision/")
     (org-download-image-dir . "./images/ai/")
     (org-download-image-dir . "./images/nlp/")
     (org-download-image-dir . "./images/operating_systems/")
     (org-download-image-dir . "./images/software-engineering/")
     (org-download-image-dir . "./images/spark/")
     (org-download-image-dir . "./images/data_viz/"))))
 '(scss-compile-at-save nil t)
 '(sendmail-program "/run/current-system/sw/bin/msmtp")
 '(shackle-rules
   (quote
    ((compilation-mode :select nil)
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
 '(shr-external-browser (quote browse-url-generic) t)
 '(shr-width 80 t)
 '(slime-contribs (quote (slime-fancy)) t)
 '(sml/mode-width (quote full))
 '(sml/name-width 44)
 '(sml/replacer-regexp-list (quote (("^~/.org/" ":O:") ("^~/\\.emacs\\.d/" ":ED:"))))
 '(sml/shorten-directory t)
 '(sml/shorten-modes nil)
 '(sml/theme (quote respectful))
 '(wdired-allow-to-change-permissions t t)
 '(web-mode-enable-css-colorization t t)
 '(whitespace-line-column 80)
 '(whitespace-style (quote (face lines-tail)))
 '(yas-snippet-dirs (quote ("~/.emacs.d/snippets/snippets/"))))
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
