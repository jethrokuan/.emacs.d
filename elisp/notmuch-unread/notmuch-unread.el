;;; notmuch-unread.el --- Display unread mail count in the mode line
;;; Package-Requires: ((notmuch "0.18"))
;;; Version: 0.1

;; Copyright (C) 2015  Tyler Earnest <tmearnest@gmail.com>
;; Copyright (C) 2014  David Thompson <davet@gnu.org>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This global minor mode alters the mode line to display the number
;; of messages with the "unread" tag in notmuch.

;;; Code:

(require 'all-the-icons)
(require 'notmuch)

(defvar notmuch-unread-mode-line-string nil
  "String to display in the mode line.")

(put 'notmuch-unread-mode-line-string 'risky-local-variable t)

(defvar notmuch-unread-update-timer nil
  "Timer for updating the mode line.")

(defcustom notmuch-unread-update-interval 5
  "The number of seconds to wait in between updates."
  :type 'integer
  :group 'notmuch-unread)

(defcustom notmuch-unread-search-term "tag:unread"
  "The search term to pass to notmuch count."
  :type 'string
  :group 'notmuch-unread)

(defcustom notmuch-unread-icon (all-the-icons-material "mail" :face 'all-the-icons-cyan-alt)
  "String to show on modeline"
  :type 'string
  :group 'notmuch-unread)

(defcustom notmuch-unread-icon-color-unread "#98d32f"
  "Color for unread mail"
  :type 'color
  :group 'notmuch-unread)

(defcustom notmuch-unread-icon-color-read "grey70"
  "Color for read mail"
  :type 'color
  :group 'notmuch-unread)

(defun notmuch-unread-count ()
  "Return the number of messages that match
`notmuch-unread-search-term`."
  (string-to-number
   (replace-regexp-in-string
    "\n" ""
    (notmuch-command-to-string "count" notmuch-unread-search-term))))

(defvar notmuch-unread-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map [mode-line mouse-1]
      (lambda ()
        (interactive)
        (notmuch-search notmuch-unread-search-term)))
    (define-key map [mode-line mouse-3]
      (lambda ()
        (interactive)
        (notmuch)))
    map)
  "Keymap to enable notmuch-search unread on click")

(defun notmuch-unread-update-handler ()
  "Update the mode line."
  (let* ((ct (notmuch-unread-count))
         (color (if (= ct 0)
                    notmuch-unread-icon-color-read
                  notmuch-unread-icon-color-unread)))
    (setq notmuch-unread-mode-line-string
          (propertize
           (format "%s %s"
                   notmuch-unread-icon
                   ct
                   )
           'face `(:foreground ,color)
           'mouse-face `(:foreground ,color :box (:line-width 1 :style released-button :color "grey75"))
           'help-echo (format "%d unread. Mouse-1 to show unread. Mouse-3 to open notmuch" ct)
           'keymap notmuch-unread-keymap)))
  (force-mode-line-update))

;;;###autoload
(define-minor-mode notmuch-unread-mode
  "Display unread mail count in the mode line"
  :global t
  :require 'notmuch
  (and notmuch-unread-update-timer
       (cancel-timer notmuch-unread-update-timer))
  (if notmuch-unread-mode
      (progn
        (setq global-mode-string (append (or global-mode-string '(""))
                                         '(notmuch-unread-mode-line-string)))
        (setq notmuch-unread-update-timer
              (run-at-time nil notmuch-unread-update-interval
                           'notmuch-unread-update-handler)))
    (setq global-mode-string
          (delq 'notmuch-unread-mode-line-string
                global-mode-string))))

(provide 'notmuch-unread)
;;; notmuch-unread.el ends here
