;;; py-pyment.el --- Use pyment to generate the proper docstring.

;; Copyright (C) 2017, Manuel Kaufmann <humitos@gmail.com>

;; Modified by: Jethro Kuan <jethrokuan95@gmail.com>
;; Original Author: Manuel Kaufmann <humitos@gmail.com>
;; URL: https://github.com/humitos/py-cmd-buffer/py-pyment.el
;; Version: 0.1
;; Package-Requires: ((buftra "0.6"))

;;; Commentary:

;; Provides the `py-pyment.el' command, which uses the external
;; "pyment" tool to generate the proper docstrings.

;; To automatically apply when saving a python file, use the
;; following code:

;;   (add-hook 'python-mode-hook 'py-pyment-enable-on-save)

;; To customize the behaviour of "pyment" you can set the
;; py-pyment-options e.g.

;;   (setq py-pyment-options '("--output reST" "--convert"))

;; To disable this command at certain situation you can set
;; py-pyment-enabled to nil e.g.

;;  (setq py-pyment-enabled nil)

;;; Code:

(require 'buftra)

(defgroup py-pyment nil
  "Use pyment to beautify a Python buffer."
  :group 'convenience
  :prefix "py-pyment-")


(defcustom py-pyment-options nil
  "Options used for pyment.

Note that `--write' is used by default."
  :group 'py-pyment
  :type '(repeat (string :tag "option")))

(defcustom py-pyment-enabled t
  "Wheter or not run \"pyment\" command even if the hook is ran."
  :group 'py-pyment
  :type 'boolean)

(defun py-pyment--call-executable (errbuf file)
  (zerop (apply 'call-process "pyment" nil errbuf nil
                (append py-pyment-options `("--write", file)))))


;;;###autoload
(defun py-pyment-buffer ()
  "Uses the \"pyment\" tool to reformat the current buffer."
  (interactive)
  (if py-pyment-enabled
      (buftra--apply-executable-to-buffer "pyment"
                                          'py-pyment--call-executable
                                          nil
                                          "py"
                                          nil)))


;;;###autoload
(defun py-pyment-region ()
  "Uses the \"pyment\" tool to reformat the selected region."
  (interactive)
  (buftra--apply-executable-to-buffer "pyment"
                                      'py-pyment--call-executable
                                      t
                                      "py"
                                      nil))


;;;###autoload
(defun py-pyment-enable-on-save ()
  "Pre-save hook to be used before running pyment."
  (interactive)
  (add-hook 'before-save-hook 'py-pyment-buffer nil t))


;;;###autoload
(defun py-pyment-generate-docstring-at-point ()
  "Generates the docstring for the object at point."
  (interactive)
  (save-excursion
    (python-mark-defun)
    (py-pyment-region)))

(defvar py-pyment-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "C-c M-d") 'py-pyment-generate-docstring-at-point)
    m))


;;;###autoload
(define-minor-mode py-pyment-mode
  "Docstring generation for Python code."
  :init-value nil
  :lighter " pyment"
  :keymap py-pyment-mode-map)

(provide 'py-pyment)

;;; py-pyment.el ends here
