;;; cfdg-mode.el --- major mode for context free art

;; Copyright (C) 2015 ZHOU Feng

;; Author: ZHOU Feng <zf.pascal@gmail.com>
;; URL: http://github.com/zweifisch/cfdg-mode
;; Version: 0.0.1
;; Created: 9th Jun 2015

;;; Commentary:
;;
;; major mode context free art
;;

;;; Code:

(defvar cfdg-mode-map nil)
(defvar cfdg-mode-syntax-table nil)
(defvar cfdg-mode-font-lock-defaults nil)

(setq cfdg-mode-map (make-sparse-keymap))
(define-key cfdg-mode-map (kbd "C-c C-c") 'cfdg-render)

(setq cfdg-mode-syntax-table
      (let ((st (make-syntax-table)))
        (modify-syntax-entry ?/ ". 124b" st)
        (modify-syntax-entry ?* ". 23" st)
        (modify-syntax-entry ?# "< b")
        (modify-syntax-entry ?\n "> b" st)
        st))

(setq cfdg-mode-font-lock-defaults
      `(("\\(shape\\|include\\|startshape\\|rule\\|background\\)[ \011]" 1 font-lock-keyword-face)
        ("include[ \011]+\\([a-zA-Z0-9\-_\.]+\\)" 1 font-lock-type-face)
        ("\\(CIRCLE\\|SQUARE\\|TRIANGLE\\)" 1 font-lock-builtin-face)
        ("\\(rule\\|startshape\\)[ \011]+\\([a-zA-Z0-9_]+\\)" 2 font-lock-function-name-face)
        ("\\(rule\\|startshape\\)[ \011]+\\([a-zA-Z0-9_]+\\)[ \011]+\\([0-9\.]+\\)" 3 font-lock-constant-face)
        ("\\([a-zA-Z0-9_]+\\)[ \011]*\\({\\|\\[\\)" 1 font-lock-function-name-face)
        ("\\(sat\\|size\\|rotate\\|flip\\|skew\\|hue\\|saturation\\|brightness\\|alpha\\|x\\|y\\|z\\|s\\|r\\|f\\|h\\|b\\|a\\)[ \011]+\\([-]*[0-9]+\\)" 1 font-lock-variable-name-face)
        ("\\([0-9]+[ \011]*\\*\\)[ \011]*{" 1 font-lock-constant-face)))

(define-derived-mode cfdg-mode fundamental-mode "CFDG"
  "major mode context free art"
  (setq font-lock-defaults '(cfdg-mode-font-lock-defaults)))

(defun cfdg-render ()
  (interactive)
  (let* ((size 500)
         (src (buffer-string))
         (tmp (make-temp-file "cfdg-"))
         (output (make-temp-file "cfdg-output-"))
         (cmd (format "cfdg -s %s %s %s" size tmp output)))
    (with-temp-file tmp (insert src))
    (shell-command-to-string cmd)
    (with-current-buffer (get-buffer-create "*cfdg-output*")
      (erase-buffer)
      (insert-image (create-image output)))
    (display-buffer "*cfdg-output*")))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.cfdg\\'" . cfdg-mode))

(provide 'cfdg-mode)
;;; cfdg-mode.el ends here
