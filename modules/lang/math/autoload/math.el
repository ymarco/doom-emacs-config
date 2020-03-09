;;; math/autoload.el -*- lexical-binding: t; -*-

(defvar math-preview-fn (lambda ()
                          (message "Math: no preview function selected"))
  "Function to call to preview math")
(defvar math-mathp-fn (lambda () (message "Math: no mathp functcion selected"))
  "Function to check if point is inside a math env")

(defvar math-mode-map
  (let ((math-mode-map (make-sparse-keymap)))
    (map! :map math-mode-map
          ;; backspace alias, the best thing ever
          :i "M-h"  (λ! (insert "\\"))
          ;; ^{} _{} aliases
          :iv "C-_" (λ! (doom-snippets-expand :name "subscript-braces"))
          :iv "C-^" (λ! (doom-snippets-expand :name "superscript-braces"))

          :localleader
          :desc "Preview math" "p" (λ! (funcall math-preview-fn)))

    math-mode-map)
  "TODO")

;;;###autoload
(define-minor-mode math-mode
  "Minor mode for LaTeX-like math writing."
  :keymap math-mode-map
  (setq math-mathp-fn (pcase major-mode
                        ('latex-mode #'texmathp)
                        ;; No reason for me to be in a LaTeX fragment unless mathing
                        ('org-mode   #'org-inside-LaTeX-fragment-p)))
  (setq math-preview-fn (pcase major-mode
                          ('latex-mode #'preview-at-point)
                          ('org-mode   #'org-latex-preview))))
