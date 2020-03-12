;;; lang/math/autoload/hebrew-math.el -*- lexical-binding: t; -*-
;;;###if (featurep! +hebrew)

(defun hebrew-math-backwards-till-math ()
  "Go backwards until reaching a math env"
  (interactive)
  (while (not (or (funcall math-mathp-fn)
                  (= (point) (point-min))))
    ;; only searching for \ beause all math commands start with a \ (well not
    ;; tex dollars but I don't use them.)
    (search-backward "\\")))

(defun hebrew-math-backwards-till-math-regular ()
  "Call `hebrew-math-backwards-till-math', and go back to normal (English) input method."
  (interactive)
  (hebrew-math-backwards-till-math)
  (hebrew-set-hebrew-input-method))

(defun hebrew-math-forward-exit-math ()
  "Go forward until exiting a math env"
  (while (and (funcall math-mathp-fn)
              (/= (point) (point-max)))
    (forward-char)))

(defun hebrew-math-forward-exit-math-regular ()
  "Call `hebrew-math-forward-exit-math', and go back to Hebrew input method."
  (interactive)
  (hebrew-math-forward-exit-math)
  (hebrew-set-hebrew-input-method))


(defun hebrew-math-enter-math ()
  "Enter inline math in a Hebrew paragraph in latex.
If already in math mode, exit it and go back to Hebrew."
  (interactive)
  (hebrew-set-regular-input-method)
  (if (funcall math-mathp-fn)
      (hebrew-math-forward-exit-math-regular)
    (doom-snippets-expand :name "hebrew-math")))

(defun hebrew-math-display-math-mode ()
  "Enters display math mode in latex. add newlines beforehand if needed."
  (interactive)
  (hebrew-set-regular-input-method)
  (doom-snippets-expand :name "hebrew-display-math"))

(defun hebrew-math-align-math-mode ()
  "Enters display math mode in latex."
  (interactive)
  (hebrew-set-regular-input-method)
  (doom-snippets-expand :name "hebrew-align-math"))

(defvar hebrew-math-mode-map
  (let ((hebrew-math-mode-map (make-sparse-keymap)))
    (map! :map hebrew-math-mode-map
          ;; commands to switch from Hebrew to English to Hebrew in math mode
          :envi "M-m" #'hebrew-math-enter-math
          :envi "M-M" #'hebrew-math-backwards-till-math-regular
          :envi "M-r" #'hebrew-math-display-math-mode
          :envi "M-R" #'hebrew-math-align-math-mode)

    hebrew-math-mode-map)
  "keymap for hebrew-math")

;;;###autoload
(define-minor-mode hebrew-math-mode
  "Minor mode for LaTeX-like math writing in Hebrew buffers."
  :keymap hebrew-math-mode-map
  (math-mode hebrew-math-mode)
  (when (require 'hebrew-mode nil t)
    (hebrew-mode hebrew-math-mode)))
