;; -*- no-byte-compile: t; -*-
;;; ~/.config/doom/math.el

(set-yas-minor-mode! 'math-mode)
(when  (featurep! +hebrew-math)
  (set-yas-minor-mode! 'hebrew-math-mode))
;; ???
(add-hook! (latex-mode org-mode) #'math-mode)
