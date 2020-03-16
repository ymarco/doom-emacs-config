;; -*- no-byte-compile: t; -*-
;;; ~/.config/doom/math.el

(set-yas-minor-mode! '+math-mode)
(add-hook 'LaTeX-mode-hook #'+math-mode)
(when  (featurep! +hebrew)
  (set-yas-minor-mode! '+hebrew-math-mode))
