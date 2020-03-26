;;; ~/.config/doom/hebrew-latex-config.el -*- lexical-binding: t; -*-
;; Notice that this contains no math config, that's on modules/lang/hebrew-math

;; yas doesnt know how to indent in Hebrew LaTex,
;; disable it
(setq yas-indent-line nil)
(setq hebrew-use-hebrew-spell-checking t)

(use-package! tex
  :defer t
  :init
  ;; Remove "section"; compiled sections ruin the hebrew. (not in XeTeX, but I
  ;; still don't like them.)
  (setq preview-default-option-list '("displaymath" "floats" "graphics"
                                      "textmath" "footnotes"))
  (add-hook 'TeX-mode-hook #'+hebrew-math-mode)
  (map! :map LaTeX-mode-map
        ;; have <SPC m c> compile with xetex. That way, previews are generated
        ;; with the fast pdlatex and actual big compiles with XeTeX which gives
        ;; better output (it can have hyperref and stuff).
        :localleader
        :desc "compile with xetex" "c"
        (lambda! () (let ((TeX-engine 'xetex))
                      (TeX-command "LaTeX" 'TeX-master-file)))))
