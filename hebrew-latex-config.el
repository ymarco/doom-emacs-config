;;; ~/.config/doom/hebrew-latex-config.el -*- lexical-binding: t; -*-
;; Notice that this contains no math config, that's on modules/lang/hebrew-math

(defun prvt/hebrew-set-stuff ()
  "Set Hebrew stuff, used in a hook.
Some of that gets overridden, so we override it back on a hook."
  (interactive)
  ;; yas doesnt know how to indent in Hebrew LaTex,
  ;; disable it
  (setq yas-indent-line nil))


(use-package! hebrew-mode
  :init (setq hebrew-use-hebrew-spell-checking t)
  :hook (LaTeX-mode . hebrew-mode))

(use-package! tex
  :defer t
  :init
  ;; Remove "section"; compiled sections ruin the hebrew. (not in XeTeX, but I
  ;; still don't like them.)
  (setq preview-default-option-list '("displaymath" "floats" "graphics"
                                      "textmath" "footnotes"))
  (map! :map LaTeX-mode-map
        ;; have <SPC m c> compile with xetex. That way, previews are generated
        ;; with the fast pdlatex and actual big compiles with XeTeX which gives
        ;; better output (it can have hyperref and stuff).
        :localleader
        :desc "compile with xetex" "c"
        (lambda! () (let ((TeX-engine 'xetex))
                      (TeX-command "LaTeX" 'TeX-master-file)))))
