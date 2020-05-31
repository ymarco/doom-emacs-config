;;; ~/.config/doom/hebrew-latex-config.el -*- lexical-binding: t; -*-
;; Notice that this contains no math config, that's on
;; modules/lang/math/autoload/+hebrew

;; yas doesnt know how to indent in Hebrew LaTex,
;; disable it
(setq yas-indent-line 'fixed)
(setq hebrew-use-hebrew-spell-checking t)

(defun input-method-heuristic ()
  "Politely try to figure out if I would probably want this or
that input method."
  (save-excursion
    (cond
     ;; After a latin word or a backslash.
     ((re-search-backward "[A-Za-z\\\\]" (- (point) 1) t)
      (hebrew-set-regular-input-method))
     ;; Right after end of math, or after Hebrew letters
     ((re-search-backward "\\\\)\\|\\\\]\\|[א-ת]"
                          (- (point) 3) t)
      (hebrew-set-hebrew-input-method))
     ;; Inside math. Last search, as it's the heaviest
     ((texmathp)
      (hebrew-set-regular-input-method)))))

(use-package! tex
  :defer t
  :init
  ;; Remove "section"; compiled sections ruin the hebrew. (not in XeTeX, but I
  ;; still don't like them.)
  (setq preview-default-option-list '("displaymath" "floats" "graphics"
                                      "textmath" "footnotes"))
  (add-hook 'TeX-mode-hook #'+hebrew-math-mode)
  (add-hook! 'TeX-mode-hook
    (add-hook! 'evil-insert-state-entry-hook :local
               #'input-method-heuristic))
  :config
  (map! :map LaTeX-mode-map
        ;; ``'' aren't useful for Hebrew
        "\"" (lambda! (insert "\""))
        ;; Have SPC m c compile with xetex. That way, previews are generated
        ;; with the fast pdlatex and actual big compiles with XeTeX which gives
        ;; better output (it can have hyperref and stuff).
        :localleader
        :desc "compile with xetex" "c"
        (lambda! () (let ((TeX-engine 'xetex))
                      (TeX-command "LatexMk" 'TeX-master-file)))))
