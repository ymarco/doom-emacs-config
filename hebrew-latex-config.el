;;; ~/.config/doom/hebrew-latex-config.el -*- lexical-binding: t; -*-
;; Notice that this contains no math config, that's on
;; modules/lang/math/autoload/+hebrew

;; yas doesnt know how to advanced indentation in Hebrew LaTex
(setq yas-indent-line 'fixed)
(setq hebrew-use-hebrew-spell-checking t)

(defun +hebrew-input-method-heuristic ()
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


(defun +hebrew-math-backwards-till-math ()
  "Go backwards until reaching a math env"
  (interactive)
  (while (not (or (texmathp)
                  (= (point) (point-min))))
    ;; only searching for \ beause all math commands start with a \ (well not
    ;; tex dollars but I don't use them.)
    (search-backward "\\")))

(defun +hebrew-math-backwards-till-math-regular ()
  "Call `+hebrew-math-backwards-till-math', and go back to normal (English) input method."
  (interactive)
  (+hebrew-math-backwards-till-math)
  (hebrew-set-regular-input-method))

(defun +hebrew-math-forward-exit-math ()
  "Go forward until exiting a math env"
  (while (and (texmathp)
              (/= (point) (point-max)))
    (forward-char)))

(defun +hebrew-math-forward-exit-math-regular ()
  "Call `+hebrew-math-forward-exit-math', and go back to Hebrew input method."
  (interactive)
  (+hebrew-math-forward-exit-math)
  (hebrew-set-hebrew-input-method))

(defun +hebrew-math-enter-math ()
  "Enter inline math in a Hebrew paragraph in latex.
If already in math mode, exit it and go back to Hebrew."
  (interactive)
  (if (texmathp)
      (+hebrew-math-forward-exit-math-regular)
    (hebrew-set-regular-input-method)
    (doom-snippets-expand :name "hebrew-math")))


(defun +heberw--snippets-newline-if-needed (&optional n)
  "return a newline if point not preceded by a newline.
with parameter N, insert up to N newlines.

Used mostly so I can type a snippet key on the same line and have
it start a new line of its own."
  (interactive)
  (let* ((n (or n 1))
         (max-point (- (point) n))
         (nl-count-minus (save-excursion (skip-chars-backward "\n" max-point))))
    (make-string (+ n nl-count-minus) ?\n)))


(defun +hebrew-math-display-math-mode ()
  "Enters display math mode in latex. add newlines beforehand if needed."
  (interactive)
  (insert (+heberw--snippets-newline-if-needed 2))
  (hebrew-set-regular-input-method)
  (doom-snippets-expand :name "hebrew-display-math"))

(defun +hebrew-math-align-math-mode ()
  "Enters display math mode in latex."
  (interactive)
  (insert (+heberw--snippets-newline-if-needed 2))
  (hebrew-set-regular-input-method)
  (doom-snippets-expand :name "align*"))


(use-package! tex
  :defer t
  :init
  ;; Remove "section"; compiled sections ruin the hebrew. (not in XeTeX, but I
  ;; still don't like them.)
  (setq preview-default-option-list '("displaymath" "floats" "graphics"
                                      "textmath" "footnotes"))
  (add-hook! 'TeX-mode-hook
    (add-hook! 'evil-insert-state-entry-hook :local
               #'+hebrew-input-method-heuristic))
  (add-hook 'TeX-mode-hook #'hebrew-mode)
  :config
  (map! :map LaTeX-mode-map
        ;; backspace alias
        :i "M-h"  (λ! (insert "\\"))
        ;; ^{} _{} aliases
        :iv "C-_" (λ! (doom-snippets-expand :name "subscript-braces"))
        :iv "C-^" (λ! (doom-snippets-expand :name "superscript-braces"))
        ;; ``'' aren't useful for Hebrew
        "\"" (lambda! (insert "\""))
        ;; commands to switch from Hebrew to English to Hebrew in math mode
        :envi "M-m" #'+hebrew-math-enter-math
        :envi "M-M" #'+hebrew-math-backwards-till-math-regular
        :envi "M-r" #'+hebrew-math-display-math-mode
        :envi "M-R" #'+hebrew-math-align-math-mode
        ;; Have SPC m c compile with xetex. That way, previews are generated
        ;; with the fast pdlatex and actual big compiles with XeTeX which gives
        ;; better output (it can have hyperref and stuff).
        :localleader
        :desc "compile with xetex" "c"
        (lambda! () (let ((TeX-engine 'xetex))
                      (basic-save-buffer)
                      (TeX-command "LatexMk" 'TeX-master-file)))))
