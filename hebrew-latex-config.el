;;; ~/.config/doom/hebrew-latex-config.el -*- lexical-binding: t; -*-

;; yas doesnt know how to advanced indentation in Hebrew LaTex
(setq yas-indent-line 'fixed)

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

(defun +hebrew-temporarily-disable-input-method ()
  (hebrew-set-hebrew-input-method)
  (remove-hook 'yas-after-exit-snippet-hook #'+hebrew-temporarily-disable-input-method))

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
  (insert (+heberw--snippets-newline-if-needed 1))
  (hebrew-set-regular-input-method)
  (doom-snippets-expand :name "hebrew-display-math"))

(defun +hebrew-math-align-math-mode ()
  "Enters display math mode in latex."
  (interactive)
  (insert (+heberw--snippets-newline-if-needed 1))
  (insert "‎")                           ; LTR char: always align to left
  (hebrew-set-regular-input-method)
  (doom-snippets-expand :name "align*"))

(defun +hebrew-bolp? ()
  "Like `doom-snippets-bolp' but ignores RTL/LTR marks."
  (or (bolp)
      (save-excursion
        (if (region-active-p)
            (goto-char (region-beginning))
          (unless (memq (char-before) (list ?\  ?\n))
            (backward-word)))
        (skip-chars-backward " \t‏‎")     ; note the rtl/ltr marks
        (bolp))))

(defvar +hebrew-amsthm-env-snippet
  (with-temp-buffer
    (insert-file-contents "~/.config/doom/snippets/latex-mode/hebrew-amsmath-env-template")
    (buffer-substring (point-min) (point-max)))
  "Template for an amsmath env snippet, ready to be used with
`format' to substitute the env name.")

(defun +hebrew-expand-amsmath-env-snippet (env)
  (yas-expand-snippet (format +hebrew-amsthm-env-snippet env env)))
(add-hook! 'hebrew-mode-hook
  (setq-local bidi-inhibit-bpa nil))

(use-package! tex
  :defer t
  :init
  ;; Remove "section"; compiled sections ruin the hebrew. (not in XeTeX, but I
  ;; still don't like them.)
  (setq preview-default-option-list '("displaymath" "floats" "graphics"
                                      "textmath" "footnotes"))
  (add-hook! 'TeX-mode-hook
    (add-hook 'evil-insert-state-entry-hook
              #'+hebrew-input-method-heuristic
              nil t))
  (add-hook 'TeX-mode-hook #'hebrew-mode)
  :config
  (map! :map LaTeX-mode-map
        ;; backspace alias
        :i "M-h"  (λ! (insert "\\"))
        ;; ^{} _{} aliases
        :iv "C-_" (λ! (doom-snippets-expand :name "subscript-braces"))
        :iv "C-^" (λ! (doom-snippets-expand :name "superscript-braces"))
        ;; ``'' aren't useful for Hebrew
        "\"" (cmd! (insert "\""))
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
        (cmd! (let ((TeX-engine 'xetex))
                (basic-save-buffer)
                (TeX-command "LatexMk" 'TeX-master-file)))))


(after! aas
  (aas-set-snippets
   'latex-mode
   :cond (lambda () (and (bolp) (not (texmathp))))
   "שחד" (cmd! (doom-snippets-expand :uuid "empty-section"))
   "סחד" (cmd! (doom-snippets-expand :uuid "empty-subsection"))))

(defun prvt/process-lyx-tex ()
  (interactive)
  (with-temp-buffer
    ;; TODO https://unix.stackexchange.com/a/26289/333391
    (insert-file-contents "hw07.tex")
    ;; delete preamble
    (search-forward "\\begin{document}")
    (goto-char (match-beginning 0))
    (delete-region (point-min) (point))
    (goto-char (point-min))
    (insert "\
\\documentclass{article}
\\usepackage{xargs}
\\usepackage{preamble}
\\usepackage{cancel}
\\usepackage{wasysym}
")

    (write-region (point-min) (point-max) "hw07-processed.tex")))

(defun prvt/lyx-tex-remove-Ls ()
  (interactive)
  (goto-char (point-min))
  (while (search-forward "\\L{" nil t)
    (let ((start (make-overlay (match-beginning 0) (match-end 0)))
          end)
      (goto-char (1- (match-end 0))) ;on {
      (forward-sexp)
      (setq end (make-overlay (1- (point)) (point)))

      (delete-region (overlay-start start) (overlay-end start))
      (delete-region (overlay-start end) (overlay-end end))
      (delete-overlay start)
      (delete-overlay end))))
