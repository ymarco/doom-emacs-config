;;; ~/.config/doom/latex-config.el -*- lexical-binding: t; -*-
;; Notice that this contains no math config, that's on modules/lang/math

(load! "cdlatex-config")

;;; Misc config
(defvar +latex-use-TeX-fold t
  "Use TeX fold in TeX-mode.
When set to non-nil, this adds a few hooks/advices to fold stuff.")

(setq
 ;; Dont auto-insert braces on _^
 TeX-electric-sub-and-superscript nil
 ;; Just save, dont ask me
 TeX-save-query nil
 ;; Just cache, dont ask me
 preview-auto-cache-preamble t
 ;; Don't raise/lower super/subscripts
 font-latex-fontify-script nil)


;;; Asthetics
(setq TeX-fold-math-spec-list
      '(;; missing symbols
        ("≤" ("le"))
        ("≥" ("ge"))
        ("≠" ("ne"))
        ;; conviniance shorts
        ("‹" ("left"))
        ("›" ("right"))
        ;; private macros
        ("ℝ" ("RR"))
        ("ℕ" ("NN"))
        ("ℚ" ("QQ"))
        ("ℤ" ("ZZ"))
        ("ℂ" ("CC"))
        ("𝔽" ("FF"))))

(load! "fontification")
(appendq! font-latex-match-math-command-keywords ; just adding my own macros as keywords
          '(("oner")
            ("half")
            ("pa")
            ("bra")
            ("bre")
            ("pba")
            ("bpa")
            ("abs")))

;; Making \( \) less visible
(defface unimportant-latex-face
  '((t
     :inherit font-lock-comment-face))
  "Face used to make \\(\\), \\[\\] less visible."
  :group 'LaTeX-math)

(font-lock-add-keywords
 'latex-mode
 `((,(rx (and "\\" (any "()[]"))) 0 'unimportant-latex-face prepend))
 'end)

(font-lock-add-keywords
 'latex-mode
 `((,"\\\\[[:word:]]+" 0 'font-lock-keyword-face prepend))
 'end)

(after! doom-themes
  (custom-set-faces!
    ;; on default, 1-depth braces don't stand out in latex math
    `(rainbow-delimiters-depth-1-face :foreground nil :inherit rainbow-delimiters-depth-6-face)
    ;; fixes latex preview background color in solaire
    `(preview-reference-face :inherit solaire-default-face)
    ;; Just configured for the theme:
    `(preview-face :background ,(doom-color 'base2))
    `(font-latex-doctex-documentation-face :background ,(doom-color 'base2))
    `(TeX-fold-folded-face :inherit font-lock-builtin-face)))

(add-hook! 'TeX-mode-hook :append
           ;; (hl-todo-mode) ; FIXME
           (setq preview-scale 1.8)) ; bigger compiled math cause it's beautiful

;;; Folding

(defadvice! prvt/TeX-fold-line-a (&rest _)
  "Auto-fold LaTeX macros after functions that typically insert them."
  :after #'cdlatex-math-symbol
  (progn
    (TeX-fold-region (line-beginning-position) (line-end-position))))

;;; Keybinds

(add-hook! 'TeX-mode-hook
  (setq-local company-idle-delay nil)) ; auto-complete is annoying here


(after! tex
  (map!
   :map LaTeX-mode-map
   :ei [C-return] #'LaTeX-insert-item

   ;; normal stuff here
   :localleader
   :desc "View" "v" #'TeX-view
   (:when +latex-use-TeX-fold
     :desc "Fold paragraph"     "f"   #'TeX-fold-paragraph
     :desc "Unfold paragraph"   "C-f" #'TeX-fold-clearout-paragraph
     :desc "Fold buffer"        "F"   #'TeX-fold-buffer
     :desc "Unfold buffer"      "C-F" #'TeX-fold-clearout-buffer)))
