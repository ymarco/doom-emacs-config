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


;;; Aesthetics
(setq TeX-fold-math-spec-list
      '( ;; missing symbols
        ("≤" ("le"))
        ("≥" ("ge"))
        ("≠" ("ne"))
        ("‘{1}’" ("text"))
        ;; conviniance shorts
        ("‹" ("left"))
        ("›" ("right"))
        ;; private macros
        ("ℝ" ("RR"))
        ("ℕ" ("NN"))
        ("ℚ" ("QQ"))
        ("ℤ" ("ZZ"))
        ("ℂ" ("CC"))
        ("𝔽" ("FF"))
        ("ℝ→ℝ" ("rtr"))
        ("I→ℝ" ("itr"))
        ("½" ("halff"))
        ("({1})" ("pa"))
        ("({1}]" ("pba"))
        ("[{1})" ("bpa"))
        ("[{1}]" ("bra"))
        ("|{1}|" ("abs"))
        ("❴{1}❵" ("bre" "set")) ; explicitly using bold unicode braces
        ("⌊{1}⌋" ("floor"))
        ("⌈{1}⌉" ("ceil"))
        ("‖{1}‖" ("norm"))
        ("❬{1}❭" ("anb")) ; explititly using narrower unicode angle brackets
        ))

(load! "fontification")
(appendq! font-latex-match-math-command-keywords ; just adding my own macros as keywords
          '("oner" "half" "pa" "bra" "bre" "pba" "bpa" "abs"))

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
    ;; on default, 1-depth braces don't stand out in LaTeX math
    `(rainbow-delimiters-depth-1-face :foreground nil :inherit rainbow-delimiters-depth-6-face)
    ;; fixes latex preview background color in solaire
    `(preview-reference-face :inherit solaire-default-face)
    ;; Just configured for the theme:
    `(preview-face :background ,(doom-color 'base2))
    `(font-latex-doctex-documentation-face :background ,(doom-color 'base2))
    `(TeX-fold-folded-face :inherit font-lock-builtin-face)
    ;; This should stand out from hl-line
    `(TeX-fold-unfolded-face
      :background ,(doom-blend (doom-color 'base4) (doom-color 'bg) 0.3))))

(add-hook! 'TeX-mode-hook :append
           ;; (hl-todo-mode) ; FIXME
           (setq preview-scale 1.8)) ; bigger compiled math cause it's beautiful

;;; Folding

;; Fold after cdlatex and snippets.
(defun +TeX-fold-line-ah (&rest _)
  "Auto-fold LaTeX macros after functions that typically insert them."
  (TeX-fold-region (line-beginning-position) (line-end-position)))

(when +latex-use-TeX-fold
  ;; TODO is TeX-fold-item enough here?
  (advice-add #'cdlatex-math-symbol :after #'TeX-fold-item)
  (advice-add #'cdlatex-math-modify :after #'TeX-fold-item)
  ;; local after-snippet hook for folding, but only in TeX buffers
  (add-hook! 'TeX-mode-hook
    (add-hook 'yas-after-exit-snippet-hook #'+TeX-fold-line-ah nil t))

  (add-hook! 'mixed-pitch-mode-hook
    (when mixed-pitch-mode
      (let ((var-pitch (face-attribute 'variable-pitch :family))
            (var-height (face-attribute 'variable-pitch :height)))
        (add-to-list 'mixed-pitch-fixed-cookie
                     (face-remap-add-relative
                      'TeX-fold-folded-face :family var-pitch :height var-height))))))


;;; Keybinds

(add-hook! 'TeX-mode-hook
           ;; auto-complete is annoying here
           (setq-local company-idle-delay nil))


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
