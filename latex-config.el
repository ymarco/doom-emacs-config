;;; ~/.config/doom/latex-config.el -*- lexical-binding: t; -*-
;; Notice that this contains no math config, that's on modules/lang/math

;;; Misc config

(setq
 ;; Dont auto-insert braces on _^
 TeX-electric-sub-and-superscript nil
 ;; Just save, dont ask me
 TeX-save-query nil
 ;; Just cache, dont ask me
 preview-auto-cache-preamble nil
 ;; Don't raise/lower super/subscripts
 font-latex-fontify-script nil)


;;; Aesthetics
(setq
 TeX-fold-math-spec-list
 `( ;; missing symbols
   ("≤" ("le"))
   ("≥" ("ge"))
   ("≠" ("ne"))
   ("‘{1}’" ("text"))
   ;; conviniance shorts
   ("‹" ("left"))
   ("›" ("right"))
   ("¡{1}" ("mathclap"))
   ("👻{1}" ("phantom"))
   ("⟋{1}" ("cancel"))
   ;; private macros
   ("ℝ" ("RR"))
   ("ℕ" ("NN"))
   ("Nₑ" ("NE"))
   ("Nₒ" ("NO"))

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

   ;; fancification
   ("{1}" ("mathrm"))
   (,(lambda (word) (string-offset-roman-chars 119743 word)) ("mathbf"))
   (,(lambda (word) (string-offset-roman-chars 119951 word)) ("mathcal"))
   (,(lambda (word) (string-offset-roman-chars 120003 word)) ("mathfrak"))
   (,(lambda (word) (string-offset-roman-chars 120055 word)) ("mathbb"))
   (,(lambda (word) (string-offset-roman-chars 120159 word)) ("mathsf"))
   (,(lambda (word) (string-offset-roman-chars 120367 word)) ("mathtt"))
   )
 TeX-fold-macro-spec-list ; thanks to @tecosaur
 '( ;; as the defaults
   ("[f]" ("footnote" "marginpar"))
   ("[c]" ("cite"))
   ("[l]" ("label"))
   ("[r]" ("ref" "pageref" "eqref"))
   ("[i]" ("index" "glossary"))
   ("..." ("dots"))
   ("{1}" ("emph" "textit" "textsl" "textmd" "textrm" "textsf" "texttt"
           "textbf" "textsc" "textup"))
   ;; tweaked defaults
   ("©" ("copyright"))
   ("®" ("textregistered"))
   ("™"  ("texttrademark"))
   ("[1]:||•" ("item"))
   ("❡ {1}" ("chapter" "chapter*"))
   ("❡❡ {1}" ("part" "part*"))
   ("§ {1}" ("section" "section*"))
   ("§§ {1}" ("subsection" "subsection*"))
   ("§§§ {1}" ("subsubsection" "subsubsection*"))
   ("¶ {1}" ("paragraph" "paragraph*"))
   ("¶¶ {1}" ("subparagraph" "subparagraph*"))
   ))

;; Kindly borrowed from @tecosaur
(defun string-offset-roman-chars (offset word)
  "Shift the codepoint of each charachter in WORD by OFFSET with an extra -6 shift if the letter is lowercase"
  (apply 'string
         (mapcar (lambda (c) (+ (if (>= c 97) (- c 6) c) offset)) word)))

;; Making \( \) less visible
(defface unimportant-latex-face
  '((t
     :inherit font-lock-comment-face))
  "Face used to make \\(\\), \\[\\] less visible."
  :group 'LaTeX-math)

(font-lock-add-keywords
 'latex-mode
 `((,(rx "\\" (any "()[]")) 0 'unimportant-latex-face prepend))
 'end)

;; Fontify anything that looks like a TeX macro, not limited to known commands
;; FIXME this also fontifies tex macros in comments, hadn't found a solution yet
(font-lock-add-keywords
 'latex-mode
 `((,"\\\\[[:word:]]+" 0 'font-lock-keyword-face prepend))
 'end)

(after! doom-themes
  (custom-set-faces!
    ;; On default, 1-depth braces don't stand out in LaTeX math
    `(rainbow-delimiters-depth-1-face :foreground nil :inherit rainbow-delimiters-depth-6-face)
    ;; Fixes latex preview background color in solaire
    `(preview-reference-face :inherit solaire-default-face)
    ;; Just configured for the theme:
    `(preview-face :background ,(doom-color 'base2))
    `(font-latex-doctex-documentation-face :background ,(doom-color 'base2))
    `(TeX-fold-folded-face :inherit font-lock-builtin-face)
    `(TeX-fold-unfolded-face
      ;; This should stand out from hl-line
      :background ,(doom-blend (doom-color 'base4) (doom-color 'bg) 0.3))))

;; Bigger compiled math cause it's pretty
(after! preview
  (setq-default preview-scale 1.8)
  (setq preview-scale 1.8))

(add-hook! 'TeX-mode-hook
           ;;(hl-todo-mode) ; TODO
           ;; Completion is 100% annoying me here, adds tons of lag and gives
           ;; terrible suggestions
           ;; TODO Debug those company backends, maybe fix completion with texlab
           (company-mode -1)
           ;; Flycheck with both chktex and lacheck both just bother me with
           ;; worthless advice all the time. when my HW doesn't compile I'll re-
           ;; enable them.
           (flycheck-mode -1))

;; mixed-pitch is pretty
(add-hook 'TeX-mode-hook #'mixed-pitch-mode)
;; WIP mine and @itai33's plugin
(add-hook 'TeX-mode-hook #'evil-tex-mode)

;;; Keybinds

(map!
 :after tex :map LaTeX-mode-map
 :ei [C-return] #'LaTeX-insert-item

 ;; normal stuff here
 :localleader
 :desc "View" "v" #'TeX-view)

(setq
 cdlatex-math-symbol-prefix ?\;
 cdlatex-math-symbol-alist
 '( ;; adding missing functions to 3rd level symbols
   ;;key   1st            2nd           3rd
   (?_    ("\\downarrow"  ""           "\\inf"))
   (?^    ("\\uparrow"    ""           "\\sup"))
   (?k    ("\\kappa"      ""           "\\ker"))
   (?m    ("\\mu"         ""           "\\lim"))
   (?d    ("\\delta"      "\\partial"  "\\dim"))
   (?D    ("\\Delta"      "\\nabla"    "\\deg"))
   ;; no idea why \Phi isnt on 'F' in first place, \phi is on 'f'.
   (?F    ("\\Phi"))
   ;; now just conveniance
   (?:    ("\\dots")))
 cdlatex-math-modify-alist
 '( ;; my own stuff
   ;;key  mathcmd   textcmd  type  rmdot(?)  it(?)
   (?/    "\\oner"  nil      t     nil       nil)
   (?h    "\\half"  nil      t     nil       nil)))

(set-popup-rule! "^\\*TeX \\(Help\\|errors\\)"
  :size 0.3 :ttl nil)
