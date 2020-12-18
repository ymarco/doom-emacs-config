;;; ~/.config/doom/latex-config.el -*- lexical-binding: t; -*-

;;; Misc config

(setq
 ;; Dont auto-insert braces on _^, I have keybinds for that
 TeX-electric-sub-and-superscript nil
 ;; Just save, dont ask me
 TeX-save-query nil
 ;; Just cache, dont ask me
 preview-auto-cache-preamble t
 ;; Don't raise/lower super/subscripts
 font-latex-fontify-script nil)

(defadvice! prvt/dont-fold-brackets (start)
  :override #'TeX-find-macro-end-helper
  "Dont consider brackets proceeding a macro as its arguments. Described here:
URL `https://tex.stackexchange.com/questions/188287/auctex-folding-and-square-brackets-in-math-mode'"
  ;; Here I just copied over the definition of `TeX-find-macro-end-helper' and
  ;; deleted the bracket branch of the cond.
  (save-excursion
    (save-match-data
      (catch 'found
        (goto-char (1+ start))
        (if (zerop (skip-chars-forward "A-Za-z@"))
            (forward-char)
          (skip-chars-forward "*"))
        (while (not (eobp))
          (cond
           ;; DONT Skip over pairs of square brackets
           ;; Skip over pairs of curly braces
           ((or (looking-at "[ \t]*\n?{") ; Be conservative: Consider
                                        ; only consecutive lines.
                (and (looking-at (concat "[ \t]*" TeX-comment-start-regexp))
                     (save-excursion
                       (forward-line 1)
                       (looking-at "[ \t]*{"))))
            (goto-char (match-end 0))
            (goto-char (or (TeX-find-closing-brace)
                           ;; If we cannot find a regular end, use the
                           ;; next whitespace.
                           (save-excursion (skip-chars-forward "^ \t\n")
                                           (point)))))
           (t
            (throw 'found (point)))))
        ;; Make sure that this function does not return nil, even
        ;; when the above `while' loop is totally skipped. (bug#35638)
        (throw 'found (point))))))

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
   ("𝔼" ("EX"))
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
   ;; ("|{1}|" ("abs")) ; collides with the auto snippets and kicks me out
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


(defun +latex/dwim-at-point ()
  "TODO."
  (interactive)
  (let (preview-revealed? preview-unrevealed? tex-fold?)
    (dolist (ol (overlays-at (point)))
      ;; (message "%s %s" ol (overlay-get ol 'category))
      (pcase (overlay-get ol 'category)
        ('preview-overlay
         (if (overlay-get ol 'display)
             (setq preview-unrevealed? ol)
           (setq preview-revealed? ol)))
        ('TeX-fold
         (setq tex-fold? ol))))
    (cond
     (preview-unrevealed?
      (preview-toggle preview-unrevealed? nil))
     (tex-fold?
      (TeX-fold-show-item tex-fold?))
     ((or preview-revealed? (texmathp))
      (preview-at-point)))))


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
 `((,"\\\\[A-Za-z@*]+" 0 'font-lock-keyword-face prepend))
 'end)

(add-hook! 'doom-load-theme-hook
  (custom-set-faces!
    ;; On default, 1-depth braces don't stand out in LaTeX math
    `(rainbow-delimiters-depth-1-face :foreground nil :inherit rainbow-delimiters-depth-6-face)
    ;; Fixes latex preview background color in solaire
    `(preview-reference-face :inherit solaire-default-face)
    ;; Just configured for the theme:
    `(preview-face :background ,(doom-color 'base1))
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
(add-hook! 'TeX-mode-hook #'mixed-pitch-mode)


;; WIP mine and @itai33's plugin
(use-package! evil-tex
  :hook (LaTeX-mode . evil-tex-mode)
  :init
  (setq evil-tex-toggle-override-m nil)
  (setq evil-tex-toggle-override-t t)
  :config
  (map! :map evil-tex-toggle-map
        "p" (cmd!
             (cl-destructuring-bind (l . r)
                 (if (memq (char-before) '(?^ ?_))
                     '("{" . "}")
                   '("(" . ")"))
               (insert l)
               (forward-char)
               (insert r)
               (backward-char)
               (evil-insert 0)))
        :map evil-tex-mode-map
        :i "M-u" (cmd! (funcall-interactively #'sp-up-sexp)))
  (evil-tex-bind-to-env-map '(("s" . "english")
                              ("h" . "hebrew"))))

(defun prvt/eval-latex-with-calc (s)
  "Evaluate string S containing LaTeX code with `calc'.

The result is another string containing LaTeX code."
  (require 'calc)
  (calc-set-language 'latex)
  ;; TODO this is also a way
  (string-join (->> s
                ;; calc thinks \cdot is a variable instead of just
                ;; multiplying
                (replace-regexp-in-string "\\\\cdot\\>" "*")
                (math-read-exprs)
                (-map #'math-evaluate-expr)
                (-map #'math-format-value))
               ", ")
  ;; (calc-eval
  )

(evil-define-operator prvt/latex-eval-with-calc (beg end type arg)
  "Evaluate latex region as math and insert the result into the kill ring.

When given prefix argument, replace region with the result instead."
  :move-point nil
  (interactive "<R>P")
  (if (eq type 'block)
      (evil-apply-on-block #'prvt/latex-eval-with-calc beg end nil nil arg)
    (let ((res (prvt/eval-latex-with-calc
                (buffer-substring-no-properties beg end))))
      (if arg
          (save-excursion
            (kill-region beg end)
            (insert res))
        (message "=> %s" res)
        (kill-new res)))))

;; WIP mine and @tecosaur's plugin
(use-package! auto-activating-snippets
  :hook (LaTeX-mode . auto-activating-snippets-mode)
  :hook (org-mode . auto-activating-snippets-mode))

(use-package! latex-auto-activating-snippets
  :after latex
  :config
  (add-hook! 'LaTeX-mode-hook
    (add-hook! 'aas-post-snippet-expand-hook :local
               #'+latex-fold-last-macro-a))
  (defun +aas-expand-snippet-fn (&optional parens func)
    (interactive)
    (yas-expand-snippet (format "\\%s%s$1%s$0"
                                (or func aas-transient-snippet-key)
                                (or (car-safe parens) "(")
                                (or (cdr-safe parens) ")")))
    (laas--shut-up-smartparens))
  (aas-set-snippets
   'latex-mode
   :cond #'texmathp
   ;; not sure if this should be mainline
   "abs" (cmd! (+aas-expand-snippet-fn '("{" . "}")))
   "pn" "^n"
   "ivs" "^{-1}"
   "Span" (cmd! (+aas-expand-snippet-fn '("\\left( " . " \\right)")))
   ;; prob functions
   "Ber" #'+aas-expand-snippet-fn
   "Bin" #'+aas-expand-snippet-fn
   "Cov" #'+aas-expand-snippet-fn
   "EX" (cmd! (+aas-expand-snippet-fn '("[" . "]")))
   "exx" "e^x"
   "Geom" #'+aas-expand-snippet-fn
   "HyperGeom" #'+aas-expand-snippet-fn
   "NB" #'+aas-expand-snippet-fn
   "Poi" #'+aas-expand-snippet-fn
   "Rank" #'+aas-expand-snippet-fn
   "Uniform" #'+aas-expand-snippet-fn
   "Var" #'+aas-expand-snippet-fn
   "std" #'+aas-expand-snippet-fn
   "supp" "\\supp"
   ;; complexity
   "On" "O(n)"
   "O1" "O(1)"
   "Olog" "O(\\log n)"
   "Olon" "O(n \\log n)"
   "emx" "e^{-x}"))

(use-package xenops
  :hook (LaTeX-mode . xenops-mode)
  :config
  (map! :map xenops-mode-map
        :n "RET" #'xenops-dwim)
  (setq xenops-math-image-scale-factor 1.7
        xenops-cache-directory (concat doom-cache-dir "xenops/")))

;;; Keybinds

(map!
 :after tex :map LaTeX-mode-map
 :ei [C-return] #'LaTeX-insert-item
 :n "g r" #'prvt/latex-eval-with-calc
 :v "g r" #'prvt/latex-eval-with-calc

 ;; normal stuff here
 :localleader
 :desc "View" "v" #'TeX-view
 :desc "Preview" "p" #'preview-at-point
 :desc "Unpreview" "P" #'preview-clearout-at-point)

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
   (?.    ("\\cdot" "\\dots"))
   (?:    ("\\vdots" "\\ddots"))
   (?*    ("\\times" "\\star" "\\ast")))
 cdlatex-math-modify-alist
 '( ;; my own stuff
   ;;key  mathcmd   textcmd  type  rmdot(?)  it(?)
   (?/    "\\oner"  nil      t     nil       nil)
   (?h    "\\half"  nil      t     nil       nil)))

(set-popup-rule! "^\\*TeX \\(?:Help\\|errors\\)"
  :size 0.3 :select t :ttl nil)

(after! latex
  (TeX-add-style-hook
   "preamble"
   (lambda ()
     (TeX-run-style-hooks
      "amsmath"
      "amssymb"
      "amsthm"
      "breqn"
      "iftex"
      "macros"
      "mathtools"
      "polyglossia"
      "xcolor"))
   LaTeX-dialect))
