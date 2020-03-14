;;; ~/.doom.d/org-setup.el -*- lexical-binding: t; -*-
;; These are nicer defaults to LaTeX and HTML through org. It also configures minted for
;; src code syntex highlighting in LaTex

;; TODO organize this mess

(setq
 ;; Don't use the current theme for src blocks colorization in HTML, assume its
 ;; configured in css.
 org-html-htmlize-output-type 'css
 ;; try not to accidently do weird stuff in invisible regions
 org-catch-invisible-edits 'smart)

;; I LOVE PRETTY STUFF
(add-hook! 'org-mode-hook
           #'+org-pretty-mode
           #'mixed-pitch-mode)
(setq org-ellipsis " ▾")

;; Native LaTeX highlighting is slower but WAY prettier
(after! org
  (setq org-highlight-latex-and-related '(native script entities)))

;; Ultra-bold annoys me, and bold is annoying when theres too much of it.
;; Semi-bold is a lot prettier (at least when supported).
(custom-set-faces!
  '(outline-1 :weight bold :height 1.11)
  '(outline-2 :weight semi-bold :height 1.1)
  '(outline-3 :weight semi-bold :height 1.07)
  '(outline-4 :weight semi-bold :height 1.06)
  '(outline-5 :weight semi-bold :height 1.04)
  '(outline-6 :weight semi-bold :height 1.02)
  '(outline-8 :weight semi-bold)
  '(outline-9 :weight semi-bold))


;; TODO add pdf -> svg preview compilation route
;;(after! org
;;(add-to-list org-preview-latex-process-alist
;;'(pdf2svg
;;:programs ("pdflatex" "pdf2svg")
;;:description "pdf > svg"
;;:message "you need to install the programs: pdflatex and pdf2svg."
;;:image-input-type "pdf"
;;:image-output-type "svg"
;;;; :image-size-adjust (1.7 . 1.5) ; TODO
;;:latex-compiler ("pdflatex -interaction nonstopmode -output-directory %o %f")
;;:image-converter ("pdf2svg %f -n -b min -c %S -o %O"))))


;; TODO Use KaTeX instead of MathJax for html latex math, its faster. Taken from
;; https://amitp.blogspot.com/2019/02/emacs-org-mode-and-katex.html
(defvar prvt/org-html-katex-template
  "<link rel=\"stylesheet\" href=\"https://cdn.jsdelivr.net/npm/katex@0.10.0/dist/katex.min.css\" integrity=\"sha384-9eLZqc9ds8eNjO3TmqPeYcDj8n+Qfa4nuSiGYa6DjLNcv9BtN69ZIulL9+8CqC9Y\" crossorigin=\"anonymous\"/>
<script defer=\"defer\" src=\"https://cdn.jsdelivr.net/npm/katex@0.10.0/dist/katex.min.js\" integrity=\"sha384-K3vbOmF2BtaVai+Qk37uypf7VrgBubhQreNQe9aGsz9lB63dIFiQVlJbr92dw2Lx\" crossorigin=\"anonymous\"></script>
<script defer=\"defer\" src=\"https://cdn.jsdelivr.net/npm/katex@0.10.0/dist/contrib/auto-render.min.js\" integrity=\"sha384-kmZOZB5ObwgQnS/DuDg6TScgOiWWBiVt0plIRkZCmE6rDZGrEOQeHM5PcHi+nyqe\" crossorigin=\"anonymous\" onload=\"renderMathInElement(document.body);\"></script>"
  "Template for using KaTeX instead of MathJax")

;; FIXME this Actually disabled cause it breaks some stuff (like align blocks)
;;(setq org-html-mathjax-template prvt/org-html-katex-template)

(after! org
  ;; Minted config for much better syntax highlightig to src blocks.
  (setq org-latex-listings 'minted)
  ;; minted calls the pygmentize process and thus needs shell escaping
  (setq org-latex-pdf-process
        '("%latex -shell-escape -interaction nonstopmode -output-directory %o %f"
          "%latex -shell-escape -interaction nonstopmode -output-directory %o %f"
          "%latex -shell-escape -interaction nonstopmode -output-directory %o %f"))
  ;; NOTE I don't actually add minted to the package list, I'll just \usepackage it when needed

  ;;(setq org-latex-pdf-process
  ;;'("latexmk -f -pdf %f")) ; Doesn't work with pdflatex
  ;; Margins
  ;;(add-to-list 'org-latex-packages-alist '("a4paper,margin=1.1in" "geometry"))
  (setq
   ;; Add 'colorlinks' option to hyperrref, its much prettier
   org-latex-hyperref-template
   "\\hypersetup{
 pdfauthor={%a},
 pdftitle={%t},
 pdfkeywords={%k},
 pdfsubject={%d},
 pdfcreator={%c},
 pdflang={%L},
 colorlinks=true
}"
   ;; page break after TOC
   org-latex-toc-command "\\tableofcontents\n\\pagebreak\n\n"
   ;; In case I configured hyperref in a separate latex preamble
   ;;(setq org-latex-hyperref-template "")
   ))
