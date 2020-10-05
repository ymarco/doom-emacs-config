;; -*- no-byte-compile: t; -*-
;;; .doom.d/packages.el

;; TODO when I finally embed inkscape inside emacs
;;(package! xelb :pin "5970017d9b")
(package! exwm  :pin "45ac28cc9cffe910c3b70979bc321a1a60e002ea")
;; ebook reading in emacs
(package! nov :pin "3d32aac0f895c8605e254c8b7f246537816744f5")
;; living on the edge here
(unpin! doom-snippets)
;; FIXME latex math previews using katex and webkit
;; (package! webkit-katex-render
;;   :recipe (:local-repo "packages/emacs-webkit-katex-render"))
;; nicer sxhkd config file editing
;; Trying to avoid as many post-commnad hooks as possible
(package! evil-escape :disable t)
;; When I finally get back to bspwm
(package! sxhkd-mode
  :recipe (:local-repo "packages/sxhkd-mode"))
;; Sane defaults to hebrew
(package! hebrew-mode
  :recipe (:host github
           :repo "yoavm448/hebrew-mode"))
;; evil enhancements for latex buffers, my and iyefrat's baby
(package! evil-tex)
;; WIP auto snippets for latex
(package! latex-auto-activating-snippets
  :recipe (:local-repo "packages/Auto-LaTeX-Snippets"
           :no-byte-compile t))
(package! auto-activating-snippets
  :recipe (:local-repo "packages/auto-activating-snippets"
           :no-byte-compile t))
;; calibre client from emacs, needs more configuration
(when (executable-find "calibredb")
  (package! calibredb
    :recipe (:host github :repo "chenyanming/calibredb.el")
    :pin "a27b71b727ebf726ed0742d968717e0e89db9786"))
;; fixes an issue with compiled pdf-tools
(package! pdf-tools :built-in t)

(package! snapshot-timemachine :pin "99efcebab309b11ed512a8dc62555d3834df5efb")
;; WIP org export figures written in latex in both latex and html backend
;; (package! org-tex-fig
;;   :recipe (:local-repo "packages/org-tex-fig"))

(package! inkscape
  :recipe (:local-repo "packages/inkscape"))

(package! xenops ; not ready yet!
  :pin "78cbe16b74480ac6304865c9c3cfad36b5d49d1d")
;; (package! vimgolf :pin "f565447ed2")
;; Fancy dictionaries
(package! lexic
  :recipe (:host github :repo "tecosaur/lexic"
           :no-byte-compile t))
(package! explain-pause-mode :disable t)
