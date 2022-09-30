;; -*- no-byte-compile: t; -*-
;;; .doom.d/packages.el

;; TODO when I finally embed inkscape inside emacs
;;(package! xelb :pin "5970017d9b")
;;(package! exwm  :pin "45ac28cc9cffe910c3b70979bc321a1a60e002ea")
;; ebook reading in emacs
;; (package! nov :pin "3d32aac0f895c8605e254c8b7f246537816744f5")
;; living on the edge here
(unpin! doom-snippets)
;; FIXME latex math previews using katex and webkit
;; (package! webkit-katex-render
;;   :recipe (:local-repo "packages/emacs-webkit-katex-render"
;;            :files ("*.el" "*.html")))
;; nicer sxhkd config file editing
;; Trying to avoid as many post-commnad hooks as possible
(package! evil-escape :disable t)
(package! org-superstar :disable t)
(package! org-appear :disable t)
(package! org-fancy-priorities :disable t)
;; When I finally get back to bspwm
;; (package! sxhkd-mode
;;   :recipe (:local-repo "packages/sxhkd-mode"))
;; Sane defaults to hebrew
(package! hebrew-mode
  :recipe (:host github
           :repo "yoavm448/hebrew-mode"))
;; evil enhancements for latex buffers, my and iyefrat's baby
(package! evil-tex
  :recipe (:local-repo "packages/evil-tex"))
;; WIP auto snippets for latex
(package! laas
  :recipe (:local-repo "packages/laas"))
(package! aas
  :recipe (:local-repo "packages/aas"))
;; calibre client from emacs, needs more configuration
(when (executable-find "calibredb")
  (package! calibredb
    :recipe (:host github :repo "chenyanming/calibredb.el")
    :pin "a27b71b727ebf726ed0742d968717e0e89db9786"))

(package! snapshot-timemachine :pin "99efcebab309b11ed512a8dc62555d3834df5efb")
;; WIP org export figures written in latex in both latex and html backend
;; (package! org-tex-fig
;;   :recipe (:local-repo "packages/org-tex-fig"))

(package! inkscape
  :recipe (:local-repo "packages/inkscape.el"
           :files ("*.el" "*.svg")))

;; (package! xenops ; not ready yet!
;;   :pin "2a72cf519bda25ee8852672e14b6c265388bc74b")
;; (package! vimgolf :pin "f565447ed2")
;; Fancy dictionaries
(package! lexic
  :recipe (:local-repo "packages/lexic"))
(package! explain-pause-mode :disable t)
(package! mu4e-msg :disable t)
(package! org-msg :disable t)
(package! mu4e-alert :disable t)

(when (executable-find "guix")
  (package! guix :pin "d246b2c443bbb9d49575b63bb78c14699c4448cf"))

;; (package! crdt
;;   :recipe (:repo "https://code.librehq.com/qhong/crdt.el"))
;; packages
;; (package! webkit
;;   :recipe (:host github :repo "akirakyle/emacs-webkit"
;;            :files ("*.el" "*.so" "*.js" "*.css")
;;            :build ("make")))
(package! tree-sitter)
(package! tree-sitter-langs)
(package! info-colors)                  ; FIXME
(package! meson-mode)
(package! all-the-icons
  :recipe (:local-repo "packages/all-the-icons"))
