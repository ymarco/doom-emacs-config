;; -*- no-byte-compile: t; -*-
;;; .doom.d/packages.el

;; TODO when I finally embed inkscape inside emacs
;;(package! xelb :pin "5970017d9b")
;; ebook reading in emacs
(package! nov :pin "cd1b264b3f")
;; living on the edge here
(unpin! doom-snippets)
;; FIXME latex math previews using katex and webkit
;; (package! webkit-katex-render
;;   :recipe (:local-repo "~/projects/emacs-webkit-katex-render"))
;; nicer sxhkd config file editing
;; Trying to avoid as many post-commnad hooks as possible
(package! evil-escape :disable t)
;; When I finally get back to bspwm
(package! sxhkd-mode
  :recipe (:local-repo "~/projects/sxhkd-mode"
           :no-byte-compile t))
;; Sane defaults to hebrew
(package! hebrew-mode
  :recipe (:host github
           :repo "yoavm448/hebrew-mode"))
;; WIP evil enhancements for latex buffers
(package! evil-tex
  :recipe (:local-repo "~/projects/evil-tex"
           :no-byte-compile t))
;; WIP auto snippets for latex
(package! auto-latex-snippets
  :recipe (:local-repo "~/projects/Auto-LaTeX-Snippets"
           :no-byte-compile t))

(when (executable-find "calibredb")
  (package! calibredb
    :recipe (:host github :repo "chenyanming/calibredb.el")))
;; WIP org export figures written in latex in both latex and html backend
;; (package! org-tex-fig
;;   :recipe (:local-repo "~/projects/org-tex-fig"))
;; I don't really know what I'm doing
(when (executable-find "guix")
  (package! guix))
;; (package! vimgolf :pin "f565447ed2")
