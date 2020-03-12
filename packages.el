;; -*- no-byte-compile: t; -*-
;;; .doom.d/packages.el

;; TODO when I finally embed inkscape inside emacs
;;(package! xelb :pin "5970017d9b")
;; ebook reading in emacs
(package! nov :pin "cd1b264b3f")
;; sideline buffer navigator powered by imenu
(package! imenu-list :pin "46008738f8")
;; unpin doom-snippets, living on the edge here
(package! doom-snippets
  :recipe (:host github
                 :repo "hlissner/doom-snippets"
                 :files ("*.el" "*"))
  :pin nil)
;; FIXME latex math previews using katex and webkit
(package! webkit-katex-render
  :recipe (:local-repo "~/projects/emacs-webkit-katex-render"))
;; nicer sxhkd config file editing
(package! sxhkd-mode
  :recipe (:local-repo "~/projects/sxhkd-mode"))
;; sane defaults to hebrew
(package! hebrew-mode
  :recipe (:local-repo "~/projects/hebrew-mode"))
;; WIP evil enhancements for latex buffers
(package! evil-tex
  :recipe (:local-repo "~/projects/evil-tex"))
;; highlight areas for screenshots
(package! pick-and-highlight
  :recipe (:local-repo "~/projects/emacs-pick-and-highlight"))
;; WIP org export figures written in latex in both latex and html backend
(package! org-tex-fig
  :recipe (:local-repo "~/projects/org-tex-fig"))
