;; -*- no-byte-compile: t; -*-
;;; .doom.d/packages.el

;;; Examples:
;; (package! some-package)
;; (package! another-package :recipe (:local-repo "home/yoavm448/projects/repo"))
;; (package! builtin-package :disable t)

(package! xelb :pin "5970017d9b")
(package! nov :pin "cd1b264b3f")
(package! imenu-list :pin "46008738f8")

(package! webkit-katex-render :pin "97e2993bb1"
  :recipe (:host github :repo "fuxialexander/emacs-webkit-katex-render"))

(package! sxhkd-mode
  :recipe (:local-repo "/home/yoavm448/projects/sxhkd-mode"))

(package! hebrew-mode
  :recipe (:local-repo "/home/yoavm448/projects/hebrew-mode"))

(package! evil-tex
  :recipe (:local-repo "/home/yoavm448/projects/evil-tex"))

(package! pick-and-highlight
  :recipe (:local-repo "/home/yoavm448/projects/emacs-pick-and-highlight"))

(package! org-tex-fig
  :recipe (:local-repo "/home/yoavm448/projects/org-tex-fig"))
