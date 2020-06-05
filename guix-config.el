;;; ~/.config/doom/guix-config.el -*- lexical-binding: t; -*-

(use-package! guix
  :init
  (after! (:or dired geiser)
    (global-guix-prettify-mode))
  (map! :leader
        :prefix ("G" . "guix")
        :desc "Popup" "G" #'guix-popup
        :desc "Pull" "p" #'guix-pull))
