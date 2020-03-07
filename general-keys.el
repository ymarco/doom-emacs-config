;;; ~/.config/doom/general-keys.el -*- lexical-binding: t; -*-

;; These are general keybinds, not related to general.el in any way.

;; Things I wanna push upstream someday
(map!
 (:after lsp :map lsp-mode-map
   "M-RET"                            #'lsp-execute-code-action) ;; like in IntelliJ
 (:after treemacs :map treemacs-mode-map
   "+"                                #'treemacs-create-dir) ;; just like in dired

 :leader
   ;; :desc "Find file in ~/.config" "fc" #'prvt/find-file-in-general-config
   :desc "Rename buffer" "bR"          #'rename-buffer)

;; Private
(map!
 ;; General
 :n "g SPC" #'evil-avy-goto-word-1
 :n "ga" (λ!! #'what-cursor-position t)
 ;; Things I picked up from JetBrains IDEs
 :eni "C-/"   #'comment-line
 :v   "C-/"   #'comment-or-uncomment-region
 :nie "C-M-l" #'+format/buffer
 ;; Smartparens Navigation
 :nie "M-u"   #'sp-up-sexp ;; exit parenthesis
 :nie "M-U"   #'sp-backward-up-sexp ;; exit parenthesis backward
 :nie "M-n"   (λ! (sp-up-sexp) (sp-down-sexp)) ;; next parentheses on same level
 :nie "M-N"   (λ! (sp-backward-up-sexp) (sp-backward-down-sexp))) ;; opposite of M-n
