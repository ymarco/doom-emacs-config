;;; ~/.config/doom/imenu-list-config.el -*- lexical-binding: t; -*-

(use-package! imenu-list
  :defer t
  :init
  ;; TODO Future Plans:

  ;; - custom toggle function, so 'SPC o i' could be used to focus the
  ;;   imenu-list buffer and not only toggle, like in vterm
  ;; - 'Integration' with treemacs/neotree window: treemacs on upper left,
  ;;   imenu-list on lower left
  ;; - Update on doom-escape-hook? Do we need this when it also updates on file
  ;;   save?

  ;; FIXME creating a new workspace when the imenu-list buffer is focused causes
  ;; very weird behaviour
  (setq
   ;; the constant flickering annoys me. Let it update through the hooks.
   imenu-list-idle-update-delay 10000000
   ;; just to be consintant; treemacs & neotree focus after activation also.
   imenu-list-focus-after-activation t
   ;; just a tad lower than the default TODO normalize to treemacs & neotree
   ;; size, or propose a global doom sidebar size?
   imenu-list-size 0.25
   ;; That modeline is plain ugly. Treemacs & neotree don't have a modeline either.
   imenu-list-mode-line-format nil)

  (map!
   (:leader
     :desc "Toggle imenu-list" "oi" #'imenu-list-smart-toggle)
   :map imenu-list-major-mode-map
   :g "r"   #'imenu-list-refresh
   :g [tab] #'hs-toggle-hiding
   :n "gr"  #'imenu-list-refresh)

  (add-hook 'imenu-list-minor-mode-hook
            #'+imenu-list-set-update-hooks)
  :config
  ;; (set-popup-rule! (concat "^ "(regexp-quote imenu-list-buffer-name) "$")
  ;; :ignore t)
  ;; (after! winner
  ;;   (add-to-list 'winner-boring-buffers imenu-list-buffer-name))
  )

(defun +imenu-list-update-unless-popup ()
  "Call `imenu-list-update-safe' if not in a popup buffer."
  (unless (+popup-buffer-p)
    (imenu-list-update-safe)))

;;;###autoload
(defun +imenu-list-set-update-hooks ()
  "Set hooks to update imenu-list, instead of waiting for the idle timer."
  (if imenu-list-minor-mode
      ;; TODO also update on +lookup/definition?
      (progn
        (add-hook! '(doom-switch-buffer-hook
                     doom-switch-window-hook
                     after-save-hook) #'+imenu-list-update-unless-popup))
    (remove-hook! '(doom-switch-buffer-hook
                    doom-switch-window-hook
                    after-save-hook)  #'+imenu-list-update-unless-popup)))
