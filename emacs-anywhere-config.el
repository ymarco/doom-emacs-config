;;; ~/.config/doom/emacs-anywhere-config.el -*- lexical-binding: t; -*-

(defun markdown-window-p (window-title)
  "Judges from WINDOW-TITLE whether the current window likes markdown"
  (string-match-p (rx (or "Stack Exchange" "Stack Overflow"
                          "Pull Request" "Issue" "Discord"))
                  window-title))

(define-minor-mode emacs-anywhere-mode
  "To tweak the current buffer for some emacs-anywhere considerations"
  :init-value nil
  :keymap (list
           ;; Finish edit, but be smart in org mode
           (cons (kbd "C-c C-c") (lambda! (if (and (eq major-mode 'org-mode)
                                                   (org-in-src-block-p))
                                              (org-ctrl-c-ctrl-c)
                                            (delete-frame))))
           ;; Abort edit. emacs-anywhere saves the current edit for next time.
           (cons (kbd "C-c C-k") (lambda! (setq ea-on nil)
                                          (delete-frame))))
  (when emacs-anywhere-mode
    ;; disable tabs
    (when (bound-and-true-p centaur-tabs-mode)
      (centaur-tabs-local-mode t))))

(defun ea-popup-handler (_app-name window-title _x _y _w _h)
  "TODO"
  (interactive)
  (set-frame-size (selected-frame) 80 12)
  ;; Position frame to where the mouse is
  (let* ((mousepos (split-string
                    (shell-command-to-string
                     "xdotool getmouselocation | sed -E \"s/ screen:0 window:[^ ]*|x:|y://g\"")))
         (mouse-x (- (string-to-number (nth 0 mousepos)) 100))
         (mouse-y (- (string-to-number (nth 1 mousepos)) 50)))
    (set-frame-position (selected-frame) mouse-x mouse-y))

  (set-frame-name (concat "Quick Edit ∷ " ea-app-name " — "
                          (truncate-string-to-width
                           (string-trim
                            (string-trim-right window-title
                                               (format "-[A-Za-z0-9 ]*%s" ea-app-name))
                            "[\s-]+" "[\s-]+")
                           45 nil nil "…")))
  (message "window-title: %s" window-title)

  ;; Set major mode
  (cond
   ((markdown-window-p window-title)
    (gfm-mode))
   (t
    (org-mode))) ; default major mode

  ;; (when-let ((selection (gui-get-selection 'PRIMARY)))
  ;; (insert selection))

  (evil-insert-state) ; start in insert
  (emacs-anywhere-mode 1))

(add-hook 'ea-popup-hook 'ea-popup-handler)
