;;; ~/.config/doom/emacs-anywhere-config.el -*- lexical-binding: t; -*-

(defun markdown-window-p (window-title)
  "Decide if a window titled WINDOW-TITLE would accept markdown text"
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

;;;###autoload
(defun ea-popup-handler (_app-name window-title _x _y _w _h)
  "TODO"
  (let* ((gwidth (display-pixel-width))
         (gheight (display-pixel-height))
         (width (/ gwidth 3))
         (height (/ gheight 4)))
    (set-frame-size (selected-frame) width height t)
    ;; Center frame in droplike fasion
    (set-frame-position (selected-frame) (/ (- gwidth width) 2) 50))
  (push '((undecorated . t)) default-frame-alist)

  (set-frame-name (concat "Quick Edit ∷ " ea-app-name " — "
                          (truncate-string-to-width
                           (string-trim
                            (string-trim-right window-title
                                               (format "-[A-Za-z0-9 ]*%s" ea-app-name))
                            "[\s-]+" "[\s-]+")
                           45 nil nil "…")))

  ;; Set major mode
  (cond
   ((markdown-window-p window-title)
    (gfm-mode)
    ;; Discord and github don't like newlines
    (auto-fill-mode -1))
    (setq truncate-lines nil)
   (t
    (org-mode))) ; default major mode

  (when-let ((selection (gui-get-selection 'PRIMARY)))
    (insert selection))

  (set-transient-map (let ((keymap (make-sparse-keymap)))
                       (define-key keymap (kbd "DEL")   (lambda! (delete-region (point-min) (point-max))))
                       (define-key keymap (kbd "C-SPC") (lambda! (delete-region (point-min) (point-max))))
                       keymap))
  (message "Press DEL to clear")

  (evil-initialize-state 'insert) ; start in insert
  (emacs-anywhere-mode 1))
