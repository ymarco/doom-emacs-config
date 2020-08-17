;;; screenshot.el -*- lexical-binding: t; -*-

(defun resize-include-text (start end &optional width frame)
  "Resize current frame to show only text between START and END.

If specified, set width to WIDTH pixels instead of the default 60.

If specified, used FRAME instead of the selected one."
  (evil-scroll-line-to-top nil)
  (set-frame-size
   (or frame (selected-frame))
   (or width (* (frame-char-width) 60))
   ;; Depending on font, we need a few more pixels not to cut the last line.
   (round (* (frame-char-height) (+ 0.5 (count-screen-lines start end))))
   'pixelwise))

(defun screenshot-region (start end &optional format)
  ;; (interactive (list (mark) (point) png svg pdf postscript))
  (interactive "r")
  (save-excursion
    (let* ((frame
            (make-frame '((name . "* Emacs Screenshot")
                          (minibuffer . nil)
                          (cursor nil)))
            )
           ;; (doom-font (font-spec :family "Source Code Pro"))
           (scroll-margin 0)
           (mode-line-format nil)
           (minibuffer-frame-alist '((height . 0)))
           ;; FIXME
           (evil-default-cursor nil)
           (evil-normal-state-cursor nil)
           (evil-emacs-state-cursor nil)
           (evil-insert-state-cursor nil)
           (evil-visual-state-cursor nil))
      (delete-other-windows)
      (resize-include-text start end nil frame)
      (redisplay)
      (sleep-for 0.2)
      (with-temp-file
          "/tmp/frame.png"
        (insert (x-export-frames frame 'png))
        ;; (call-process-region
        ;;  (x-export-frames frame 'png) nil
        ;;  "xclip"
        ;;  nil nil nil
        ;;  "-selection" "clipboard"
        ;;  "-target" "image/png")
        )
      (sleep-for 1)
      (delete-frame frame)
      )))
