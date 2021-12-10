;;; testing/screenshot2.el -*- lexical-binding: t; -*-

(defvar screenshot--buffer (generate-new-buffer " *screenshot")
  "Buffer containing text content of the last screenshot")

(defvar screenshot-border-width 15
  "Frame border width used when taking screenshots.")

(defun screenshot--format-buffer (beg end)
  ;; include indentation if `beg' is where indentation starts
  (save-excursion
    (goto-char beg)
    (back-to-indentation)
    (when (= beg (point))
      (setq beg (line-beginning-position))))
  (let ((s (buffer-substring beg end)))
    (with-current-buffer screenshot--buffer
      (erase-buffer)
      (insert s)
      (indent-rigidly (point-min) (point-max)
                      (- (or (indent-rigidly--current-indentation
                              (point-min) (point-max))
                             0))))))

;;;###autoload
(defun screenshot (beg end &optional type)
  "Copy a screenshot of the selected region.
With prefix argument, use svg instead of png."
  (interactive (list (region-beginning) (region-end)
                     (if current-prefix-arg 'svg
                       'png)))
  (when (not beg)
    (setq beg (point-min)
          end (point-max)))
  (screenshot--format-buffer beg end)
  (let* ((frame (posframe-show
                 screenshot--buffer
                 :position (point-min)
                 :internal-border-width screenshot-border-width
                 :poshandler #'posframe-poshandler-point-bottom-left-corner
                 :hidehandler #'posframe-hide)))
    (redraw-frame frame)
    (with-temp-file (concat "/tmp/frame." (symbol-name type))
      (insert (x-export-frames frame type))
      (call-process-region
       (point-min) (point-max)
       "wl-copy"
       nil nil nil
       "--type" (pcase type
                  ('png "image/png")
                  ('svg "image/svg+xml")))
      (message "Copied!"))
    (posframe-hide screenshot--buffer)))
