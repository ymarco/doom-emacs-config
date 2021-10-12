;;; ~/.config/doom/latex-template.el -*- lexical-binding: t; -*-

(defun prvt/plist-get-override (plist override prop)
  (or
   (plist-get override prop)
   (plist-get plist prop)))

;;;###autoload
(defun prvt/get-latex-template-env (config override-config)
  "Get template snippet env by parsing CONFIG json file.

The OVERRIDE-CONFIG may override values from CONFIG.
If CONFIG or OVERRIDE-CONFIG don't exist or don't define some fields, use defaults.
NOTE: uses emacs27 json features. I'm too lazy to also implement this without it."
  (let ((config (when (file-readable-p config)
                  (with-temp-buffer
                    (insert-file-contents config)
                    (json-parse-buffer :object-type 'plist))))
        (override-config (when (file-readable-p override-config)
                           (with-temp-buffer
                             (insert-file-contents override-config)
                             (json-parse-buffer :object-type 'plist))))
        (filebname (file-name-base buffer-file-name))
        type number language author course title
        english-includes-hebrew)

    ;; Language
    (setq language (or (prvt/plist-get-override
                        config override-config :language)
                       "hebrew")
          english-includes-hebrew (prvt/plist-get-override
                                   config override-config :englishIncludesHebrew))

    ;; Type and number, based on filename
    (cond
     ((string-match "^hw\\([[:digit:]]+\\)$" filebname)
      (setq type "homework"
            number (match-string 1 filebname)))
     ((string-match (rx (or "lecture" "lec" "class" "lesson") (group (one-or-more digit)))
                    filebname)
      (setq type "lecture"
            number (match-string 1 filebname)))
     (t
      (setq type "unknown")))

    (setq author (or (prvt/plist-get-override config override-config :author)
                     "יואב מרקו")
          title (or (prvt/plist-get-override config override-config :title)
                    ;; Titleize filename
                    (string-join
                     (mapcar #'capitalize
                             ;; Replace -,_... with space
                             (split-string
                              (let (case-fold-search)
                                ;; Seperating lower from upper: hello|World
                                (replace-regexp-in-string
                                 "\\([[:lower:]]\\)\\([[:upper:]]\\)" "\\1 \\2"
                                 ;; Separating upper from upper-and-lower: HTTP|Server
                                 (replace-regexp-in-string "\\([[:upper:]]\\)\\([[:upper:]][0-9[:lower:]]\\)"
                                                           "\\1 \\2" filebname)))
                              "[^[:word:]0-9]+"
                              )) " " )))
    ;; trying to find author/title nested by hw/lecture and language
    (let ((type-kw (doom-keyword-intern type)))
      (dolist (startenv (list config override-config))
        (let ((tempenv startenv))
          (dolist (envname (list (doom-keyword-intern language)
                                 (doom-keyword-intern type)
                                 (doom-keyword-intern language)))
            (setq tempenv (or (plist-get tempenv envname) tempenv))
            (setq author (or (plist-get tempenv :author) author))
            (setq title (or (plist-get tempenv type-kw) title))))))
    ;; course
    (setq course (plist-get override-config :course))
    ;; format templates in title
    (setq title (replace-regexp-in-string "{course}" course title))
    (setq title (replace-regexp-in-string "{number}" number title))
    (list
     :language language
     :english-includes-hebrew english-includes-hebrew
     :author author
     :title title
     :type type)))

;; TODO have __english __hebrew snippets to force a language, currently its only
;; configured through defaults and config files.
