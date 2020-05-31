;;; ~/.config/doom/native-comp-fixes.el -*- lexical-binding: t; -*-

;;;###autoload
(defadvice! fix-emacs28-idiotic-browse-url (url &rest args)
  :override #'browse-url
  (interactive (browse-url-interactive-arg "URL: "))
  (unless (called-interactively-p 'interactive)
    (setq args (or args (list browse-url-new-window-flag))))
  (when (and url-handler-mode
             (not (file-name-absolute-p url))
             (not (string-match "\\`[a-z]+:" url)))
    (setq url (expand-file-name url)))
  (let ((process-environment (copy-sequence process-environment))
	      (function (or (browse-url-select-handler url)
                      browse-url-browser-function))
	      ;; Ensure that `default-directory' exists and is readable (bug#6077).
	      (default-directory (or (unhandled-file-name-directory default-directory)
			                         (expand-file-name "~/"))))
    ;; When connected to various displays, be careful to use the display of
    ;; the currently selected frame, rather than the original start display,
    ;; which may not even exist any more.
    (if (stringp (frame-parameter nil 'display))
        (setenv "DISPLAY" (frame-parameter nil 'display)))
    (if (functionp function)                 ; !!!!!
        (apply function url args)
      (error "No suitable browser for URL %s" url))))


(defvar prvt/packages-to-native-compile
  '(all-the-icons
    all-the-icons-dired
    avy
    ;; centaur-tabs
    company-box
    company
    diredfl
    evil                                ; Remember to remove evil-commands.eln
    evil-mc
    flycheck
    flycheck-popup-tip
    flyspell-lazy
    helpful
    ivy
    swiper
    counsel
    swiper
    ivy-prescient
    ivy-rich
    js2-mode
    lispy
    org-mode
    org-superstar
    posframe
    prescient
    rainbow-delimiters
    rjsx-mode
    smartparens
    persp-mode
    projectile
    web-mode
    which-key
    ws-butler
    yasnippet)
  "TODO")

(defun prvt/ensure-native-comp-list ()
  (interactive)
  (require 'straight)
  (dolist (req prvt/packages-to-native-compile)
    (native-compile-async
     (concat straight-base-dir "straight/build/" (symbol-name req) "/")
     t t)))

;; FIXME set compilation-mode for native-compilation buffers
(after! comp
  (set-popup-rule!
    (regexp-opt (list comp-log-buffer-name comp-async-buffer-name))
    :actions
    (list
     #'compilation-mode
     #'+popup-display-buffer-stacked-side-window-fn)
    :ttl nil
    :vslot -1))
