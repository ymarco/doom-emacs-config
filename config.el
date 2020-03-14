;;; .doom.d/config.el -*- lexical-binding: t; -*-

;; measure time!
(setq t0 (current-time))

;;; Misc package options
(setq
 ;; User config, used for templates mostly
 user-full-name "Yoav Marco"
 user-mail-address "yoavm448@gmail.com"
 user-login-name "yoavm448"

 ;; prettiness
 doom-theme 'doom-spacegrey
 ;; take new window space from all other windows (not just current)
 window-combination-resize t

 ;; Even more cololful pars
 rainbow-delimiters-max-face-count    4
 ;; Dired auto-detects multiple windows
 dired-dwim-target                    t
 ;; I don't need it to tell me its UTF-8
 doom-modeline-buffer-encoding nil
 ;; The unsaved icon made me notice and save the buffer on every stop
 doom-modeline-buffer-state-icon nil
 ;; Nested snippet expansion
 yas-triggers-in-field                t
 ;; Avy can jump through windows
 avy-all-windows                      t
 ;; Avy can auto-jump when theres 1 candidate
 avy-single-candidate-jump            t
 ;; e.g don't colorise the word 'white'
 rainbow-x-colors                     nil
 ;; Treemacs usually seems too wide to me
 treemacs-width 27
 ;; Leave my comments alone
 +evil-want-o/O-to-continue-comments  nil
 ;; Don't replace the current window when splitting
 evil-split-window-below  t
 evil-vsplit-window-right t
 ;; Wait for a bit longer before prompting me, lsp
 lsp-idle-delay 0.5
 lsp-ui-sideline-delay 0.5
 ;; js2 you too
 js2-idle-timer-delay 0.5
 ;; I have issues with variable-pitch fonts that appear larger than their
 ;; mixed-pitch brothers. Setting the font size would normally work, but it
 ;; doesn't with mixed-pitch-mode which I use. Fortunately, Source Sans Pro is
 ;; just in the perfect size for ubuntu-mono!
 doom-variable-pitch-font (font-spec :family "Source Sans Pro")
 ;; Just testing fonts
 ;;doom-font (font-spec :family "Source Code Pro" :size 18)
 projectile-project-search-path '("~/projects")
 abbrev-file-name (concat doom-private-dir "abbrevs.el"))


;; Show tags in org-brain
(add-hook 'org-brain-vis-current-title-append-functions #'org-brain-entry-tags-string)
;; Colorify colors in X config mode
(add-hook! 'conf-xdefaults-mode-hook (rainbow-mode 1))

(defconst prvt/raw-git-packages-dir
  (eval-when-compile
    (concat doom-local-dir "straight/repos"))
  "Directory for raw git packages, as cloned by straight.")


;; No more 'Starting new Ispell process aspell with default dictionary...done'
(advice-add #'ispell-init-process :around #'doom-shut-up-a)


(defun prvt/file-search-package ()
  (interactive)
  (doom-project-find-file prvt/raw-git-packages-dir))

(defun prvt/find-file-in-general-config ()
  "Find file in xdg-home directory"
  (interactive)
  (doom-project-find-file (or (getenv "XDG_CONFIG_HOME")
                              "~/.config")))

;;;###autoload
(defun prvt/snippets-newline-if-needed (&optional n)
  "return a newline if point not preceded by a newline.
with parameter N, insert up to N newlines.

Used mostly so I can type a snippet key on the same line and have
it start a new line of its own."
  (interactive)
  (doom-snippets-without-trigger
   (let* ((n (or n 1))
          (max-point (- (point) n))
          (nl-count-minus (save-excursion (skip-chars-backward "\n" max-point))))
     (make-string (+ n nl-count-minus) ?\n))))

;; some function complains about this being void, idk how but this fixes it.
(defun org-latex-link-override (&rest _))

;; Recenter instead of 'preserve position'
;; And yes, we are advising an advice here.
(advice-add #'doom-preserve-window-position-a :override
            (lambda (orig-fn &rest args)
              (apply orig-fn args)
              (doom-recenter-a)))


;; I like abbrevs, at least in org mode
(add-hook 'org-mode-hook #'abbrev-mode)
;; ALWAYS expand abbrevs
(setq evil-want-abbrev-expand-on-insert-exit t)


;; I don't know what breaks it but sometimes RET gets bounded to some other
;; stuff
(after! evil
  (map! :map org-mode-map
        :mn "RET" #'+org/dwim-at-point ))


(use-package! sxhkd-mode
  ;; Doom auto-configures a mode for sxhkd, using a slightly different key here
  ;; overrides it
  :mode "sxhkdrc\\'")

(use-package! pick-and-highlight
  :defer t
  :init
  (map!
   :nv "gh" #'pah-highlight)
  ;; Don't define keys for de-highlighting; just use doom-escape-key for that.
  :config
  (add-hook 'doom-escape-hook #'pah-dehighlight))

(auto-image-file-mode)

(use-package! nov
  :mode ("\\.epub\\'" . nov-mode)
  :init
  (setq nov-save-place-file (concat doom-etc-dir "nov-places")))


;;; Aesthetics

(after! rainbow-mode
  (defadvice! +rainbow-priority-over-hl-line (color &optional match)
    "Advice to make sure the colors made by rainbow-mode are
    not hidden by the current-line highlighting done by hl-line"
    :override 'rainbow-colorize-match
    ;; This just converts `rainbow-colorize-match' to use overlays instead of
    ;; faces - this way the overlay priority kicks in. Since hl-mode has
    ;; priority of -50, not specifying a priority here would mean priority=0
    ;; which is fine.
    (let ((match (or match 0)))
      (overlay-put
       (make-overlay (match-beginning match) (match-end match))
       'face
       `((:foreground ,(if (> 0.5 (rainbow-x-color-luminance color))
                           "white" "black"))
         (:background ,color))))))

;;; Load other configs
(load! "latex-config") ; this also loads cdlatex-config
(load! "hebrew-latex-config")
(load! "dvorak-config")
(load! "org-config")
(load! "imenu-list-config")
(load! "general-keys")

;;; Config performance measure
(let ((elapsed (float-time (time-subtract (current-time) t0))))
  ;; I don't wanna encase this whole file in "(let ((t0 ...)))"
  ;; so I just setq t0 at the start and unbind it here.
  (makunbound 't0)
  (message "spent %.3fs in config.el" elapsed))
