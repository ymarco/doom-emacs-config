;;; .doom.d/config.el -*- lexical-binding: t; -*-

;; measure time!

(setq t0 (current-time))

(setq
 ;; User config, used for templates mostly
 user-full-name "Yoav Marco"
 user-mail-address "yoavm448@gmail.com"
 user-login-name "yoavm448"

 ;; Random collection of various package options
 ;; prettiness
 doom-theme 'doom-spacegrey
 ;; take new window space from all other windows (not just current)
 window-combination-resize t

 ;; seperate words also by CamelCase
 global-subword-mode t
 ;; Even more cololful pars
 rainbow-delimiters-max-face-count    4
 ;; Dired auto-detects multiple windows
 dired-dwim-target                    t
 ;; doom-expand-snippets -> %expand
 doom-snippets-enable-short-helpers   t
 ;; I don't need it to tell me its UTF-8
 doom-modeline-buffer-encoding nil
 ;; The unsaved icon made me notice and save the buffer on every stop
 doom-modeline-buffer-state-icon nil
 ;; Nested snippet expansion
 yas-triggers-in-field                nil
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
 +zen-text-scale 1                        ; Don't scale text on zen
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

;; Don't replace the current window when splitting
(setq
 evil-split-window-below  t
 evil-vsplit-window-right t)

(add-hook 'org-brain-vis-current-title-append-functions #'org-brain-entry-tags-string) ; Show tags in org-brain
;; Colorify colors in X config mode
(add-hook! 'conf-xdefaults-mode-hook (rainbow-mode 1))

(defconst prvt/raw-git-packages-dir
  (eval-when-compile
    (concat doom-local-dir "straight/repos"))
  "Directory for raw git packages, as cloned by straight.")

(defconst prvt/home-dir
  (eval-when-compile (getenv "HOME"))
  "User home directory")

;; TODO make this work just with md/org files
(defun prvt/file-search-package-doc ()
  (interactive)
  (doom-project-find-file prvt/raw-git-packages-dir))

(defun prvt/find-file-in-general-config ()
  "Find file in xdg-home directory"
  (interactive)
  (doom-project-find-file (or (getenv "XDG_CONFIG_HOME")
                              "~/.config")))

;;;###autoload
(defun prvt/snippets-newline-if-needed (&optional n)
  "Insert a newline if not perceeded by a newline.
with parameter N, insert up to N newlines."
  (interactive)
  (doom-snippets-without-trigger
   (let* ((n (or n 1))
          (max-point (- (point) n))
          (nl-count-minus (save-excursion (skip-chars-backward "\n" max-point))))
     (make-string (+ n nl-count-minus) ?\n))))

(set-eshell-alias! ; haven't been using these much tbh
 ;; "config" "/usr/bin/git --git-dir=$HOME/.dotfiles/ --work-tree=$HOME"
 "python" "python3 $*"
 "sai" "sudo apt install $*"
 "s" "sudo $*"
 "config" (format "git --git-dir=%s/.dotfiles/ --work-tree=%s $*"
                  prvt/home-dir prvt/home-dir))

;; I like abbrevs, at least in org mode:
(add-hook 'org-mode-hook #'abbrev-mode)
;; ALWAYS expand abbrevs
(setq evil-want-abbrev-expand-on-insert-exit t)

;; That ultra-bold annoys me, and bold is annoying when theres too much of it.
;; Semi-bold is a lot prettier (when supported).
(custom-set-faces!
  '(outline-1 :weight bold :height 1.11)
  '(outline-2 :weight semi-bold :height 1.1)
  '(outline-3 :weight semi-bold :height 1.07)
  '(outline-4 :weight semi-bold :height 1.06)
  '(outline-5 :weight semi-bold :height 1.04)
  '(outline-6 :weight semi-bold :height 1.02)
  '(outline-8 :weight semi-bold)
  '(outline-9 :weight semi-bold))

;; Doom auto-configures a mode for sxhkd, override it:
(use-package! sxhkd-mode
  :mode "sxhkdrc\\'")

(use-package! xelb
  :commands xcb:connect)

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

;; Shut up ispell
(advice-add #'ispell-init-process :around #'doom-shut-up-a)

(after! evil
  (map! :map org-mode-map
   :mn "RET" #'+org/dwim-at-point ))


(load! "latex-config") ; this also loads cdlatex-config
(load! "hebrew-latex-config")
(load! "dvorak-config")
(load! "org-setup")
(load! "imenu-list-config")
(load! "general-keys")


(let ((elapsed (float-time (time-subtract (current-time) t0))))
  ;; I don't wanna encase this whole file in "(let ((t0 ...)))"
  ;; so I just setq t0 at the start and unbind it here.
  (makunbound 't0)
  (message "spent %.3fs in config.el" elapsed))
