;;; .doom.d/config.el -*- lexical-binding: t; -*-

;; measure time! see end of file for explanation
(setq t0 (current-time))

;;; Misc package options
(setq
 gc-cons-threshold 67108864 ; 64mb
 gc-cons-percentage 0.2
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
 +modeline-encoding nil ; same for (modeline +light)
 ;; No line numbers. The ones on the modeline are enough for me.
 display-line-numbers-type nil
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
 treemacs-width 30
 ;; Leave my comments alone
 +evil-want-o/O-to-continue-comments  nil
 ;; Don't replace the current window when splitting
 evil-split-window-below  t
 evil-vsplit-window-right t
 ;; This way I use evil-snipe instead of evil-easymotion. I find
 ;; searching-then-repeating-search-until-done faster then
 ;; searching-then-typing-the-displayed-letter-pair
 evil-snipe-scope 'visible
 ;; Wait for a bit longer before prompting me, lsp
 lsp-idle-delay 0.5
 lsp-ui-sideline-delay 0.5
 ;; js2 you too
 js2-idle-timer-delay 0.5
 ;; Fix inconsistency with tab width in JSON files
 json-reformat:indent-width 2
 ;; I have issues with variable-pitch fonts that appear larger than their
 ;; mixed-pitch brothers. Setting the font size would normally work, but it
 ;; doesn't with mixed-pitch-mode which I use. Fortunately, Source Sans Pro is
 ;; just in the perfect size for ubuntu-mono!
 ;; EDIT: you can do that by setting 'height property, I've read the code. But
 ;; source sans pro is so pretty!
 doom-variable-pitch-font (font-spec :family "Source Sans Pro")
 doom-serif-font (font-spec :family "Bitstream Charter")
 projectile-project-search-path '("~/projects")
 abbrev-file-name (concat doom-private-dir "abbrevs.el")
 org-roam-directory "~/org/roam/"
 )


;; Don't blink the cursor, it's too distracting.
(blink-cursor-mode -1)
(setq visible-cursor nil)

;; I hate these vertical scrolls
(setq truncate-lines nil)
(setq-default truncate-lines nil)
;; Show tags in org-brain
(add-hook 'org-brain-vis-current-title-append-functions #'org-brain-entry-tags-string)
;; Colorify colors in X config mode
(add-hook! 'conf-xdefaults-mode-hook (rainbow-mode 1))
;; When I have something witty to say about mixed-pitch, this comment would be
;; replaced
(add-hook! 'markdown-mode-hook #'mixed-pitch-mode)

(defconst prvt/raw-git-packages-dir
  (eval-when-compile
    (concat doom-local-dir "straight/repos"))
  "Directory for raw git packages, as cloned by straight.")


;; No more 'Starting new Ispell process aspell with default dictionary...done'
;; (defadvice! ispell-shut-up-a ()
;;   :around #'ispell-init-process
;;   (cl-letf (((symbol-function 'progress-reporter-done) #'ignore))
;;     (ispell-init-process)))


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

(dolist (fn '(evil-visualstar/begin-search-forward
              evil-visualstar/begin-search-backward
              evil-ex-search-word-forward
              evil-ex-search-word-backward
              evil-ex-search-next
              evil-ex-search-previous
              evil-ex-search-forward
              evil-ex-search-backward))
  (advice-add fn :after #'doom-recenter-a))


;; I like abbrevs, at least in org mode
(add-hook 'org-mode-hook #'abbrev-mode)
;; ALWAYS expand abbrevs
(setq evil-want-abbrev-expand-on-insert-exit t)

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

(setq-hook! 'emacs-lisp-mode-hook
  tab-width 4)


(evil-define-command +evil:drag-file (force-p)
  "Open a drag window with dragon for current file.
Without FORCE-P, if the file is tex/org, open the pdf. "
  (interactive "<!>")
  (start-process "dragon-from-emacs"
                 nil
                 "dragon"
                 (cond
                  (force-p
                   buffer-file-name)
                  ((and (or "org" "tex")
                        (file-exists-p
                         (concat (file-name-sans-extension buffer-file-name) ".pdf"))
                        (concat (file-name-sans-extension buffer-file-name) ".pdf")))
                  (t
                   buffer-file-name))
                 "-x"))
(evil-ex-define-cmd "drag" #'+evil:drag-file)

;;; Keybinds

;; Things I wanna push upstream someday
(map!
 (:after treemacs :map treemacs-mode-map
   "+" #'treemacs-create-dir) ; just like in dired
 :leader
 :desc "Rename buffer" "bR" #'rename-buffer)

;; Private
(map!
 ;; General
 :n "g SPC"   #'evil-avy-goto-word-1
 :n "ga" (λ!! #'what-cursor-position t)
 :v "="       #'er/expand-region
 :v "-"       #'er/contract-region
 ;; $ is way too inconvenient, and I barely use marks
 :n "m" #'evil-end-of-line
 ;; to replace the lost m. I never use regisetrs, and we have SPC i y anyway.
 :n "\"" #'evil-set-marker
 ;; More conviniant surround operators
 :n "R" (λ!! #'evil-surround-edit nil)
 ;; Things I picked up from JetBrains IDEs
 :ni "C-/"   #'comment-line
 :v   "C-/"   #'comment-or-uncomment-region
 :ni "C-M-l" #'+format/buffer
 (:after lsp :map lsp-mode-map
   "M-RET"    #'lsp-execute-code-action)
 ;; Old TAB behavior that was removed in b8a3cad295
 :n [tab] (general-predicate-dispatch nil
            (and (featurep! :editor fold)
                 (save-excursion (end-of-line) (invisible-p (point))))
            #'+fold/toggle
            (fboundp 'evil-jump-item)
            #'evil-jump-item)
 :v [tab] (general-predicate-dispatch nil
            (and (bound-and-true-p yas-minor-mode)
                 (or (eq evil-visual-selection 'line)
                     (not (memq (char-after) (list ?\( ?\[ ?\{ ?\} ?\] ?\))))))
            #'yas-insert-snippet
            (fboundp 'evil-jump-item)
            #'evil-jump-item)
 ;; Smartparens Navigation
 :ni "M-u"   #'sp-up-sexp ; exit parenthesis
 :ni "M-U"   #'sp-backward-up-sexp ; exit parenthesis backward
 :ni "M-n"   (λ! (sp-up-sexp) (sp-down-sexp)) ; next parentheses on same level
 :ni "M-N"   (λ! (sp-backward-up-sexp) (sp-backward-down-sexp))) ; opposite of M-n

;; Company, don't block my snippet expansion
(define-key! company-active-map
  "TAB" nil
  [tab] nil)

(defvar prvt/omitted-extensions '(".aux" ".bbl" ".blg" ".brf" ".dpth" ".auxlock"
                                  ".log" ".out" ".synctex.gz" "dvi" "md5")
  "Extensions that I never wanna see.")
;; Don't show latex junk files in dired
(after! dired-x
  (setq dired-omit-extensions (append dired-omit-extensions
                                      prvt/omitted-extensions)))

(setq counsel-find-file-ignore-regexp (concat "\\(?:^[#.]\\|?:[#~]$\\|?:^Icon?\\)"
                                              (regexp-opt prvt/omitted-extensions)))

;; Let me see syntax highlighting under #ifndef which amount to false
(custom-set-faces!
  '(font-lock-preprocessor-face :foreground nil))

;; See autoload/emacs-anywhere-config
(add-hook 'ea-popup-hook 'ea-popup-handler)
;; See autoload/latex-template
(set-file-template!
  "/.*\\.tex$"
  :mode 'latex-mode
  ;; When we let file-templates trigger the snippet it errors out, so I'm
  ;; triggering manually here
  :trigger (lambda ()
             (evil-initialize-state 'insert) ; start in insert
             (doom-snippets-expand :uuid "__")))

;; (set-fontset-font t 'unicode (font-spec :family "Font Awesome"))

(defadvice! prvt/dont-confirm-kill-on-restart-a (orig-fn &rest args)
  "An advice to make emacs not ask for killing confirmration when using SPC q r."
  :around #'doom/restart-and-restore
  (let ((confirm-kill-emacs nil))
    (apply orig-fn args)))

;; No window borders (doesn't work in daemon)
(setq initial-frame-alist '((undecorated . t)))

;; Set fringes for solaire, experimental in emacs27
(after! solaire-mode
  (setq solaire-mode-remap-fringe nil)
  (add-to-list 'solaire-mode-remap-alist '((fringe solaire-fringe-face) . t))

  (remove-hook 'focus-in-hook #'solaire-mode-reset)
  (remove-hook! '(doom-load-theme-hook doom-reload-hook) #'solaire-mode-reset))

;;; Load other configs
(load! "latex-config")
(load! "hebrew-latex-config")
(load! "dvorak-config")
(load! "org-config")
(load! "imenu-list-config")
(load! "splash")


;;; Config performance measure
(let ((elapsed (float-time (time-subtract (current-time) t0))))
  ;; I don't wanna encase this whole file in "(let ((t0 ...)))"
  ;; so I just setq t0 at the start and unbind it here.
  (makunbound 't0)
  (message "spent %.3fs in config.el" elapsed))
