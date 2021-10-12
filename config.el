;;; .doom.d/config.el -*- lexical-binding: t; -*-

;; measure time! see end of file for explanation
(setq t0 (current-time))

(defun get-preferred-theme ()
  (let ((hour (string-to-number
               (substring (current-time-string) 11 13))))
    (if (<= 6 hour 17)
        'doom-one-light
      'doom-spacegrey)))
(setq doom-theme (get-preferred-theme))
(run-with-timer (* 60 60) (* 60 60)
                (defun update-theme ()
                  (let ((preferred (get-preferred-theme)))
                    (unless (eq preferred doom-theme)
                      (load-theme preferred t)))))
;;; Misc package options
(setq
 gc-cons-threshold 67108864             ; 64mb
 gc-cons-percentage 0.2
 scroll-margin 7
 ;; User config, used for templates mostly
 user-full-name "Yoav Marco"
 user-mail-address "yoavm448@gmail.com"
 user-login-name "ymarco"
 ;; prettiness
 ;; take new window space from all other windows (not just current)
 window-combination-resize t
 ;; don't ask me every time
 geiser-active-implementations '(guile)
 ;; Even more cololful pars
 rainbow-delimiters-max-face-count    4
 ;; Dired auto-detects multiple windows
 dired-dwim-target                    t
 ;; I don't need it to tell me its UTF-8
 doom-modeline-buffer-encoding nil
 +modeline-encoding nil                 ; same for (modeline +light)
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
 evil-ex-substitute-global t
 ;; Don't replace the current window when splitting
 evil-split-window-below  t
 evil-vsplit-window-right t
 ;; This way I use evil-snipe instead of evil-easymotion. I find
 ;; searching-then-repeating-search-until-done faster then
 ;; searching-then-typing-the-displayed-letter-pair
 evil-snipe-scope 'visible
 ;; Wait for a bit shorter - I've got native comp - before prompting me
 lsp-idle-delay 0.2
 lsp-ui-sideline-delay 0.2
 lsp-enable-symbol-highlighting nil
 js2-idle-timer-delay 0.2
 eldoc-idle-delay 0.2
 ;; Fix inconsistency with tab width in JSON files
 json-reformat:indent-width 2
 ;; zfs compresses for me already
 undo-fu-session-compression nil
 projectile-project-search-path '("~/projects")
 abbrev-file-name (concat doom-private-dir "abbrevs.el")
 deft-directory "~/org/"
 org-roam-directory "~/org/"
 rmh-elfeed-org-files (list "~/media/elfeed-rss.org")
 source-directory "~/programs/emacs-pgtk-nativecomp-source"
 ;; shorten modeline by decreasing icons size
 all-the-icons-scale-factor 1.0)

;; Don't blink the cursor, it's too distracting.
(blink-cursor-mode -1)
(setq visible-cursor nil)

;; I hate these vertical scrolls
(setq truncate-lines nil)
(setq-default truncate-lines nil)
;; Colorify colors in X config mode
(add-hook 'conf-xdefaults-mode-hook #'rainbow-mode)
;; When more witty things to say about mixed-pitch, this comment would be
;; replaced
(add-hook! '(markdown-mode-hook elfeed-show-mode-hook) #'mixed-pitch-mode)
(add-hook 'elfeed-search-mode #'elfeed-update)

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

(remove-hook 'evil-insert-state-entry-hook #'delete-selection-mode)

(use-package! calibredb
  :commands (calibredb)
  :config
  (setq sql-sqlite-program (executable-find "sqlite3")
        calibredb-root-dir "~/Desktop/books"
        calibredb-db-dir (expand-file-name "metadata.db" calibredb-root-dir)
        calibredb-program (executable-find "calibredb"))
  (custom-set-faces!
    '(calibredb-search-header-highlight-face :inherit hl-line)))

(evil-define-command +evil:drag-file (file)
  "Open a drag window with dragon for the file opened in the current buffer.
With FILE, use that file instead. Also works in Dired buffers."
  (interactive "<f>")
  (start-process "dragon-from-emacs"
                 nil
                 "dragon"
                 (or file
                     (when (eq major-mode 'dired-mode)
                       (dired-get-filename))
                     (when-let* ((file (file-name-extension (buffer-file-name)))
                                 (_ (or (eq major-mode 'org-mode)
                                        (eq major-mode 'latex-mode)))
                                 (_ (file-exists-p file)))
                       file)
                     (buffer-file-name))
                 "-x"))
(evil-ex-define-cmd "drag" #'+evil:drag-file)
(evil-ex-define-cmd "lc" (defun +evil:run-latex-cleanup ()
                           (interactive)
                           (start-process "latex-cleanup:from-emacs" "*Messages*"
                                          "latex-cleanup")))
(evil-ex-define-cmd "pdff[rame]"
                    (defun +evil:export-frame-to-pdf-and-drag ()
                      (interactive)
                      ;; no need to create seperate temp files for each
                      ;; export,they shouldn't be colliding.
                      (let ((filename "/tmp/frames.pdf"))
                        ;; for some reason using write-region doesn't work in
                        ;; this case
                        (with-temp-file filename
                          (insert (x-export-frames)))
                        (+evil:drag-file filename))))

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
 ;;  the normal ! operator is in practice totally useless
 :n "!" (cmd! (evil-ex "!"))
 ;; standard emacs delete
 :i "C-d"     #'delete-char
 ;; $ is way too inconvenient, and I barely use marks
 :n "m"       #'evil-end-of-line
 ;; to replace the lost m. I never use regisetrs, and we have SPC i y anyway.
 :n "\""      #'evil-set-marker
 ;; Things I picked up from JetBrains IDEs
 :ni "C-/"    #'comment-line
 :v   "C-/"   #'comment-or-uncomment-region
 :ni "C-M-l"  #'+format/buffer
 ;; force myself to use M-q (fill-column) through normal-mode, so I can undo it
 :i "M-q" nil
 (:after lsp-mode :map lsp-mode-map
  "M-RET"     #'lsp-execute-code-action)
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
 :ni "M-u"    #'sp-up-sexp              ; exit parenthesis
 :n "$" (lambda (cmd file)
          "Execute a command asyncly on current file."
          (interactive (list (read-from-minibuffer "cmd: " nil minibuffer-local-ns-map)
                             (pcase major-mode
                               ('dired-mode (dired-get-filename))
                               (_ (buffer-file-name)))))
          (make-process
           :name (concat "user-started:" cmd)
           :noquery t
           :buffer nil
           :command (list cmd file)))
 :leader
 :prefix "o"
 "s" (cmd! (make-process
            :name "user-terminal"
            :noquery t
            :buffer nil
            :command (list "setsid" (or (getenv "TERMINAL") "st")))))

(mapc (lambda (c)
        (map! :n (string c)
              (lambda () (interactive)
                (require 'quail)
                ;; switch to English
                (start-process "switch-to-hebrew" nil
                               "swaymsg" "input * xkb_switch_layout 0")
                ;; emulate the keybind caught by this lambda
                (cl-callf2 nconc
                    (list
                     (aref quail-keyboard-layout
                           (string-match (string c)
                                         (concat
                                          "                              "
                                          "  1!2@3#4$5%6^7&8*9(0)-_=+`~  "
                                          "  /Q'WקEרRאTטYוUןIםOפP[{]}    "
                                          "  שAדSגDכFעGיHחJלKךLף:'\"\\|  "
                                          "    זZסXבCהVנBמNצMת<ץ>/?      "
                                          "                                "))))
                    unread-command-events))))
      "אבגדהוזחטיכךלמםנןסעפףצץקרשת")

(map! :after evil-markdown
      :map evil-markdown-mode-map
      :i "M-b" nil)
;; Company, don't block my snippet expansion
;; FIXME doesn't work
(define-key! company-active-map
  "TAB" nil
  [tab] nil)

(defvar prvt/omitted-extensions '(".aux" ".bbl" ".blg" ".brf" ".dpth" ".auxlock"
                                  ".log" ".out" ".synctex.gz" ".dvi" ".md5")
  "Extensions that I never wanna see.")
;; Don't show latex junk files in dired
(after! dired-x
  (setq dired-omit-extensions (append dired-omit-extensions
                                      prvt/omitted-extensions)))

(setq counsel-find-file-ignore-regexp (concat "\\(?:^[#.]\\|?:[#~]$\\|?:^Icon?\\)"
                                              "\\|"
                                              (regexp-opt prvt/omitted-extensions)))

(add-hook! 'doom-load-theme-hook
  (custom-set-faces!
    ;; Let me see syntax highlighting even under #ifndef that amount to false
    ;; `(font-lock-preprocessor-face :foreground nil :background ,(doom-color 'base2))
    `(ccls-skipped-range-face :foreground nil :background ,(doom-color 'base2))
    ;; highlight matching parens more clearly
    `(show-paren-match :weight normal :foreground nil :background ,(doom-color 'selection))
    ;; comments are too dark in spacegrey
    `(font-lock-comment-face
      :foreground
      ,(doom-blend (doom-color 'fg)
                   (face-attribute 'font-lock-comment-face :foreground) 0.2))
    ;; remove ugly underline
    `(mu4e-header-highlight-face :background ,(doom-color 'bg))))

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

(setq iedit-toggle-key-default nil)

;; fonts
(setq
 doom-unicode-font (font-spec :family "Symbola" :size 20)
 doom-font (font-spec :family "monospace" :size 20)
 ;; I have issues with variable-pitch fonts that appear larger than their
 ;; mixed-pitch brothers. Setting the font size would normally work, but it
 ;; doesn't with mixed-pitch-mode which I use. Fortunately, Source Sans Pro is
 ;; just in the perfect size for ubuntu-mono!
 ;; EDIT: you can do that by setting 'height property, I've read the code. But
 ;; source sans pro is so pretty!
 doom-variable-pitch-font (font-spec :family "Source Sans Pro" :size 20)
 doom-serif-font (font-spec :family "XCharter"))
(add-hook! 'after-setting-font-hook
  (set-fontset-font t 'greek doom-font)
  (set-fontset-font t 'hebrew (font-spec :family "DejaVu Sans"))
  (set-fontset-font t 'kana (font-spec :family "Noto Sans CJK SC"))
  (set-fontset-font t 'cjk-misc (font-spec :family "Noto Sans CJK SC")))



;;; Load other configs
(load! "latex-config")
(load! "hebrew-latex-config")
(load! "dvorak-config")
(load! "org-config")
(load! "mu4e-config")
(when EMACS28+
  (load! "native-comp.el"))
(load! "splash")

(add-hook! 'nix-mode-hook (company-mode -1))

(use-package! snapshot-timemachine
  :defer t
  :init
  ;; virtual files in .zfs/ somehow bug out the ivy stuff with recentf
  (after! recentf
    (add-to-list 'recentf-exclude "/\\.zfs/"))
  :config
  ;; lispy C-j C-k bindings shadow the ones on snapshot-timemachine
  (when (featurep! :editor lispy)
    (add-hook! 'snapshot-timemachine-mode-hook (lispy-mode -1)))
  (map! :map snapshot-timemachine-mode-map
        :n "C-k" #'snapshot-timemachine-show-prev-interesting-snapshot
        :n "C-j" #'snapshot-timemachine-show-next-interesting-snapshot
        :n "q"   #'snapshot-timemachine-quit
        :map snapshot-timeline-mode-map
        :n "RET" #'snapshot-timeline-show-diff
        :n "<S-return>" #'snapshot-timeline-view-snapshot
        :n "a"   #'snapshot-timeline-mark-as-A
        :n "b"   #'snapshot-timeline-mark-as-B
        :n "d"   #'snapshot-timeline-show-diff-A-B
        :n "q"   #'snapshot-timemachine-quit
        :n "j"   #'snapshot-timeline-goto-next-snapshot
        :n "k"   #'snapshot-timeline-goto-prev-snapshot
        :n "J"   #'snapshot-timeline-goto-next-interesting-snapshot
        :n "K"   #'snapshot-timeline-goto-prev-interesting-snapshot
        :n "u"   #'snapshot-timeline-unmark
        :n "U"   #'snapshot-timeline-unmark-all
        :n "m"   #'snapshot-timeline-emerge-A-B
        :n "d"   #'snapshot-timeline-ediff-A-B
        :n "<"   #'tabulated-list-narrow-current-column
        :n ">"   #'tabulated-list-widen-current-column)
  (defadvice! +snapshot-timemachine-close-diff-as-well ()
    "Close the opened diff when quitting `snapshot-timeline'."
    :before #'snapshot-timemachine-quit
    (when-let ((diff-buffer (get-buffer
                             (format "diff:%s" (file-name-nondirectory
                                                snapshot-timemachine--file)))))
      (mapc #'delete-window (get-buffer-window-list diff-buffer))
      (kill-buffer diff-buffer)))
  (defadvice! +snapshot-timemachine-header ()
    "Display snapshot info in the header. Minibuffer tends to get hidden."
    :after #'snapshot-timemachine-show-focused-snapshot
    (let ((snapshot (snapshot-timemachine--zipper-focus
                     snapshot-timemachine--snapshots)))
      (setq header-line-format
            (format "Snapshot %s from %s"
                    (propertize  (snapshot-name snapshot) 'face 'font-lock-variable-name-face)
                    (propertize (format-time-string snapshot-timemachine-time-format
                                                    (snapshot-date snapshot))
                                'face 'font-lock-builtin-face)))))
  (defun get-snapshots (file)
    "Return a list of paths for existing snapshotted copies of FILE."
    (let ((snapshot-dir (locate-dominating-file file ".zfs")))
      (file-expand-wildcards
       (concat snapshot-dir ".zfs/snapshot/*/"
               (string-trim-left file (regexp-quote snapshot-dir))))))
  (defun get-snapshot-tables (filename)
    (sort
     (cl-loop for file being the elements of (get-snapshots filename)
              using (index i)
              for name = (progn (string-match ".*/\\.zfs/snapshot/\\(zfs-auto-snap_[a-z]+-[^/]+\\)/" file)
                                (match-string 1 file))
              for date-odd-string = (progn (string-match "zfs-auto-snap_[a-z]+-\\([^/]+\\)/" file)
                                           (match-string 1 file))
              for date = (progn
                           (string-match
                            "\\([0-9]+\\)-\\([0-9]+\\)-\\([0-9]+\\)-\\([0-9]+\\)h\\([0-9]+\\)U"
                            date-odd-string)
                           (cl-destructuring-bind (year month day hour minute)
                               (mapcar (lambda (i) (string-to-number (match-string i date-odd-string)))
                                       (number-sequence 1 5))
                             (encode-time
                              ;; decoded-time format
                              (list 0 minute hour day month year nil nil t))))
              collect (make-snapshot
                       :id i
                       :name name
                       :file file
                       :date date))
     (lambda (s1 s2)
       (time-less-p (snapshot-date s1) (snapshot-date s2)))))
  (setq snapshot-timemachine-snapshot-finder #'get-snapshot-tables))

(use-package! lexic
  :commands lexic-search lexic-list-dictionary
  :init
  (defadvice! +lookup/dictionary-definition-lexic (identifier &optional arg)
    "Look up the definition of the word at point (or selection) using `lexic-search'."
    :override #'+lookup/dictionary-definition
    (interactive
     (list (or (doom-thing-at-point-or-region 'word)
               (read-string "Look up in dictionary: "))
           current-prefix-arg))
    (lexic-search identifier nil nil t))
  (defun +lexic-capture ()
    (interactive)
    (let* ((selection (unless (and (fboundp 'pgtk-backend-display-class)
                                   (equal (pgtk-backend-display-class) "GdkWaylandDisplay"))
                        (x-selection)))
           (gwidth (display-pixel-width))
           (gheight (display-pixel-height))
           (charwidth 81)
           (width (* charwidth (frame-char-width)))
           (height (/ gheight 4))
           (left (/ (- gwidth width) 4)) ; FIXME should be 2 but the `left' scale is 1080 and gwidth is 3840
           (top (/ (- gheight height) 4))
           (charheight (round (/ height (frame-char-width))))
           (frame (make-frame `((name . ,(concat "* Define Word:" selection))
                                (width . ,charwidth)
                                (height . ,charheight)
                                (left . ,left)
                                (top . ,top)
                                (auto-raise . t)
                                (skip-taskbar . t)
                                (alpha . 97)))))
      (require 'lexic)
      ;; TODO handle quit signal and close frame
      (condition-case err
          (lexic-search selection nil nil t)
        (t
         (progn
           (message "err: %s" err)
           (when selection (message "%s not found" selection))
           (call-interactively #'lexic-search))))
      (delete-other-windows)
      (add-transient-hook! #'lexic-return-from-lexic :after
                           (when (equal (selected-frame) frame)
                             (delete-frame frame)))))
  :config
  (map! :map lexic-mode-map
        :n "q" #'lexic-return-from-lexic
        :nv "RET" #'lexic-search-word-at-point
        :n "TAB" #'lexic-toggle-entry
        :n "<tab>" #'lexic-toggle-entry
        :n "a" #'outline-show-all
        :n "S-TAB" (cmd! (outline-hide-sublevels 3))
        :n "<S-tab>" (cmd! (outline-hide-sublevels 3))
        :n "C-j" #'lexic-next-entry
        :n "C-J" (cmd! (lexic-next-entry t))
        :n "C-k" #'lexic-previous-entry
        :n "C-K" (cmd! (lexic-previous-entry t))
        :n "C-h" #'lexic-search-history-backwards
        :n "C-l" #'lexic-search-history-forwards
        :n "s" #'lexic-search)
  (add-hook 'lexic-mode-hook #'turn-off-evil-snipe-mode)
  ;; FIXME mixed-pitch? fix indentation
  (font-lock-add-keywords
   'lexit-mode
   '(("^[​ ]+" (0 font-lock-comment-face t))
     ("^[​ ]*-\\( \\)" (1 font-lock-comment-face t))
     ("^[​ ]*[0-9]+\\.\\( \\)" (1 font-lock-comment-face t)))))

(setq safe-local-variable-values '((eval . (flyspell-mode t))
                                   (org-latex-compiler . "xelatex")))

(after! all-the-icons
  (when (boundp 'all-the-icons-extension-icon-alist)
    (push
     '("drv" all-the-icons-octicon "package" :v-adjust 0.0 :face all-the-icons-dsilver)
     all-the-icons-extension-icon-alist)))

(defvar +dired-hidden-subdir-p-prev-result nil
  "Previous argument and result of `dired-subdir-hidden-p', held
in a cons cell of (dir . hidden?).")

(defadvice! +dired-cache-subdir-hidden-p-a (orig-fn dir)
  "Cache the previous result of `dired-subdir-hidden-p' using
`+dired-hidden-subdir-p-prev-result'"
  :around #'dired-subdir-hidden-p
  (unless (equal dir (car +dired-hidden-subdir-p-prev-result))
    (setq +dired-hidden-subdir-p-prev-result
          (cons dir (funcall orig-fn dir)))
    (cdr +dired-hidden-subdir-p-prev-result)))

(defadvice! +vc-backend-is-always-git-a (file &optional no-error)
  :override #'vc-responsible-backend
  (if (locate-dominating-file file ".git")
      'Git
    (unless no-error
      (error "No VC backend is responsible for %s" file))))

(use-package! crdt
  :commands (crdt-share-buffer crdt-connect)
  :config
  (map! :map crdt-buffer-menu-mode-map
        :n "RET" #'crdt--buffer-menu-goto
        :n "d"   #'crdt--buffer-menu-kill
        :map crdt-user-menu-mode-map
        :n "RET" #'crdt--user-menu-goto
        :map crdt-session-menu-mode-map
        :n "RET" #'crdt--session-menu-goto
        :n "d"   #'crdt--session-menu-kill))

(map! :after vterm
      :map vterm-mode-map
      :ing "C-c" nil
      :ing "C-c" #'vterm-send-C-c)

(map! :after dired
      :map dired-mode-map
      :n "l" #'dired-find-file
      :n "h" #'dired-up-directory)

(use-package! webkit-ace
  :commands (webkit-ace))
(use-package! webkit
  :commands (webkit webkit-browse-url)
  :init
  (add-load-path! (concat doom-private-dir "packages/emacs-webkit"))
  (setq browse-url-browser-function #'webkit-browse-url)
  (defun +webkit-toggle-current-file ()
    (interactive)
    (require 'webkit)
    (pcase major-mode
      ('webkit-mode
       (require 'ffap)
       (find-file (ffap-fixup-url (webkit--get-uri webkit--id))))
      (_
       (webkit-browse-url (xdg-thumb-uri buffer-file-name)))))
  (map! :after web-mode
        :map web-mode-map
        :localleader
        "w" #'+webkit-toggle-current-file)
  :config
  (map! :map webkit-mode-map
        :n "F" #'+webkit-toggle-current-file)
  (setq webkit-history-file (concat doom-etc-dir "webkit-history"))
  (setq webkit-dark-mode t)
  (require 'evil-collection-webkit)
  (evil-collection-xwidget-setup))

(use-package! meson-mode
  :mode ("meson\\.build\\'" . meson-mode))

(use-package screenshot
  :defer t
  :init
  (map! :leader
        :prefix "c"
        "s" #'screenshot))


(defun consult-better-jumper ()
  (interactive)
  (let* ((ctx (better-jumper--get-current-context))
         (marker-table (better-jumper--get-marker-table ctx))
         (ring (better-jumper--get-jump-list ctx))
         (idx (better-jumper-jump-list-struct-idx (better-jumper--get-struct ctx)))
         (cands (cl-loop for i from 0 to (ring-length ring)
                         for jump = (ring-ref ring i)
                         for file-name = (nth 0 jump)
                         for pos* = (nth 1 jump)
                         for marker-key* = (nth 2 jump)
                         for marker* = (gethash marker-key* marker-table)
                         for marker = (if (and marker* (marker-position marker*))
                                          marker*
                                        (set-marker (make-marker) pos* (get-buffer file-name)))
                         collect (consult--location-candidate
                                  (with-current-buffer (marker-buffer marker)
                                    (goto-char marker)
                                    (buffer-substring (line-beginning-position) (line-end-position)))
                                  marker
                                  ;; i instead of real line number
                                  i
                                  ;; add shortened file name in text property to use as title
                                  'consult--title
                                  (abbreviate-file-name file-name))))
         (default (when (>= idx 0) (nth idx cands))))
    (message "idx: %s, def: %s" idx default)
    (consult--read
     cands
     :prompt "Jump to: "
     :annotate (consult--line-prefix)
     :sort nil
     :require-match t
     :lookup #'consult--line-match
     :state (consult--jump-state)
     :title (lambda (cand transform)
              (if transform
                  cand
                (get-text-property 0 'consult--title cand)))
     :default default
     :sort nil)))

(advice-add #'+selectrum--embark-target-package! :override #'ignore)

;;; Config performance measure
(let ((elapsed (float-time (time-subtract (current-time) t0))))
  ;; I don't wanna encase this whole file in "(let ((t0 ...)))"
  ;; so I just setq t0 at the start and unbind it here.
  (makunbound 't0)
  (message "spent %.3fs in config.el" elapsed))
