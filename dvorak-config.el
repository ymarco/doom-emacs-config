;;; ~/.config/doom/dvorak-config.el -*- lexical-binding: t; -*-

;; Make quail know what is the global key layout. maybe not useful for
;; everybody, but that way I can write in an another input method (Hebrew) while
;; leaving my global keymap as dvorak. Without this, the remapping of keys to
;; another input method gets screwed: For instance, with Hebrew, 'א' is normally
;; placed on 'T' in qwerty - but when using dvorak, to get a 'א' in the internal
;; input method youll need to find the DVORAK 'T' which is somewhere else and
;; press that. Its a nightmare.
(after! quail
  (add-to-list 'quail-keyboard-layout-alist
               `("dvorak" . ,(concat "                              "
                                     "  1!2@3#4$5%6^7&8*9(0)[{]}`~  "
                                     "  '\",<.>pPyYfFgGcCrRlL/?=+    "
                                     "  aAoOeEuUiIdDhHtTnNsS-_\\|    "
                                     "  ;:qQjJkKxXbBmMwWvVzZ        "
                                     "                              ")))
  (quail-set-keyboard-layout "dvorak"))
;; Convert things that default to querty home row to dvorak
(let ((dvorak-homerow '(?a ?o ?e ?u ?i ?d ?h ?t ?n ?s)))
  (setq avy-keys dvorak-homerow
        aw-keys dvorak-homerow
        webkit-ace-chars (apply #'string dvorak-homerow)))
