;;; ~/.doom.d/dvorak-config.el -*- lexical-binding: t; -*-

(after! quail
  (add-to-list 'quail-keyboard-layout-alist
               `("dvorak" . ,(concat "                              "
                                     "  1!2@3#4$5%6^7&8*9(0)[{]}`~  "
                                     "  '\",<.>pPyYfFgGcCrRlL/?=+    "
                                     "  aAoOeEuUiIdDhHtTnNsS-_\\|    "
                                     "  ;:qQjJkKxXbBmMwWvVzZ        "
                                     "                              ")))
  (quail-set-keyboard-layout "dvorak"))
(setq avy-keys '(?a ?o ?e ?u ?i ?d ?h ?t ?n ?s)) ; dvorak home row
(setq aw-keys '(?a ?o ?e ?u ?i ?d ?h ?t ?n ?s)) ; dvorak home row
