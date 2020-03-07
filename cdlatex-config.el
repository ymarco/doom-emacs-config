;;; ~/.doom.d/cdlatex-config-mine.el -*- lexical-binding: t; -*-

(setq cdlatex-math-symbol-prefix ?\;
      cdlatex-math-symbol-alist
      '( ;; adding missing functions to 3rd level symbols
        (?_    ("\\downarrow"  ""           "\\inf"))
        (?^    ("\\uparrow"    ""           "\\sup"))
        (?k    ("\\kappa"      ""           "\\ker"))
        (?m    ("\\mu"         ""           "\\lim"))
        (?d    ("\\delta"      "\\partial"  "\\dim"))
        (?D    ("\\Delta"      "\\nabla"    "\\deg"))
        ;; no idea why Phi isnt on F in first place
        (?F    ("\\Phi"))
        ;; now just conveniance
        (?:    ("\\dots")))
      cdlatex-math-modify-alist
      '( ;; my own stuff
        (?/  "\\oner"        nil          t    nil  nil)
        (?h  "\\half"        nil          t    nil  nil)))
