;;; ~/.config/doom/cdlatex-config-mine.el -*- lexical-binding: t; -*-

(setq cdlatex-math-symbol-prefix ?\;
      cdlatex-math-symbol-alist
      '( ;; adding missing functions to 3rd level symbols
        ;;key   1st            2nd           3rd
        (?_    ("\\downarrow"  ""           "\\inf"))
        (?^    ("\\uparrow"    ""           "\\sup"))
        (?k    ("\\kappa"      ""           "\\ker"))
        (?m    ("\\mu"         ""           "\\lim"))
        (?d    ("\\delta"      "\\partial"  "\\dim"))
        (?D    ("\\Delta"      "\\nabla"    "\\deg"))
        ;; no idea why \Phi isnt on 'F' in first place, \phi is on 'f'.
        (?F    ("\\Phi"))
        ;; now just conveniance
        (?:    ("\\dots")))
      cdlatex-math-modify-alist
      '( ;; my own stuff
        ;;key  mathcmd   textcmd  type  rmdot(?)  it(?)
        (?/    "\\oner"  nil      t     nil       nil)
        (?h    "\\half"  nil      t     nil       nil)))
