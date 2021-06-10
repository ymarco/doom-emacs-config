;;; mu4e.el -*- lexical-binding: t; -*-

(setq +mu4e-mu4e-mail-path "~/.local/share/mail")
(set-email-account! "yoavm448@gmail.com"
                    `((mu4e-sent-folder       . "/yoavm448@gmail.com/[Gmail].Sent Mail")
                      (mu4e-drafts-folder     . "/yoavm448@gmail.com/[Gmail].Drafts")
                      (mu4e-trash-folder      . "/yoavm448@gmail.com/[Gmail].Trash")
                      (mu4e-refile-folder     . "/yoavm448@gmail.com/[Gmail].All Mail")
                      (smtpmail-smtp-user     . ,user-mail-address)
                      (mu4e-compose-signature . nil)) ; I like to make it on the spot
                    'default)
(add-to-list 'load-path "/usr/share/emacs/site-lisp/mu4e" :append)
(after! mu4e
  (setq sendmail-program (executable-find "msmtp")
        send-mail-function #'smtpmail-send-it
        message-sendmail-f-is-evil t
        message-sendmail-extra-arguments '("--read-envelope-from") ; , "--read-recipients")
        message-send-mail-function #'message-send-mail-with-sendmail)
  (map! [remap mu4e-update-mail-and-index] #'mu4e-update-index))
(add-hook! '(mu4e-view-mode-hook mu4e-compose-mode-hook)
           #'hebrew-mode)
