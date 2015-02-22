;;; packages.el --- mu4e Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2014-2015 Sylvain Benner & Contributors
;; Copyright (c) 2015 Amin Bandali
;;
;; Authors: Amin Bandali <me@aminb.org>
;;          Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/aminb/dotfiles
;;
;; This file is not part of GNU Emacs; nor that of spacemacs.
;;
;;; License: GPLv3

(defvar mu4e-packages
  '(
    ;; mu4e is not in any repos, so it's commented
    ;; mu4e
    smtpmail
    )
  "List of all packages to install and/or initialize. Built-in packages
which require an initialization must be listed explicitly in the list.")

(defvar mu4e-excluded-packages '()
  "List of packages to exclude.")

(defun get-string-from-file (filePath)
  "Return filePath's file content."
  (with-temp-buffer
    (insert-file-contents filePath)
    (buffer-string)))
;; thanks to “Pascal J Bourguignon” and “TheFlyingDutchman
;;   〔zzbba…@aol.com〕”. 2010-09-02

(defun get-passwd-file (name)
  "Return name's passwd file content"
  (get-string-from-file (concat "/home/amin/.passwd/" name)))

(require 'mu4e)
(require 'smtpmail)
(setq
    mu4e-get-mail-command "offlineimap"   ;; or fetchmail, or ...
    mu4e-update-interval 300              ;; update every 5 minutes
    mu4e-sent-folder "/aminb/Sent"
    mu4e-drafts-folder "/aminb/Drafts"
    mu4e-trash-folder "/aminb/Trash"
    user-mail-address (get-passwd-file "aminb-mail")
    smtpmail-default-smtp-server "mail.aminb.org"
    smtpmail-local-domain "aminb.org"
    smtpmail-smtp-server "mail.aminb.org"
    smtpmail-stream-type 'ssl
    smtpmail-smtp-service 465)

(defvar my-mu4e-account-alist
  '(("aminb"
     (mu4e-sent-folder "/aminb/Sent")
     (mu4e-drafts-folder "/aminb/Drafts")
     (mu4e-trash-folder  "/aminb/Trash")
     (user-mail-address (get-passwd-file "aminb-mail"))
     (user-full-name "Amin Bandali")
     (smtpmail-default-smtp-server "mail.aminb.org")
     (smtpmail-local-domain "aminb.org")
     (smtpmail-smtp-user (get-passwd-file "aminb-user"))
     (smtpmail-smtp-server "mail.aminb.org")
     (smtpmail-stream-type ssl)
     (smtpmail-smtp-service 465))
    ("Gmail"
     (mu4e-sent-folder   "/Gmail/[Gmail].Sent Mail")
     (mu4e-drafts-folder "/Gmail/[Gmail].Drafts")
     (mu4e-trash-folder  "/Gmail/[Gmail].Trash")
     (user-mail-address (get-passwd-file "gmail-mail"))
     (user-full-name "Amin Bandali")
     (smtpmail-default-smtp-server "smtp.gmail.com")
     (smtpmail-local-domain "gmail.com")
     (smtpmail-smtp-user (get-passwd-file "gmail-mail"))
     (smtpmail-smtp-server "smtp.gmail.com")
     (smtpmail-stream-type ssl)
     (smtpmail-smtp-service 465))))

(defun my-mu4e-set-account ()
  "Set the account for composing a message."
  (let* ((account
          (if mu4e-compose-parent-message
              (let ((maildir (mu4e-message-field mu4e-compose-parent-message :maildir)))
                (string-match "/\\(.*?\\)/" maildir)
                (match-string 1 maildir))
            (completing-read (format "Compose with account: (%s) "
                                     (mapconcat #'(lambda (var) (car var))
                                                my-mu4e-account-alist "/"))
                             (mapcar #'(lambda (var) (car var)) my-mu4e-account-alist)
                             nil t nil nil (caar my-mu4e-account-alist))))
         (account-vars (cdr (assoc account my-mu4e-account-alist))))
    (if account-vars
        (mapc #'(lambda (var)
                  (set (car var) (cadr var)))
              account-vars)
      (error "No email account found"))))

(add-hook 'mu4e-compose-pre-hook 'my-mu4e-set-account)


;; Shortcut for mu4e.
(global-set-key (kbd "C-c m") 'mu4e)
(evil-leader/set-key "am" 'mu4e)


;; A special version of the gnus-dired-mail-buffers function
;; that understands mu4e buffers as well
(require 'gnus-dired)
;; make the `gnus-dired-mail-buffers' function also work on
;; message-mode derived modes, such as mu4e-compose-mode
(defun gnus-dired-mail-buffers ()
  "Return a list of active message buffers."
  (let (buffers)
    (save-current-buffer
      (dolist (buffer (buffer-list t))
        (set-buffer buffer)
        (when (and (derived-mode-p 'message-mode)
                   (null message-sent-message-via))
          (push (buffer-name buffer) buffers))))
    (nreverse buffers)))

(setq gnus-dired-mail-mode 'mu4e-user-agent)
(add-hook 'dired-mode-hook 'turn-on-gnus-dired-mode)

(require 'mu4e-contrib)
(setq mu4e-html2text-command 'mu4e-shr2text
      mu4e-view-html-plaintext-ratio-heuristic 10
      mu4e-view-prefer-html t)

;; For each package, define a function mu4e/init-<package-mu4e>
;;
;; (defun mu4e/init-my-package ()
;;   "Initialize my package"
;;   )
;;
;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package
