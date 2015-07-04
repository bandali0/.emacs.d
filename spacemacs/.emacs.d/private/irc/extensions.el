;;; extensions.el --- irc Layer extensions File for Spacemacs
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2014-2015 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;; vbe* functions for reading passwords from authinfo by Vincent Bernat
;;
;;; License: GPLv3

(setq irc-pre-extensions
      '(
        ;; pre extension names go here
        ))

(setq irc-post-extensions
      '(
        ;; post extension names go here
        znc
        ))

;; For each extension, define a function irc/init-<extension-name>
;;
(defun irc/init-znc ()
  "Initialize znc"
  (use-package znc
    :load-path "private/ZNC.el"
    :commands (znc-erc znc-all)
    :defer t
    :init (evil-leader/set-key
            "aize" 'znc-erc
            "aiza" 'znc-all)
    :config
    (progn
      (defun vbe:znc-add-server (server port user networks)
        "Add a server to the list of ZNC servers.
We use SSL inconditionaly. Moreover, we don't store the password
but put nil instead. At least, we tweak the username to contain
the network name later, this will be separated again."
        (add-to-list 'znc-servers
                     (list server
                           port
                           t                  ; SSL enabled
                           (mapcar (function (lambda (slug) (list slug
                                                                  (format "%s/%s" user slug)
                                                                  nil)))
                                   networks))))

      (defun vbe:znc-erc-ssl-connector (&rest R)
        "Connect to ERC using SSL and retrieve password with `auth-source-search'.
Moreover, handle multiple networks by sending the password with
the appropriate network slug that we extract from the nick."
        (let* ((user (nth 0 (split-string (plist-get R :nick) "/")))
               (slug (nth 1 (split-string (plist-get R :nick) "/")))
               (found (nth 0 (auth-source-search :host (plist-get R :server)
                                                 :user user
                                                 :require '(:user :secret)
                                                 :max 1))))
          (if found
              (let ((password (let ((secret (plist-get found :secret)))
                                (if (functionp secret)
                                    (funcall secret)
                                  secret))))
                (plist-put R :password (format "%s/%s:%s" user slug password))
                (plist-put R :nick user)
                (apply 'erc-tls R)))))
      (setq znc-erc-ssl-connector 'vbe:znc-erc-ssl-connector)

      ;; Define networks
      (vbe:znc-add-server "aminb.org" 6697 "aminb"
                          '(freenode mozilla))
      )
    )
  )
;;
;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package
