;;; packages.el --- aminb layer packages file for Spacemacs.
;;
;; Copyright (c) 2016 Amin Bandali
;;
;; Author: Amin Bandali <amin@aminb.org>
;; URL: https://github.com/aminb/dotfiles
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;; This file is a collection of my settings and customizations on top of
;; spacemacs.

;;; Code:

(defconst aminb-packages
  '(crux writeroom-mode znc)
  "The list of Lisp packages required by the aminb layer.")

(defun aminb/init-crux ()
    (use-package crux
      :defer t
      :bind (("C-c d" . crux-duplicate-current-line-or-region)
             ("C-c M-d" . crux-duplicate-and-comment-current-line-or-region)
             )))

(defun aminb/init-writeroom-mode ()
  (use-package writeroom-mode             ; Distraction-free editing
    :defer t
    :config (setq writeroom-width 82)
    :bind (("C-c W" . writeroom-mode)
           ("s-?" . writeroom-toggle-mode-line))))

(defun aminb/init-znc ()
  (use-package znc
    :defer t
    :init
    (spacemacs/set-leader-keys
      "aiz" 'znc-erc)
    :config
    (progn
      ;; Set the erc nick completion postfix to ", "
      (setq erc-pcomplete-nick-postfix ", ")

      ;; Restore channel buffers from logs
      (setq erc-log-insert-log-on-open t)

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
      (vbe:znc-add-server "nix.aminb.org" 6669 "amin"
                          '(freenode mozilla))

      ;; https://www.emacswiki.org/emacs/ErcBar
      ;; Display a bar before unread messages
      (eval-after-load 'erc-track
        '(progn
           (defun erc-bar-move-back (n)
             "Moves back n message lines. Ignores wrapping, and server messages."
             (interactive "nHow many lines ? ")
             (re-search-backward "^.*<.*>" nil t n))

           (defun erc-bar-update-overlay ()
             "Update the overlay for current buffer, based on the content of
erc-modified-channels-alist. Should be executed on window change."
             (interactive)
             (let* ((info (assq (current-buffer) erc-modified-channels-alist))
                    (count (cadr info)))
               (if (and info (> count erc-bar-threshold))
                   (save-excursion
                     (end-of-buffer)
                     (when (erc-bar-move-back count)
                       (let ((inhibit-field-text-motion t))
                         (move-overlay erc-bar-overlay
                                       (line-beginning-position)
                                       (line-end-position)
                                       (current-buffer)))))
                 (delete-overlay erc-bar-overlay))))

           (defvar erc-bar-threshold 1
             "Display bar when there are more than erc-bar-threshold unread messages.")
           (defvar erc-bar-overlay nil
             "Overlay used to set bar")
           (setq erc-bar-overlay (make-overlay 0 0))
           (overlay-put erc-bar-overlay 'face '(:underline "purple"))
           ;;put the hook before erc-modified-channels-update
           (defadvice erc-track-mode (after erc-bar-setup-hook
                                            (&rest args) activate)
             ;;remove and add, so we know it's in the first place
             (remove-hook 'window-configuration-change-hook 'erc-bar-update-overlay)
             (add-hook 'window-configuration-change-hook 'erc-bar-update-overlay))
           (add-hook 'erc-send-completed-hook (lambda (str)
                                                (erc-bar-update-overlay)))))

      )))

;;; packages.el ends here
