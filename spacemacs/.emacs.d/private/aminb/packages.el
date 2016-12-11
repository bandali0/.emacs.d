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
  '(crux writeroom-mode)
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


;;; packages.el ends here
