;;; packages.el --- evil Layer packages File for Spacemacs
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

(defvar evil-packages
  '(
    ;; package evils go here
    )
  "List of all packages to install and/or initialize. Built-in packages
which require an initialization must be listed explicitly in the list.")

(defvar evil-excluded-packages '(
                                 ;; evil-search-highlight-persist
                                 )
  "List of packages to exclude.")

(define-key evil-motion-state-map ";" 'evil-ex)
(define-key evil-motion-state-map ":" 'evil-repeat-find-char)

(custom-set-faces '(evil-search-highlight-persist-highlight-face ((t (:background "#5F5F5F")))))

;; For each package, define a function evil/init-<package-evil>
;;
;; (defun evil/init-my-package ()
;;   "Initialize my package"
;;   )
;;
;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package
