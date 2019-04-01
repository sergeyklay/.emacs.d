;;; shells.el --- Shells configuration. -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Serghei Iakovlev

;; Author: Serghei Iakovlev (concat "sadhooklay" "@" "gmail" ".com")
;; URL: https://github.com/sergeyklay/.emacs.d
;;
;; This file is not part of GNU Emacs.
;;
;; License: GPLv3

;;; Commentary:

;; Shells related features for the GNU Emacs.

;;; Code:

;; -i is for interactive, and -c tells bash to read whatever commands follow
;;
;; However, due to
;; * `https://github.com/bbatsov/projectile/issues/1097'
;; * `https://emacs.stackexchange.com/q/3447/16592'
;;
;; I use -l instead of -i
;; -l means invoke login shells, so that .profile or .bash_profile is read
(setq shell-command-switch "-lc")

;;; Eshell

;; Emacs shell interactive mode.
(use-package eshell
  :commands eshell-mode
  :init
  (setq eshell-directory-name (concat user-etc-dir "/eshell")
        eshell-scroll-to-bottom-on-input 'all
        eshell-scroll-to-bottom-on-output 'all
        eshell-kill-processes-on-exit t)
  :config
  (use-package eshell-git-prompt
    :init
    (eshell-git-prompt-use-theme 'powerline)))

(with-eval-after-load 'eshell-mode
  (define-key eshell-mode-map (kbd "C-a") 'eshell-bol))

(bind-key (kbd "M-s e") 'eshell)

(provide 'shells)
;;; shells.el ends here
