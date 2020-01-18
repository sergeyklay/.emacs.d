;;; shells.el --- Shells configuration. -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2020 Serghei Iakovlev <egrep@protonmail.ch>

;; Author: Serghei Iakovlev <egrep@protonmail.ch>
;; URL: https://github.com/sergeyklay/.emacs.d
;;
;; This file is NOT part of GNU Emacs.
;;
;; License: GPLv3

;;; Commentary:

;; Shell related features for GNU Emacs.

;;; Code:

(require 'directories)

;; -i is for interactive (I don't use it)
;; -c tells bash to read whatever commands follow
;; -l means invoke login shells, so that .profile or .bash_profile is read
;;
;; For more see:
;;
;; - `https://github.com/bbatsov/projectile/issues/1097'
;; - `https://emacs.stackexchange.com/q/3447/16592'
(setq shell-command-switch "-lc")

;;;; Exec Paths

;; Emacs does set `exec-path' from the value of PATH on startup, but will not
;; look at it again later.  But if you run a command, it will inherit PATH,
;; not `exec-path', so subprocesses can find different commands than Emacs
;; does.  This can be especially confusing for `shell-command', as that does
;; not run a process directly, but calls a shell to run it, which will use
;; PATH, not `exec-path'.
;;
;; The problem on macOs is that macOs does not set the environment the same
;; when you call a program from the global UI or when you call it from a shell.
;; This means that running Emacs from a shell will result in different
;; environment variables being set than when you run it from the finder.  This
;; is especially annoying if you set environment variables in .bashrc or
;; similar, as that won't affect the "global" Emacs.
(use-package exec-path-from-shell
  :if (memq window-system '(mac ns x))
  :custom
  (exec-path-from-shell-variables '("PATH"))
  (exec-path-from-shell-arguments '("-l"))
  :config
  (exec-path-from-shell-initialize))

;;; Eshell

;; Emacs shell interactive mode.
(use-package eshell
  :commands eshell-mode
  :custom
  (eshell-scroll-to-bottom-on-input 'all)
  (eshell-scroll-to-bottom-on-output 'all)
  (eshell-kill-processes-on-exit t)
  (eshell-directory-name (concat user-etc-dir "eshell/"))
  :config

  (use-package eshell-git-prompt
    :init
    (eshell-git-prompt-use-theme 'default)))

(with-eval-after-load 'eshell-mode
  (define-key eshell-mode-map (kbd "C-a") 'eshell-bol))

(bind-key (kbd "M-s e") 'eshell)

(provide 'shells)
;;; shells.el ends here
