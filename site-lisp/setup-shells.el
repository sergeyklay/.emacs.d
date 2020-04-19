;;; setup-shells.el --- Shells configuration. -*- lexical-binding: t; -*-

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

(eval-when-compile
  (require 'rx))

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

;;;; Eshell

;; Emacs shell interactive mode.
(use-package eshell
  :ensure nil
  :commands eshell-mode
  :custom
  (eshell-scroll-to-bottom-on-input 'all)
  (eshell-scroll-to-bottom-on-output 'all)
  (eshell-kill-processes-on-exit t)
  (eshell-directory-name (concat user-etc-dir "eshell/"))
  :bind
  (("M-s e" . eshell))
  :config
  (use-package eshell-git-prompt
    :init
    (eshell-git-prompt-use-theme 'default)))

(use-package helm-eshell
  :ensure nil
  :after eshell
  :commands
  (helm-eshell-history
   helm-comint-input-ring
   helm-minibuffer-history)
  :bind (([remap eshell-list-history] . helm-eshell-history)))

;;;; Powershell Mode

(defconst my/pwsh-executable-path (executable-find "pwsh")
  "The PowerShell executable path on this system.")

(use-package powershell
  :mode (("\\.ps1\\'"  . powershell-mode)
         ("\\.psm1\\'" . powershell-mode))
  :interpreter "pwsh"
  :config
  (when my/pwsh-executable-path
    (setq powershell-location-of-exe my/pwsh-executable-path)))

;;;; Shell Mode

(defun sh-variables-in-quotes (limit)
  "Match variables in double-quotes up to LIMIT in `sh-mode'."
  (with-syntax-table sh-mode-syntax-table
    (catch 'done
      (while (re-search-forward
              ;; `rx' is cool, mkay.
              (rx (or line-start (not (any "\\")))
                  (group "$")
                  (group
                   (or (and "{" (+? nonl) "}")
                       (and (+ (any alnum "_")))
                       (and (any "*" "@" "#" "?" "-" "$" "!" "0" "_")))))
              limit t)
        (-when-let (string-syntax (nth 3 (syntax-ppss)))
          (when (= string-syntax 34)
            (throw 'done (point))))))))

(use-package sh-script
  :ensure nil
  :mode (("\\.zsh\\'" . sh-mode)
	 ("zlogin\\'" . sh-mode)
	 ("zlogout\\'" . sh-mode)
	 ("zpreztorc\\'" . sh-mode)
	 ("zprofile\\'" . sh-mode)
	 ("zshenv\\'" . sh-mode)
         ("zshrc\\'" . sh-mode))
  :custom
  (sh-basic-offset 2))

(font-lock-add-keywords
 'sh-mode '((sh-variables-in-quotes
             (1 'default t)
             (2 font-lock-variable-name-face t))))

(use-package company-shell
  :after (company sh-script)
  :init
  (push 'company-shell company-backends)
  (push 'company-shell-env company-backends)
  :hook (sh-mode . company-mode))

(provide 'setup-shells)
;;; setup-shells.el ends here
