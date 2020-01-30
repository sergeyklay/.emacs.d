;;; langs-conf.el --- Configuration like languages. -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2020 Serghei Iakovlev <egrep@protonmail.ch>

;; Author: Serghei Iakovlev <egrep@protonmail.ch>
;; URL: https://github.com/sergeyklay/.emacs.d
;;
;; This file is NOT part of GNU Emacs.
;;
;; License: GPLv3

;;; Commentary:

;; Add support for the configuration like languages for GNU Emacs.

;;; Code:

(require 'utils)

;;;; JSON

(use-package json-mode
  :mode "\\.js\\(on\\|[hl]int\\(rc\\)?\\)\\'"
  :config
  (bind-keys :map json-mode-map
	     ("C-c <tab>" . json-mode-beautify)))

;;;; Ini

(use-package ini-mode
  :mode "\\.ini\\'")

;;;; Dotenv

(use-package dotenv-mode
  :mode "\\.env\\'"
  :mode "\\.env\\..*\\'")

;;;; Vim

(use-package vimrc-mode
  :mode "/\\.?g?vimrc\\'"
  :mode "\\.vim\\'"
  :mode "\\.?vimperatorrc\\'"
  :mode "\\.vimp\\'")

;;;; Yaml

(use-package yaml-mode
  :config
  (defun my|common-yaml-hook()
    "The common hook to configure `yaml-mode'."
    (let ((ruby-path (my/ruby-locate-executable)))
      (when (boundp 'flycheck-yaml-ruby-executable)
	(setq-local flycheck-yaml-ruby-executable ruby-path))))
  :hook
  ((yaml-mode . my|common-yaml-hook))
  :mode "\\.ya?ml\\'"
  :interpreter ("yml" . yml-mode))

;;;; Autoconf

(add-to-list 'auto-mode-alist
             '("/config\\.\\(ac\\|in\\|m4\\)\\'" . autoconf-mode))

;;;; Nginx

(use-package nginx-mode
  :mode (("/nginx/sites-\\(?:available\\|enabled\\)/" . nginx-mode)
         ("nginx.conf" . nginx-mode)))

(use-package company-nginx
  :after (company nginx-mode)
  :hook
  ((nginx-mode . company-mode)
   (nginx-mode . company-nginx-keywords)))


;;;; conf-mode

;; https://github.com/jrockway/emacs/blob/master/lisp/textmodes/conf-mode.el
(use-package conf-mode
  :ensure nil
  :mode (("\\.conf\\'"    . conf-space-mode)
         ("\\.setup.*\\'" . conf-space-mode)
         ("/\\(Cargo.lock\\|\\.cargo/config\\)\\'" . conf-toml-mode)))

;;;; Dotenv


;; https://github.com/preetpalS/emacs-dotenv-mode/tree/master
(use-package dotenv-mode
  :mode (("\\.env\\..*\\'" . dotenv-mode)))

;;;; Systemd

;; Major mode for editing systemd units
;; https://github.com/holomorph/systemd-mode
(use-package systemd
  :defer t)

;; TODO: Move below code to the separated file

;;;; Powershell

(defconst my/pwsh-executable-path (executable-find "pwsh")
  "The PowerShell executable path on this system.")

(use-package powershell
  :mode (("\\.ps1\\'"  . powershell-mode)
         ("\\.psm1\\'" . powershell-mode))
  :interpreter "pwsh"
  :config
  (when my/pwsh-executable-path
    (setq powershell-location-of-exe my/pwsh-executable-path)))

;;;; Shell Script

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

(use-package company-shell
  :after (company sh-script)
  :init
  (push 'company-shell company-backends)
  (push 'company-shell-env company-backends)
  :hook (sh-mode . company-mode))

(provide 'langs-conf)
;;; langs-conf.el ends here
