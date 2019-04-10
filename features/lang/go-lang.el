;;; go-lang.el --- Go related configuration. -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Serghei Iakovlev <sadhooklay@gmail.com>

;; Author: Serghei Iakovlev <sadhooklay@gmail.com>
;; URL: https://github.com/sergeyklay/.emacs.d
;;
;; This file is not part of GNU Emacs.
;;
;; License: GPLv3

;;; Commentary:

;; Go related configuration for GNU Emacs.

;;; Code:

(defun my--go-setup-env-vars ()
  "Setting up enviroment variables for Go lang."
  (unless gopath
    (setq gopath (concat (getenv "HOME") "/go")))

  (unless (getenv "GOPATH")
    (setenv "GOPATH" gopath))

  (unless (member (concat gopath "/bin") exec-path)
    (setenv "PATH" (concat (getenv "PATH") ":" (concat gopath "/bin")))
    (setq exec-path (append exec-path (list (concat gopath "/bin"))))))

(defun my--go-common-hook ()
  "Common configuration for Go lang."
  ;; Call Gofmt before saving
  (add-hook 'before-save-hook 'gofmt-before-save t)
  (subword-mode 1))

(use-package go-mode
  :defer t
  :mode "\\.go\\'"
  :preface
  (use-package go-eldoc
    :defer t)

  :hook
  ((go-mode . my--go-setup-env-vars)
   (go-mode . go-eldoc-setup)
   (go-mode . my--go-common-hook)))

(use-package company-go
  :defer t
  :init
  (progn
    (my|add-company-backends
      :modes go-mode
      :backends company-go
      :variables company-go-show-annotation t)))

(provide 'go-lang)
;;; go-lang.el ends here
