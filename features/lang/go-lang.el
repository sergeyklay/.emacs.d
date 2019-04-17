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

(defun my|go-setup-env-vars ()
  "Setting up enviroment variables for Go lang."
  (let (gopath)
    (setq gopath (concat (getenv "HOME") "/go"))
    (unless (getenv "GOPATH")
      (setenv "GOPATH" gopath))
    (unless (member (concat gopath "/bin") exec-path)
      (setenv "PATH" (concat (getenv "PATH") ":" (concat gopath "/bin")))
      (setq exec-path (append exec-path (list (concat gopath "/bin")))))))

(defconst gocode-executable-path (executable-find "gocode")
  "The gocode executable path on this system.")

(defun my|go-common-hook ()
  "Common configuration for Go lang."
  (add-hook 'before-save-hook 'gofmt-before-save t))

(use-package go-mode
  :defer t
  :mode "\\.go\\'"
  :interpreter "go"
  :hook
  ((go-mode . subword-mode)
   (go-mode . my|go-setup-env-vars)
   (go-mode . my|go-common-hook)
   (go-mode . my|ggtags-mode-enable))
  :bind
  (:map go-mode-map
        ("C-?" . #'comment-or-uncomment-region)))

(use-package go-guru
  :hook (go-mode . go-guru-hl-identifier-mode))

(use-package company-go
  :after (go-mode company)
  :hook
  (go-mode . company-mode)
  :init
  (add-company-backends!!
    :modes go-mode
    :backends company-go
    :variables company-go-show-annotation t))

(use-package go-eldoc
  :if gocode-executable-path
  :defer t
  :after go-mode
  :hook
  ((go-mode . eldoc-mode)
   (go-mode . go-eldoc-setup)))

(provide 'go-lang)
;;; go-lang.el ends here

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
