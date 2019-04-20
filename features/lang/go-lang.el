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
  (add-hook 'before-save-hook 'gofmt-before-save t)
  ; gofmt uses invokes goimports
  (setq gofmt-command "goimports")
  ; set compile command default
  (if (not (string-match "go" compile-command))
      (set (make-local-variable 'compile-command)
           "go build -v && go test -v && go vet")))

(use-package go-mode
  :defer t
  :mode "\\.go\\'"
  :interpreter "go"
  :hook
  ((go-mode . subword-mode)
   (go-mode . my|go-setup-env-vars)
   (go-mode . my|go-common-hook))
  :bind
  (:map go-mode-map
        ("C-?" . #'comment-or-uncomment-region)
        ("M-." . #'godef-jump)
        ("M-*" . #'pop-tag-mark)
        ("M-p" . #'compile)
        ("M-P" . #'recompile)
        ("M-]" . #'next-error)
        ("M-[" . #'previous-error)))

(use-package go-guru
  :after go-mode
  ;; highlight identifiers
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
