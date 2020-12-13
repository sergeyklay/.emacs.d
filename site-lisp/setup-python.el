;;; setup-python.el --- Setup Python for Emacs. -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2020 Serghei Iakovlev <egrep@protonmail.ch>

;; Author: Serghei Iakovlev <egrep@protonmail.ch>
;; URL: https://github.com/sergeyklay/.emacs.d
;;
;; This file is NOT part of GNU Emacs.
;;
;; License: GPLv3

;;; Commentary:

;; Configuration for Python language.

;;; Code:

(require 'directories)

(defvar jedi-custom-file (concat user-etc-dir "jedi-custom.el")
  "User-wide file to customize Jedi configuration.")

(use-package python-mode
  :ensure nil
  :after company
  :hook
  ((python-mode . company-mode)))

(use-package company-jedi
  :commands company-jedi
  :custom
  (jedi:use-shortcuts t)
  :config
  (defun company-jedi-hook()
    "Add `company-jedi' to the backends of `company-mode'."
    (add-to-list 'company-backends 'company-jedi))
  :hook ((python-mode . jedi:setup)
         (python-mode . company-jedi-hook)))

;; Tidy up (require python package 'autopep8').
(use-package py-autopep8
  :defer t
  :custom
  (py-autopep8-options '("--max-line-length=80"))
  :hook (python-mode . py-autopep8-enable-on-save))

(with-eval-after-load 'company-jedi
  (when (file-exists-p jedi-custom-file)
    (load jedi-custom-file nil 'nomessage)))

(provide 'setup-python)
;;; setup-python.el ends here
