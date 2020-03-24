;;; langs-zephir.el --- Zephir related configuration. -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2020 Serghei Iakovlev <egrep@protonmail.ch>

;; Author: Serghei Iakovlev <egrep@protonmail.ch>
;; URL: https://github.com/sergeyklay/.emacs.d
;;
;; This file is NOT part of GNU Emacs.
;;
;; License: GPLv3

;;; Commentary:

;; This file provides my own Zephir configuration and relies on private mode
;; symlinked in `user-private-dir'.

;;; Code:

(require 'directories)

(when (file-directory-p (concat user-private-dir "zephir-mode"))
  (require 'zephir-mode)

  (customize-set-variable
   '(zephir-speedbar-config t))

  (defun zephir-common-hook ()
    "The common hook to configure `zephir-mode'."
    ;; These modes are not ready to use with `zephir-mode'.
    (company-statistics-mode -1)
    (company-mode -1)
    (flycheck-mode -1)
    (eldoc-mode -1)

    ;; These options are common.
    (setq-local fill-column 120)
    (setq-local indent-tabs-mode nil))

  (add-hook 'zephir-mode-hook #'subword-mode)
  (add-hook 'zephir-mode-hook #'yas-minor-mode)
  (add-hook 'zephir-mode-hook #'auto-fill-mode)
  (add-hook 'zephir-mode-hook #'zephir-common-hook))

(provide 'langs-zephir)
;;; langs-zephir.el ends here
