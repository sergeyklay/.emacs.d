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

(eval-when-compile
  (require 'company)
  (require 'show-point-mode))

(defun zephir-common-hook ()
  "The common hook to configure `zephir-mode'."

  ;; These modes are not ready to use with `zephir-mode'.
  (flycheck-mode -1)
  (eldoc-mode -1)

  ;; These options are common.
  (setq-local fill-column 120)
  (show-point-mode 1))

(use-package zephir-mode
  :mode "\\.zep\\'"
  :init
  (add-to-list 'company-backends
	       '(company-capf
		 company-dabbrev-code
		 company-yasnippet
		 company-files))
  :hook ((zephir-mode . zephir-common-hook)
	 (zephir-mode . subword-mode)
	 (zephir-mode . yas-minor-mode)
	 (zephir-mode . auto-fill-mode)
	 (zephir-mode . company-mode)
	 (zephir-mode . rainbow-delimiters-mode)))

(provide 'langs-zephir)
;;; langs-zephir.el ends here
