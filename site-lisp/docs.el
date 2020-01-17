;;; docs.el --- Setting up documentation features. -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2020 Serghei Iakovlev <egrep@protonmail.ch>

;; Author: Serghei Iakovlev <egrep@protonmail.ch>
;; URL: https://github.com/sergeyklay/.emacs.d
;;
;; This file is NOT part of GNU Emacs.
;;
;; License: GPLv3

;;; Commentary:

;; Setting up documentation features.

;;; Code:

;;;; Eldoc customizations.

(use-package eldoc
  :ensure nil
  :defer t
  :commands turn-on-eldoc-mode
  :hook
  ((eval-expression-minibuffer . eldoc-mode)
   (ielm-mode . eldoc-mode))
  :config
  (setq
   ;; Show ElDoc messages in the echo area in 1/3 of a scend.
   eldoc-idle-delay 0.3
   ;; Always truncate ElDoc messages to one line.
   ;; This prevents the echo area from resizing itself unexpectedly
   ;; when point is on a variable with a multiline docstring.
   eldoc-echo-area-use-multiline-p nil
   ;; Don’t show ElDoc in the mode line.
   eldoc-minor-mode-string nil))

(provide 'docs)
;;; docs.el ends here
