;;; cc-lang.el --- Add support for the C-family of languages. -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Serghei Iakovlev <sadhooklay@gmail.com>

;; Author: Serghei Iakovlev <sadhooklay@gmail.com>
;; URL: https://github.com/sergeyklay/.emacs.d
;;
;; This file is not part of GNU Emacs.
;;
;; License: GPLv3

;;; Commentary:

;; Add support for the C-family of languages for GNU Emacs.

;;; Code:

(require 'jump)

(eval-when-compile
  (require 'company))

(use-package cmake-mode
  :after company
  :mode "CMakeLists\\.txt\\'"
  :mode "\\.cmake\\'"
  :hook
  ((cmake-mode . company-mode))
  :init
  (add-to-list 'company-backends 'company-cmake))

(use-package c-eldoc
  :hook
  ((c-mode . c-turn-on-eldoc-mode)
   (c++-mode . c-turn-on-eldoc-mode))
  :config
  (setq c-eldoc-buffer-regenerate-time 60))

(use-package company-c-headers
  :after company
  :defer t
  :hook
  ((c-mode-common . company-mode))
  :init
  (add-to-list 'company-backends 'company-c-headers))

(defun my|cc-common-hook ()
  "Change the default way that Emacs handles indentation."
  (setq-local c-basic-offset 4)
  (setq-local indent-tabs-mode t)
  (setq-local tab-width 4)
  (c-set-offset 'substatement-open 0)
  (c-set-offset 'case-label 4)

  (local-set-key (kbd "C-c j") #'xref-find-definitions))

(add-hook 'c-mode-common-hook #'hs-minor-mode)
(add-hook 'c-mode-common-hook #'my|ggtags-mode-enable)
(add-hook 'c-mode-common-hook #'my|cc-common-hook)

(provide 'cc-lang)
;;; cc-lang.el ends here
