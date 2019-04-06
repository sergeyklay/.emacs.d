;;; cc-lang.el --- Add support for the C-family of languages. -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Serghei Iakovlev

;; Author: Serghei Iakovlev (concat "sadhooklay" "@" "gmail" ".com")
;; URL: https://github.com/sergeyklay/.emacs.d
;;
;; This file is not part of GNU Emacs.
;;
;; License: GPLv3

;;; Commentary:

;; Add support for the C-family of languages for the GUN Emacs.

;;; Code:

;;; Packages

(use-package cmake-mode
  :mode ("CMakeLists\\.txt\\'" "\\.cmake\\'"))

(use-package c-eldoc
  :init
  (add-hook 'c-mode-hook 'c-turn-on-eldoc-mode)
  (add-hook 'c++-mode-hook 'c-turn-on-eldoc-mode)
  :config
  (setq c-eldoc-buffer-regenerate-time 60))

(use-package company-c-headers
  :after company
  :init
  (add-hook 'c-mode-hook #'my--cc-headers-hook)
  (add-hook 'c++-mode-hook #'my--cc-headers-hook))

;;; Defuns

(defun my--cc-common-hook ()
  "Change the default way that Emacs handles indentation."
  (validate-setq tab-width 4
                 c-basic-offset 4
                 indent-tabs-mode t)

  (c-set-offset 'substatement-open 0)
  (c-set-offset 'case-label 4))

(defun my--cc-headers-hook ()
  "Enable `company-mode' and add `company-c-headers' to the `company-backends'."
  (make-local-variable 'company-backends)
  (add-to-list 'company-backends 'company-c-headers))

(defun my--cc-tags ()
  "Enable ggtags-mode for C/C++/Java modes."
  (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
    (progn
      (require 'ggtags)
      (ggtags-mode 1))))

;;; Hooks

(add-hook 'c-mode-hook #'cc--common-hook)
(add-hook 'c-mode-common-hook #'my--cc-tags)

(provide 'cc-lang)
;;; cc-lang.el ends here
