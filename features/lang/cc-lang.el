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

;;; Cmake

(use-package cmake-mode
  :mode ("CMakeLists\\.txt\\'" "\\.cmake\\'"))

(with-eval-after-load 'company
  (add-hook 'c++-mode-hook #'company-mode)
  (add-hook 'c-mode-hook #'company-mode))

;; Change the default way that Emacs handles indentation

(defun my/c-mode-hook ()
  (setq tab-width 4
        c-basic-offset 4
        indent-tabs-mode t)
  (c-set-offset 'substatement-open 0)   ; Curly braces alignment
  (c-set-offset 'case-label 4))         ; Switch case statements alignment

(add-hook 'c-mode-hook #'my/c-mode-hook)

(provide 'cc-lang)
;;; cc-lang.el ends here
