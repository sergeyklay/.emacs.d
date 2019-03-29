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

(defun cc--common-hook ()
  "Change the default way that Emacs handles indentation."
  (validate-setq tab-width 4
                 c-basic-offset 4
                 indent-tabs-mode t)

  (c-set-offset 'substatement-open 0)
  (c-set-offset 'case-label 4))

(add-hook 'c-mode-hook #'cc--common-hook)

(defun cc--headers-hook ()
  "Enable `company-mode' and add `company-c-headers' to the `company-backends'."
  (make-local-variable 'company-backends)
  (add-to-list 'company-backends 'company-c-headers))

(use-package company-c-headers
  :after company
  :hook
  ((c-mode c++mode) . cc--headers-hook))

(provide 'cc-lang)
;;; cc-lang.el ends here
