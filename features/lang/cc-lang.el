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

(use-package cmake-mode
  :mode "CMakeLists\\.txt\\'"
  :mode "\\.cmake\\'"
  :init
  (add-company-backends!!
    :modes cmake-mode
    :backends company-cmake))

(use-package c-eldoc
  :hook
  ((c-mode . c-turn-on-eldoc-mode)
   (c++-mode . c-turn-on-eldoc-mode))
  :config
  (setq c-eldoc-buffer-regenerate-time 60))

(use-package company-c-headers
  :after company
  :defer t
  :init
  (add-company-backends!!
    :modes c-mode-common
    :backends company-c-headers))

(defun my|cc-common-hook ()
  "Change the default way that Emacs handles indentation."
  (setq tab-width 4
        c-basic-offset 4
        indent-tabs-mode t)
  (c-set-offset 'substatement-open 0)
  (c-set-offset 'case-label 4)

  (hs-minor-mode t)
  (my|ggtags-mode-enable))

(add-hook 'c-mode-common-hook #'my|cc-common-hook)

(provide 'cc-lang)
;;; cc-lang.el ends here

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
