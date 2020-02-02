;;; build-tools.el --- Build tools support. -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2020 Serghei Iakovlev <egrep@protonmail.ch>

;; Author: Serghei Iakovlev <egrep@protonmail.ch>
;; URL: https://github.com/sergeyklay/.emacs.d
;;
;; This file is NOT part of GNU Emacs.
;;
;; License: GPLv3

;;; Commentary:

;; Support of various build tools.

;;; Code:

(use-package cmake-mode
  :mode "CMakeLists\\.txt\\'"
  :mode "\\.cmake\\'"
  :hook
  ((cmake-mode . company-mode))
  :config
  (setq cmake-tab-width tab-width))

(use-package cmake-font-lock
  :hook (cmake-mode . cmake-font-lock-activate))

(use-package company-cmake
  :ensure nil                 ; Included in company
  :after (company cmake-mode)
  :config
  (push 'company-cmake company-backends))

(use-package make-mode
  :ensure nil
  :defer t
  :after (company)
  :hook (makefile-mode . company-mode))

(provide 'build-tools)
;;; build-tools.el ends here