;;; docs.el --- Setting up documentation features. -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Serghei Iakovlev <sadhooklay@gmail.com>

;; Author: Serghei Iakovlev <sadhooklay@gmail.com>
;; URL: https://github.com/sergeyklay/.emacs.d
;;
;; This file is not part of GNU Emacs.
;;
;; License: GPLv3

;;; Commentary:

;; Setting up documentation features.

;;; Code:

(use-package eldoc
  :ensure nil
  :defer t
  :diminish (eldoc-mode . " Ⓔ")
  :hook
  ((eval-expression-minibuffer . eldoc-mode)
   (ielm-mode . eldoc-mode)))

(provide 'docs)
;;; docs.el ends here

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
