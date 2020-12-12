;;; search-tools.el --- Setup search tools for Emacs. -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2020 Serghei Iakovlev <egrep@protonmail.ch>

;; Author: Serghei Iakovlev <egrep@protonmail.ch>
;; URL: https://github.com/sergeyklay/.emacs.d
;;
;; This file is NOT part of GNU Emacs.
;;
;; License: GPLv3

;;; Commentary:

;; Setup search tools for Emacs.

;;; Code:

(use-package ag
  :if (executable-find "ag")
  :defer t)

(use-package helm-ag
  :if (executable-find "ag")
  :defer t
  :after helm
  :commands
  (helm-ag
   helm-ag-buffers
   helm-ag-project-root
   helm-ag-this-file)
  :bind ("C-c h a" . helm-ag))

(provide 'search-tools)
;;; search-tools.el ends here
