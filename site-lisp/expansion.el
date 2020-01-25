;;; expansion.el --- Expansions configuration. -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2020 Serghei Iakovlev <egrep@protonmail.ch>

;; Author: Serghei Iakovlev <egrep@protonmail.ch>
;; URL: https://github.com/sergeyklay/.emacs.d
;;
;; This file is NOT part of GNU Emacs.
;;
;; License: GPLv3

;;; Commentary:

;; Configuration of the expansions like abbrev, snippets and so on.

;;; Code:

(require 'prelude)
(require 'directories)

(use-package abbrev
  :ensure nil
  :custom
  (save-abbrev 'silently)
  (abbrev-file-name (concat user-etc-dir "abbrev-defs.el"))
  :hook
  ((text-mode prog-mode) . abbrev-mode))

(use-package yasnippet
  :diminish yas-minor-mode
  :defer 20
  :custom
  (yas-indent-line 'fixed)
  :config
  (setq yas-verbosity (if emacs-debug-mode 3 0))
  (add-to-list
   'yas-snippet-dirs (concat user-emacs-directory "snippets"))
  (when (fboundp 'yas-reload-all)
    (yas-reload-all))
  (yas-global-mode t))

(use-package ivy-yasnippet
  :defer 20
  :after (yasnippet ivy))

(provide 'expansion)
;;; expansion.el ends here
