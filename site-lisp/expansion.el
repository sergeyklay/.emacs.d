;;; expansion.el --- Expansions configuration. -*- lexical-binding: t; -*-

;; Copyright (C) 2019, 2020 Serghei Iakovlev <egrep@protonmail.ch>

;; Author: Serghei Iakovlev <egrep@protonmail.ch>
;; URL: https://github.com/sergeyklay/.emacs.d

;; This file is NOT part of Emacs.

;;;; License

;; This file is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <https://www.gnu.org/licenses/>.

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
  :custom
  (yas-indent-line 'fixed)
  :config
  (setq yas-verbosity (if emacs-debug-mode 3 0))
  (add-to-list
   'yas-snippet-dirs (concat user-emacs-directory "snippets"))
  (when (fboundp 'yas-reload-all)
    (yas-reload-all))
  (yas-global-mode t))

(provide 'expansion)
;;; expansion.el ends here
