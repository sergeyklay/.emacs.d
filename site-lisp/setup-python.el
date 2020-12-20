;;; setup-python.el --- Setup Python for Emacs. -*- lexical-binding: t; -*-

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

;; Configuration for Python language.
;;
;; Recomended Python packages are:
;;   - ipython
;;   - jedi
;;   - flake8
;;   - autopep8

;;; Code:

(require 'directories)

(defconst jedi-custom-file (concat user-etc-dir "jedi-custom.el")
  "User-wide file to customize Jedi configuration.")

(use-package python
  :ensure nil
  :after company
  :custom
  (python-shell-interpreter-args "-i --simple-prompt --pprint")
  (python-shell-interpreter "ipython")
  :hook (python-mode . company-mode))

(use-package company-jedi
  :commands company-jedi
  :custom
  (jedi:use-shortcuts t)
  (jedi:complete-on-dot t)
  :config
  (defun company-jedi-hook()
    "Add `company-jedi' to the backends of `company-mode'."
    (add-to-list 'company-backends 'company-jedi))
  :hook ((python-mode . jedi:setup)
         (python-mode . company-jedi-hook)))

;; Load custom Jedi configuration
(with-eval-after-load 'company-jedi
  (when (file-exists-p jedi-custom-file)
    (load jedi-custom-file nil 'nomessage)))

;; Tidy up (require python package 'autopep8').
(use-package py-autopep8
  :defer t
  :custom
  (py-autopep8-options '("--max-line-length=80"))
  :hook (python-mode . py-autopep8-enable-on-save))

(provide 'setup-python)
;;; setup-python.el ends here
