;;; langs-python.el --- Setup Python for Emacs. -*- lexical-binding: t; -*-

;; Copyright (C) 2019, 2020, 2021, 2022 Serghei Iakovlev <egrep@protonmail.ch>

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
;;   - flake8
;;   - autopep8

;;; Code:

(eval-when-compile
  (require 'directories))

(require 'f)

;; Silence linter
(declare-function
 projectile-project-name
 "projectile" (&optional PROJECT))

(use-package python
  :ensure nil
  :after company
  :custom
  (python-shell-interpreter "python3")
  (python-shell-interpreter-args "-i --simple-prompt --pprint")
  :hook (python-mode . company-mode))

(defun my/pyenv-activate-current-project ()
  "Activate pyenv version if .python-version file exists."
  (interactive)
  (f-traverse-upwards
   (lambda (path)
     (let ((pyenv-version-path (f-expand ".python-version" path))
           pyenv-current-version)
       (when (f-exists? pyenv-version-path)
           (progn
             (setq pyenv-current-version
                   (s-trim (f-read-text pyenv-version-path 'utf-8)))
             (pyenv-mode-set pyenv-current-version)
             (pyvenv-workon pyenv-current-version)))))))

;; I use pyenv on a daily basis.
(use-package pyenv-mode
  :if (executable-find "pyenv")
  :init
  (setenv "WORKON_HOME" "~/.pyenv/versions/")
  :custom
  ;; pyenv global 3.11.0
  (flycheck-python-pycompile-executable
   (expand-file-name "~/.pyenv/shims/python"))
  ;; pip install flake8
  (flycheck-python-flake8-executable
   (expand-file-name "~/.pyenv/shims/flake8"))
  ;; pip install mypy
  (flycheck-python-mypy-executable
   (expand-file-name "~/.pyenv/shims/mypy"))
  :config
  ;; I set `exec-path' from `$PATH' variable. Therefore,
  ;; there is no need to set this variable here.
  ;;
  ;; (add-to-list 'exec-path "~/.pyenv/shims")
  ;;
  ;; Start `pyenv-mode' as soon as possible.
  (pyenv-mode)
  (defun my|pyenv-init-hook ()
    "Set python version according to return value of pyenv global command."
    (let ((global-pyenv (replace-regexp-in-string
                         "\n" ""
                         (shell-command-to-string "pyenv global"))))
      (message "Setting pyenv version to %s" global-pyenv)
      (pyenv-mode-set global-pyenv)))

  (defun my|projectile-pyenv-mode-hook ()
    "Set pyenv version matching project name."
    (let ((project (projectile-project-name)))
      (if (and (fboundp 'pyenv-mode-versions)
               (member project (pyenv-mode-versions)))
          (pyenv-mode-set project)
        (pyenv-mode-unset))))
  :bind
  ("C-c p e" . my/pyenv-activate-current-project)
  :hook
  ((after-init . my|pyenv-init-hook)
   (projectile-switch-project . my|projectile-pyenv-mode-hook)))

(provide 'langs-python)
;;; langs-python.el ends here
