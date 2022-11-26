;;; langs-lisp.el --- Lisp-family of languages. -*- lexical-binding: t; -*-

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

;; Add support for the Lisp-family of languages.

;;; Code:

(eval-when-compile
  (require 'company)
  (require 'show-point-mode))

(defconst sbcl-executable-path (executable-find "sbcl")
  "The sbcl executable path on this system.")

;; The Superior Lisp Interaction Mode for Emacs.
(use-package slime
  :defer t
  :if sbcl-executable-path
  :commands slime-mode
  :hook
  ((lisp-mode . slime-mode))
  :custom
  (slime-complete-symbol*-fancy t)
  (slime-completion-at-point-functions 'slime-fuzzy-complete-symbol)
  (slime-net-coding-system 'utf-8-unix)
  :config
  (setq inferior-lisp-program sbcl-executable-path)
  (slime-setup '(slime-asdf
		 slime-fancy
		 slime-indentation
		 slime-sbcl-exts
		 slime-scratch)))

(use-package slime-company
  :after (slime company)
  :defer t
  :hook
  ((slime-mode . company-mode))
  :init
  (slime-setup '(slime-fancy slime-company)))

(use-package ielm
  :ensure nil
  :hook
  ((ielm-mode . company-mode)
   (ielm-mode . turn-on-eldoc-mode)
   (ielm-mode . rainbow-delimiters-mode)))

(use-package lisp-mode
  :ensure nil
  :hook
  ((lisp-mode . company-mode)
   (lisp-mode . turn-on-eldoc-mode)
   (lisp-mode . rainbow-delimiters-mode)))

(use-package elisp-mode
  :ensure nil
  :hook
  ((emacs-lisp-mode . show-point-mode)
   (emacs-lisp-mode . company-mode)
   (emacs-lisp-mode . turn-on-eldoc-mode)
   (emacs-lisp-mode . rainbow-delimiters-mode)

   (lisp-interaction-mode . company-mode)
   (lisp-interaction-mode . turn-on-eldoc-mode)
   (lisp-interaction-mode . rainbow-delimiters-mode))
  :bind
  (:map emacs-lisp-mode-map
        ("C-c C-b" . #'eval-buffer)))

(with-eval-after-load 'company
  (add-to-list 'company-backends
	       '(company-elisp
		 company-capf
		 company-dabbrev-code
		 company-yasnippet
		 company-files)))

(use-package eros
  :defer t
  :hook
  ((ielm-mode       . eros-mode)
   (emacs-lisp-mode . eros-mode))
  :custom
  (eros-eval-result-prefix "∴ "))

(use-package cask-mode
  :mode "Cask")

(use-package flycheck-cask
  :after flycheck-mode
  :hook
  (flycheck-mode . flycheck-cask-setup))

(use-package buttercup
  :defer t)

(use-package undercover
  :defer t)

(provide 'langs-lisp)
;;; langs-lisp.el ends here
