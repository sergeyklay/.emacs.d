;;; langs-markup.el --- Markup syntax configuration. -*- lexical-binding: t; -*-

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

;; Markup syntax configuration.

;;; Code:

(require 'rx)

;;;; Constants

;;;; Constants

(defconst pandoc-executable-path (executable-find "pandoc")
  "The pandoc executable path on this system.")

(defconst gfm-patterns
  (rx (or "README" "CONTRIBUTTING" "BACKERS"
          (: "CHANGELOG" (? "-" (+ (in "." digit))))
          "CODE_OF_CONDUCT"
          "PULL_REQUEST_TEMPLATE"
          "pull_request_template"
          "bug_report"
          "feature_request")
      "."
      (or "markdown" "md")
      string-end)
  "Regexp to match files with GFM format.")

;;;; reStructuredText mode

(use-package rst
  :ensure nil
  :mode (("\\.rst\\'" . rst-mode)
         ("\\.rest\\'" . rst-mode)))

;;;; Markdown mode

(use-package markdown-mode
  :mode "\\.\\(?:m\\(?:arkdown\\|d\\)\\)\\'"
  :custom
  (markdown-command
   (if pandoc-executable-path "pandoc" nil))
  (markdown-enable-wiki-links t)
  (markdown-enable-math t)
  (markdown-italic-underscore t)
  (markdown-asymmetric-header t)
  (markdown-make-gfm-checkboxes-buttons t)
  (markdown-gfm-additional-languages
   '("sh" "python" "js" "lisp" "elisp"))
  (markdown-gfm-uppercase-checkbox t)
  (markdown-fontify-code-blocks-natively t)
  (markdown-hide-urls nil)
  :hook
  (markdown-mode . auto-fill-mode)
  :config
  (add-to-list 'auto-mode-alist
	       `(,gfm-patterns . gfm-mode)))

(provide 'langs-markup)
;;; langs-markup.el ends here
