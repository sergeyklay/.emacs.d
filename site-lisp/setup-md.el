;;; setup-md.el --- Add support for the Markdown. -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2020 Serghei Iakovlev <egrep@protonmail.ch>

;; Author: Serghei Iakovlev <egrep@protonmail.ch>
;; URL: https://github.com/sergeyklay/.emacs.d
;;
;; This file is NOT part of GNU Emacs.
;;
;; License: GPLv3

;;; Commentary:

;; Configuration of the Markdown / GFM modes.

;;; Code:

(require 'rx)

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
   '("sh" "python" "js" "lisp" "elisp" "zep"))
  (markdown-gfm-uppercase-checkbox t)
  (markdown-fontify-code-blocks-natively t)
  (markdown-hide-urls nil)
  :hook
  (markdown-mode . auto-fill-mode)
  :config
  (add-to-list 'auto-mode-alist
	       `(,gfm-patterns . gfm-mode)))

(provide 'setup-md)
;;; setup-md.el ends here
