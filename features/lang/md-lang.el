;;; md-lang.el --- Add support for the Markdown. -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Serghei Iakovlev

;; Author: Serghei Iakovlev (concat "sadhooklay" "@" "gmail" ".com")
;; URL: https://github.com/sergeyklay/.emacs.d
;;
;; This file is not part of GNU Emacs.
;;
;; License: GPLv3

;;; Commentary:

;; Add support for the Markdown.

;;; Code:

;; Markdown mode

(use-package markdown-mode
  :mode "/README$"
  :mode "\\.m\\(d\\|arkdown\\)$"
  :mode ("/README\\.md$" . gfm-mode)
  :config
  (setq markdown-command "cmark"
        markdown-enable-wiki-links t
        markdown-enable-math t
        markdown-italic-underscore t
        markdown-asymmetric-header t
        markdown-make-gfm-checkboxes-buttons t
        markdown-gfm-additional-languages '("sh")
        markdown-fontify-code-blocks-natively t
        markdown-hide-urls nil))

(use-package markdown-toc
  :commands markdown-toc-generate-toc)

(provide 'md-lang)
;;; md-lang.el ends here
