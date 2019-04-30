;;; md-lang.el --- Add support for the Markdown. -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Serghei Iakovlev <sadhooklay@gmail.com>

;; Author: Serghei Iakovlev <sadhooklay@gmail.com>
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
  :mode ("/README\\(?:\\.\\(?:markdown\\|md\\)\\)?\\'" . gfm-mode)
  :config
  (setq markdown-command "pandoc"
        markdown-enable-wiki-links t
        markdown-enable-math t
        markdown-italic-underscore t
        markdown-asymmetric-header t
        markdown-make-gfm-checkboxes-buttons t
        markdown-gfm-additional-languages '("sh")
        markdown-gfm-uppercase-checkbox t
        markdown-fontify-code-blocks-natively t
        markdown-hide-urls nil)
  :hook
  (markdown-mode . auto-fill-mode)
  :bind
  (:map markdown-mode-map
        ("M-n"   . nil)
        ("M-p"   . nil)
        ("C-x n" . #'markdown-next-link)
        ("C-x p" . #'markdown-previous-link)))

(use-package markdown-toc
  :commands markdown-toc-generate-toc)

(provide 'md-lang)
;;; md-lang.el ends here
