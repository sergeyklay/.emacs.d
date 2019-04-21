;;; web-lang.el --- Add support for the Web-based languages. -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Serghei Iakovlev <sadhooklay@gmail.com>

;; Author: Serghei Iakovlev <sadhooklay@gmail.com>
;; URL: https://github.com/sergeyklay/.emacs.d
;;
;; This file is not part of GNU Emacs.
;;
;; License: GPLv3

;;; Commentary:

;; Add support for the Web-based languages for GNU Emacs.

;;; Code:

(use-package web-mode
  :ensure t
  :mode (("\\.html?\\'"      . web-mode)
         ("\\.tpl\\'"        . web-mode)
         ("\\.tpl\\.php\\'"  . web-mode)
         ("\\.phtml\\'"      . web-mode)
         ("\\.volt\\'"       . web-mode)
         ("\\.twig\\'"       . web-mode)
         ("\\.[agj]sp\\'"    . web-mode)
         ("\\.as[cp]x\\'"    . web-mode)
         ("\\.erb\\'"        . web-mode)
         ("\\.mustache\\'"   . web-mode)
         ("\\.handlebars\\'" . web-mode)
         ("\\.hbs\\'"        . web-mode)
         ("\\.djhtml\\'"     . web-mode))
  :init
  ;; associate an engine
  (setq web-mode-engines-alist
        '(("php"   . "\\.phtml\\'")
          ("blade" . "\\.blade\\.")))

  ;; indentation
  (setq-default indent-tabs-mode nil)
  (setq web-mode-markup-indent-offset 4
        web-mode-code-indent-offset 4))

(use-package emmet-mode
  :commands emmet-mode
  :init
  (my/add-to-hooks
   #'emmet-mode
   '(html-mode-hook
     sgml-mode-hook
     web-mode-hook
     css-mode-hook)))

(provide 'web-lang)
;;; web-lang.el ends here
