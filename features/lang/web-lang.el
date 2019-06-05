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

(require 'core-defuns)

(defun my|common-web-hook ()
  "Common hook to configure `web-mode'."
  (setq indent-tabs-mode nil
        fill-column 120))

(use-package web-mode
  :mode (("\\.eco\\'"        . web-mode)
	 ("\\.eex\\'"        . web-mode)
	 ("\\.ejs\\'"        .  web-mode)
	 ("\\.html?\\'"      . web-mode)
         ("\\.[agj]sp\\'"    . web-mode)
         ("\\.as[cp]x\\'"    . web-mode)
         ("\\.djhtml\\'"     . web-mode)
         ("\\.erb\\'"        . web-mode)
         ("\\.handlebars\\'" . web-mode)
         ("\\.hbs\\'"        . web-mode)
         ("\\.mustache\\'"   . web-mode)
         ("\\.phtml\\'"      . web-mode)
         ("\\.tpl\\'"        . web-mode)
         ("\\.tpl\\.php\\'"  . web-mode)
         ("\\.twig\\'"       . web-mode)
         ("\\.volt\\'"       . web-mode))
  :init
  (setq web-mode-engines-alist
        '(("php"   . "\\.phtml\\'")
          ("blade" . "\\.blade\\.")))
  :config
  (setq web-mode-markup-indent-offset 4
        web-mode-css-indent-offset 4
	web-mode-code-indent-offset 4)

  ;; highlight enclosing tags of the element under cursor
  (setq web-mode-enable-current-element-highlight t)
  :hook
  (web-mode . my|common-web-hook))

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
