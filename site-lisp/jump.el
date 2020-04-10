;;; jump.el --- GNU Emacs configuration for tags. -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2020 Serghei Iakovlev <egrep@protonmail.ch>

;; Author: Serghei Iakovlev <egrep@protonmail.ch>
;; URL: https://github.com/sergeyklay/.emacs.d
;;
;; This file is NOT part of GNU Emacs.
;;
;; License: GPLv3

;;; Commentary:

;; Tags realated features for GNU Emacs.

;;; Code:

(require 'directories)
(require 'utils)

;;;; Bookmark

(use-package bookmark
  :ensure nil
  :custom
  (bookmark-default-file
   (concat user-etc-dir "bookmarks.el")))

;;;; RTags

;; Do not install the following packages using MELPA.
;; These packages should be installed by hand using `make install' from
;; rtags source directory.
;;
;; For more see URL `https://github.com/Andersbakken/rtags/issues/1318'.

(defun rtags-eldoc-function ()
  "Eldoc documentation function to use for `c-mode' as well as `c++-mode'."
  (let ((summary (rtags-get-summary-text)))
    (and summary
         (fontify-string
          (replace-regexp-in-string
           "{[^}]*$" ""
           (mapconcat
            (lambda (str) (if (= 0 (length str)) "//" (string-trim str)))
            (split-string summary "\r?\n")
            " "))
          major-mode))))

(use-package rtags
  :ensure nil
  :if (executable-find "rdm")
  :defer 3
  :custom
  ;; Whether RTags automatically will restart diagnostics.
  (rtags-autostart-diagnostics t)
  ;; Path to RTags executables.
  (rtags-path (directory-file-name
	       (file-name-directory (executable-find "rdm")))))

(defun rtags-setup ()
  "Common hook to setup `rtags'."
  (setq-local eldoc-documentation-function #'rtags-eldoc-function)
  (rtags-enable-standard-keybindings)
  (rtags-start-process-unless-running))

(use-package company-rtags
  :ensure nil
  :if (executable-find "rdm")
  :after company rtags)

(defun company-rtags-setup ()
  "Configure `company-backends' for `company-rtags'."
  (delete 'company-semantic company-backends)
  ;; Whether completions are enabled.
  (setq rtags-completions-enabled t)
  (push '(company-rtags :with company-yasnippet) company-backends))

(use-package flycheck-rtags
  :ensure nil
  :if (executable-find "rdm")
  :after flycheck rtags)

(defun flycheck-rtags-setup ()
  "Configure flycheck-rtags."
  ;; Do not enable `eldoc' here (it is enabled in separated configuration).
  (flycheck-select-checker 'rtags)
  ;; RTags creates more accurate overlays.
  (setq-local flycheck-highlighting-mode nil)
  (setq-local flycheck-check-syntax-automatically nil))

(provide 'jump)
;;; jump.el ends here
