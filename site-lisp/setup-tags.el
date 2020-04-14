;;; setup-tags.el --- Setup tags. -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2020 Serghei Iakovlev <egrep@protonmail.ch>

;; Author: Serghei Iakovlev <egrep@protonmail.ch>
;; URL: https://github.com/sergeyklay/.emacs.d
;;
;; This file is NOT part of GNU Emacs.
;;
;; License: GPLv3

;;; Commentary:

;; Configuration for tags and code navigation.

;;; Code:

(require 'cl-macs)
(require 'utils)
(require 'ecfg)
(require 'directories)

;;;; Constants

(defconst global-executable-path (executable-find "global")
  "The global executable path on this system.")

(defconst gtags-executable-path (executable-find "gtags")
  "The gtags executable path on this system.")

(defconst lid-executable-path (executable-find "lid")
  "The lid executable path on this system.")

(defconst mkid-executable-path (executable-find "mkid")
  "The mid executable path on this system.")

(defconst rdm-executable-path (executable-find "rdm")
  "The rdm executable path on this system.")

(unless (and global-executable-path gtags-executable-path)
  (warn "Cannot find necessary installation of GNU GLOBAL"))

(unless (and mkid-executable-path lid-executable-path)
  (warn "Cannot find necessary installation of ID utils"))

(unless rdm-executable-path
  (warn "Cannot find necessary installation of RTags"))

;;;; Etags

(use-package etags
  :ensure nil
  :defer t
  :custom
  ;; Make Emacs reload the TAGS file automatically.
  (tags-revert-without-query 1)
  ;; Whether tags operations should be case-sensitive.
  (tags-case-fold-search nil)
  ;; Never “Keep current list of tags tables also”.
  (tags-add-tables nil))

;;;; Ggtags

;; A front-end for accessing the gtags-generated tags.
;; For more see URL `https://github.com/leoliu/ggtags'.
;;
;; To be efficient, the host system tool “gtags” (called “global”) should be
;; compiled from source with “exuberant-ctags”.  Also for proper parsing, the
;; Python package “pygments” should be installed.  For better and faster lookup
;; support, “id-utils” should be inastalled so a database instead of a text
;; file is used.
;;
;; The full requirements list to use “gtags” is:
;;   - exuberant-ctags
;;   - pygment
;;   - id-utils
;;   - global (6.5 or later)
;;
;; Note: There's an Emacs tool called “ctags” that is not the same as
;; exuberant-ctags' even though the executable is named the same.  Be careful
;; that's not in your path first.  The one possible solution to resolve name
;; collision is to pass the following option to Emacs' confige script:
;;
;;   --program-transform-name='s/^ctags$/ctags.emacs/'

(defun setup-ggtags-environment ()
  "Setup buffer local enviroment variables for `ggtags'."
  (pcase (ecfg-get :ggtags-process-environment)
    ((and (pred listp) env)
     (setq-local ggtags-process-environment env))
    ((and (pred stringp) env)
     (setq-local ggtags-process-environment '(env)))
    ((pred null)
     nil)
    (f (warn "Unknown ggtags env type: %S (%s)" f (type-of f)))))

(use-package ggtags
  :if (and global-executable-path gtags-executable-path)
  :commands (ggtags-mode)
  :custom
  ;; Auto-pdate GTAGS on each save.
  (ggtags-update-on-save t)
  ;; Do not auto-highlight tag at point.
  (ggtags-highlight-tag nil)
  ;; Enabling nearness requires global 6.5.+
  (ggtags-sort-by-nearness t)
  ;; The over size limit for the  GTAGS file.
  (ggtags-oversize-limit (* 100 1024 1024))
  ;; Generate the idutils DB.
  (ggtags-use-idutils (not (null mkid-executable-path)))
  ;; The directory to search GNU GLOBAL executables.
  (ggtags-executable-directory
   (directory-file-name (file-name-directory global-executable-path)))
  :bind (:map ggtags-mode-map
         ("C-c g s" . ggtags-find-other-symbol)
         ("C-c g h" . ggtags-view-tag-history)
         ("C-c g r" . ggtags-find-reference)
         ("C-c g f" . ggtags-find-file)
         ("C-c g c" . ggtags-create-tags)
         ("C-c g u" . ggtags-update-tags)
         ("C-c <"   . ggtags-prev-mark)
         ("C-c >"   . ggtags-next-mark))
  :hook (ggtags-mode . setup-ggtags-environment)
  :config
  (when lid-executable-path
    (define-key ggtags-mode-map (kbd "M-]") #'ggtags-idutils-query)))

;;;; Rtags

;; Note: Do not install the following packages using MELPA.
;; These packages should be installed by hand using `make install' from
;; rtags source directory.
;;
;; For more see URL `https://github.com/Andersbakken/rtags/issues/1318'.

(use-package rtags
  :ensure nil
  :if rdm-executable-path
  :commands (rtags-mode)
  :custom
  ;; Whether RTags automatically will restart diagnostics.
  (rtags-autostart-diagnostics t)
  ;; Path to RTags executables.
  (rtags-path (directory-file-name
               (file-name-directory rdm-executable-path))))

(use-package company-rtags
  :ensure nil
  :if rdm-executable-path
  :after company rtags)

(use-package flycheck-rtags
  :ensure nil
  :if rdm-executable-path
  :after flycheck rtags)

(use-package helm-rtags
  :ensure nil
  :if rdm-executable-path
  :after rtags
  :custom
  (rtags-display-result-backend 'helm))

;;;; Utils

(defun rtags--eldoc-function ()
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

(defun company-rtags-setup ()
  "Configure `company-backends' for `company-rtags'."
  (delete 'company-semantic company-backends)
  ;; Whether completions are enabled.
  (setq rtags-completions-enabled t)
  (push '(company-rtags :with company-yasnippet) company-backends))

(defun flycheck-rtags-setup ()
  "Configure `flycheck-rtags'."
  ;; Do not enable `eldoc' here (it is enabled in separated configuration).
  (flycheck-select-checker 'rtags)
  ;; RTags creates more accurate overlays.
  (setq-local flycheck-highlighting-mode nil)
  (setq-local flycheck-check-syntax-automatically nil))

(defun tags-enable-rtags ()
  "Common function to enable and configure `rtags'."
  (rtags-mode 1)

  ;; TODO(serghei): Move outside
  (company-rtags-setup)
  (flycheck-rtags-setup)

  (setq-local eldoc-documentation-function
              #'rtags--eldoc-function)

  (rtags-enable-standard-keybindings)
  (rtags-start-process-unless-running))

(defun tags-enable-ggtags ()
  "Common function to enable and configure `ggtags'."
  (ggtags-mode 1)

  (setq-local imenu-create-index-function
              #'ggtags-build-imenu-index)

  (setq-local eldoc-documentation-function
              #'ggtags-eldoc-function))

(defun setup-tags-fronted ()
  "Enable tags fronted using project configuration."
  (pcase (ecfg-get :tags-frontend)
    ;; string
    ("ggtags"    (tags-enable-ggtags))
    ("rtags"     (tags-enable-rtags))
    ;; symbol
    ('ggtags     (tags-enable-ggtags))
    ('rtags      (tags-enable-rtags))
    ;; nil
    ((pred null) nil)
    ;; unknown
    (f           (warn "Unknown tags fronted type: %S (%s)" f (type-of f)))))

(defsubst tags--apply-to-buffer (buffer project-root)
  "Apply tags configuration to buffer BUFFER.
The PROJECT-ROOT variable should point to the current project root."
  (with-current-buffer buffer
    (when (and (bufferp buffer)
               (buffer-file-name)
               (equal (projectile-project-root) project-root))
      (setup-tags-fronted))))

(defun my/select-tags-frontend (frontend)
  "Setup project wide FRONTEND to source code tagging system."
  (interactive
   (list (completing-read "Tags frontend: " '(ggtags rtags disable))))
  (let ((project-root (projectile-project-root)))
    (when (string= frontend "disable")
      (setq frontend nil))
    (ecfg-set :tags-frontend frontend)
    (cl-dolist (buffer (buffer-list))
      (funcall #'tags--apply-to-buffer buffer project-root))))

(provide 'setup-tags)

;;; setup-tags.el ends here
