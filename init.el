;;; init.el -- Initialization file
;; Serghei Iakovlev
;; 4 March 2019

;;; Commentary:
;; This file is used to set up the packages sources and then call
;; an org-babel tangle in order to load a literate configuration.

;;; Code:

(require 'server)
(unless (server-running-p)
       (server-start))

(require 'package)

(setq package-archives '(("org" . "http://orgmode.org/elpa/")
                         ("melpa" . "http://melpa.org/packages/")
                         ("melpa-stable" . "http://stable.melpa.org/packages/")
                         ("gnu" . "https://elpa.gnu.org/packages/")))

(setq package-enable-at-startup nil)
(package-initialize)

(defconst user-init-dir
  (cond ((boundp 'user-emacs-directory)
         user-emacs-directory)
        ((boundp 'user-init-directory)
         user-init-directory)
        (t "~/.emacs.d/")))

; See: https://emacs.stackexchange.com/a/3147/16592
(org-babel-load-file
 (expand-file-name "settings.org" user-init-dir))

;;; init.el ends here
