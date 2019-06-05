;;; http-tools.el --- Various http tools. -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Serghei Iakovlev <sadhooklay@gmail.com>

;; Author: Serghei Iakovlev <sadhooklay@gmail.com>
;; URL: https://github.com/sergeyklay/.emacs.d
;;
;; This file is not part of GNU Emacs.
;;
;; License: GPLv3

;;; Commentary:

;;; Code:

(defvar oauth-access-token nil)

;; From
;; https://github.com/pashky/restclient.el/issues/149#issuecomment-330475676
(defun my-oauth-hook ()
  "Update request variables from a request."
  (save-excursion
    (save-match-data
      (when (re-search-forward "\"access_token\":\"\\(.*?\\)\"" nil t)
        (setq oauth-access-token (match-string 1))))))

(use-package restclient
  :mode
  ("\\.http\\'" . restclient-mode)
  :hook
  (restclient-response-received . my-oauth-hook))

(use-package restclient-test
  :hook
  (restclient-mode . restclient-test-mode))

(use-package ob-restclient
  :after (org restclient)
  :init
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((restclient . t))))

(eval-when-compile
  (require 'restclient))

(declare-function restclient-mode "restclient")

(require' restclient)

(use-package company-restclient
  :after (restclient company)
  :hook
  ((restclient-mode . company-mode))
  :init
  (add-company-backends!!
    :backends company-restclient
    :modes restclient-mode))

(provide 'http-tools)
;;; http-tools.el ends here
