;;; config.el --- Base user configuration. -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Serghei Iakovlev

;; Author: Serghei Iakovlev (concat "sadhooklay" "@" "gmail" ".com")
;; URL: https://github.com/sergeyklay/.emacs.d
;;
;; This file is not part of GNU Emacs.
;;
;; License: GPLv3

;;; Commentary:

;; This file is used to set up base features

;;; Code:

;;; Base user configuration for GNU Emacs.

(defvar global-found (executable-find "global")
  "Whether or not the global executable present on this system.")

(provide 'config)
;;; config.el ends here
