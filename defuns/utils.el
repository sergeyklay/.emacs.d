;;; utils.el --- Various utulity functions. -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2020 Serghei Iakovlev <egrep@protonmail.ch>

;; Author: Serghei Iakovlev <egrep@protonmail.ch>
;; URL: https://github.com/sergeyklay/.emacs.d
;;
;; This file is NOT part of GNU Emacs.
;;
;; License: GPLv3

;;; Commentary:

;; Various utulity functions.

;;; Code:

;; See URL `https://github.com/Andersbakken/rtags/issues/987'.
(defun fontify-string (str mode)
  "Return STR fontified according to MODE."
  (with-temp-buffer
    (insert str)
    (delay-mode-hooks (funcall mode))
    (font-lock-default-function mode)
    (font-lock-default-fontify-region
     (point-min) (point-max) nil)
    (buffer-string)))

;; Some packages are too noisy.
;; See URL `http://superuser.com/a/1025827'.
(defun suppress-messages (func &rest args)
  "Suppress message output when call FUNC with remaining ARGS."
  (cl-flet ((silence (&rest args1) (ignore)))
    (advice-add 'message :around #'silence)
    (unwind-protect
        (apply func args)
      (advice-remove 'message #'silence))))

(provide 'utils)
;;; utils.el ends here
