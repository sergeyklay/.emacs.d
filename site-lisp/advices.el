;;; advices.el --- Advices for Elisp functions. -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Serghei Iakovlev <egrep@protonmail.ch>

;; Author: Serghei Iakovlev <egrep@protonmail.ch>
;; URL: https://github.com/sergeyklay/.emacs.d

;; This file is NOT part of Emacs.

;;;; License

;; This file is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Advices for Elisp functions.

;;; Code:

;; Some packages are too noisy.
;; See URL `http://superuser.com/a/1025827'.
(defun my/suppress-messages (func &rest args)
  "Suppress message output when call FUNC with remaining ARGS."
  (cl-flet ((silence (&rest _) (ignore)))
    (advice-add 'message :around #'silence)
    (unwind-protect (apply func args)
      (advice-remove 'message #'silence))))

;; See URL `https://superuser.com/a/1025827/280737'.
(defun my/who-called-me? (func format &rest args)
  "Allow determine who called FUNC with FORMAT and remaining ARGS.
Usage:
  (advice-add 'message :around #'my/who-called-me?)"
  (let ((trace nil) (n 1) (frame nil))
      (while (setf frame (backtrace-frame n))
        (setf n (1+ n)
              trace (cons (cadr frame) trace)) )
      (apply func (concat "<<%S>>\n" format) (cons trace args))))

(provide 'advices)
;;; advices.el ends here
