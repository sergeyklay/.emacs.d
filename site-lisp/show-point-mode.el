;;; show-point-mode.el --- show point position in status bar


;; Copyright (C) 2007, 2010, 2012 Toby Cubitt

;; Author: Toby Cubitt <toby-predictive@dr-qubit.org>
;; Version: 0.3
;; Keywords: point, mode line
;; URL: http://www.dr-qubit.org/emacs.php

;; This file is NOT part of Emacs.
;;
;; This file is free software: you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation, either version 3 of the License, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
;; more details.
;;
;; You should have received a copy of the GNU General Public License along
;; with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.


;;; Commentary:
;;
;; A simple minor-mode to display the point in the status bar.


;;; Code:

(provide 'show-point-mode)


;; add point display to mode-line construct
(add-to-list 'mode-line-position
	     '(show-point-mode (5 (:eval (format "(%d)" (point)))))
	     'append)


(define-minor-mode show-point-mode
  "Toggle show-point mode.
With no argument, this command toggles the mode.
A non-null prefix argument turns the mode on.
A null prefix argument turns it off.

When enabled, the value of `point' is displayed in the
mode-line (after the line and column numbers, if those are being
displayed too).")


;;; show-point-mode.el ends here
