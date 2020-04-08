;;; devtools.el --- Various devtools. -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2020 Serghei Iakovlev <egrep@protonmail.ch>

;; Author: Serghei Iakovlev <egrep@protonmail.ch>
;; URL: https://github.com/sergeyklay/.emacs.d
;;
;; This file is NOT part of GNU Emacs.
;;
;; License: GPLv3

;;; Commentary:

;; Various devtools to profile and debug Emacs.

;;; Code:

;;;; Emacs Startup Profiler

(use-package esup
  :commands (esup))

(use-package relint
  :commands (relint-file relint-directory relint-current-buffer relint-batch))

(use-package highlight-refontification
  :defer t)

;; Measure the current start up time.
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs ready in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

(provide 'devtools)
;;; devtools.el ends here
