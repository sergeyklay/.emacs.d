;;; docker-io.el --- Docker related configuration. -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Serghei Iakovlev <sadhooklay@gmail.com>

;; Author: Serghei Iakovlev <sadhooklay@gmail.com>
;; URL: https://github.com/sergeyklay/.emacs.d
;;
;; This file is not part of GNU Emacs.
;;
;; License: GPLv3

;;; Commentary:

;; Docker related configuration

;;; Code:

(use-package docker
  :defer t
  :init
  (progn
    (use-package docker-tramp
      :defer t)

    (use-package dockerfile-mode
      :mode "Dockerfile\\'")

    (use-package docker-compose-mode
      :mode "docker-compose[^/]*\\.yml\\'")))

(provide 'docker-io)
;;; docker-io.el ends here
