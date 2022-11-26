;;; chats.el --- Chats support. -*- lexical-binding: t; -*-

;; Copyright (C) 2019, 2020, 2021, 2022 Serghei Iakovlev <egrep@protonmail.ch>

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

;; Chats support for GNU Emacs.

;;; Code:

(eval-when-compile
  (require 'directories))

(defconst erc-logging-directory
  (concat user-local-dir "logs/erc/"))

(defun erc-logging-hook ()
  "Setting up channel logging for `erc'."
  (eval-when-compile (require 'erc-log nil t))
  (custom-set-variables
   '(erc-log-channels-directory erc-logging-directory)
   '(erc-log-insert-log-on-open t)
   '(erc-save-buffer-on-part nil)
   '(erc-save-queries-on-quit nil)
   '(erc-log-write-after-insert t)
   '(erc-log-write-after-send t))
  (unless (file-exists-p erc-logging-directory)
    (make-directory erc-logging-directory t)))

;; https://www.reddit.com/r/emacs/comments/8ml6na/tip_how_to_make_erc_fun_to_use
(use-package erc
  :ensure nil
  :after auth-source
  :commands erc
  :custom
  (erc-autojoin-channels-alist
   '(("freenode.net"
      "#emacs"
      "#clojure"
      "#phalcon"
      "#zephir")))
  (erc-nick "klay")
  (erc-try-new-nick-p t)
  (erc-nick-uniquifier "_")
  (erc-autojoin-timing 'ident)
  (erc-server-coding-system '(utf-8 . utf-8))
  (erc-fill-function 'erc-fill-static)
  (erc-fill-static-center 22)
  (erc-hide-list '("JOIN" "PART" "QUIT"))
  (erc-track-exclude-types '("JOIN" "MODE" "NICK" "PART" "QUIT"
                             "324" "329" "332" "333" "353" "477"))
  (erc-lurker-hide-list '("JOIN" "PART" "QUIT"))
  (erc-lurker-threshold-time 43200)
  ;; For more see
  ;; URL `https://www.gnu.org/software/emacs/manual/html_node/erc/Connecting.html'
  (erc-prompt-for-nickserv-password nil)
  (erc-prompt-for-password nil)
  (erc-server-reconnect-attempts 6)
  (erc-server-reconnect-timeout 3)
  :config
  (dolist (module '(notifications spelling log))
    (add-to-list 'erc-modules module))
  :hook
  (erc-mode . erc-logging-hook))

;; erc-hl-nicks: Nickname Highlighting for ERC
;; See URL `https://github.com/leathekd/erc-hl-nicks'
(use-package erc-hl-nicks
  :after erc)

(declare-function erc-track-switch-buffer (arg))
(declare-function erc-update-modules ())

(defun my/erc-start-or-switch ()
  "Connects to ERC, or switch to last active buffer."
  (interactive)
  (if (get-buffer "irc.freenode.net:6667")
      (erc-track-switch-buffer 1)
    (when (y-or-n-p "Start ERC? ")
      (progn
	(erc-services-mode 1)
	(erc-update-modules)
	(erc :server "irc.freenode.net" :port 6667)))))

(defun my/search-erc-logs (term)
  "Search the irc logs for a given TERM."
  (interactive "sTerm to search for in the logs: ")
  (counsel-rg term erc-logging-directory))

(provide 'chats)
;;; chats.el ends here
