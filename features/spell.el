;;; spell.el --- Spell related features. -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Serghei Iakovlev

;; Author: Serghei Iakovlev (concat "sadhooklay" "@" "gmail" ".com")
;; URL: https://github.com/sergeyklay/.emacs.d
;;
;; This file is not part of GNU Emacs.
;;
;; License: GPLv3

;;; Commentary:

;; Spell related features for GNU Emacs.

;;; Code:

(use-package ispell
  :custom
  (ispell-silently-savep t))

(use-package flyspell
  :defer t
  :delight
  :init
  (dolist (hook '(text-mode-hook org-mode-hook))
    (add-hook hook (lambda () (flyspell-mode 1))))

  (dolist (hook '(org-agenda-mode-hook))
    (add-hook hook (lambda () (flyspell-mode -1))))

  :config
  (setq
        ;; my dictionary-alist, using for redefinition russian dictionary
        ispell-dictionary-alist
        '(("english"   ;; English
           "[a-zA-Z]"  ;; casecshars
           "[^a-zA-Z]" ;; not-casechars
           "['-’]"     ;; other-chars
           nil
           ("-d" "en" "--lang" "en_GB" "--encoding=en_GB.utf8" "--size=90")
           nil
           utf-8
           ;;iso-8859-1
           )
          ("russian" ;; Russian
           "[АБВГДЕЁЖЗИЙКЛМНОПРСТУФХЦЧШЩЬЫЪЭЮЯабвгдеёжзийклмнопрстуфхцчшщьыъэюя]"
           "[^АБВГДЕЁЖЗИЙКЛМНОПРСТУФХЦЧШЩЬЫЪЭЮЯабвгдеёжзийклмнопрстуфхцчшщьыъэюя]"
           "[`'-]"
           nil
           ("-C" "-d" "ru")
           nil
           utf-8))
        ispell-russian-dictionary "russian"
        ispell-english-dictionary "english"
        ispell-aspell-dictionary-alist ispell-dictionary-alist
        ispell-dictionary ispell-english-dictionary
        ispell-extra-args '("--sug-mode=normal")
        ispell-list-command "--list"
        flyspell-default-dictionary ispell-russian-dictionary))

(use-package flyspell-correct-ivy
  :after (flyspell ivy)
  :bind (:map flyspell-mode-map
              ("C-;" . flyspell-correct-word-generic))
  :custom (flyspell-correct-interface 'flyspell-correct-ivy))

(provide 'spell)
;;; spell.el ends here
