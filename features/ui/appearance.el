;;; appearance.el --- All appearance related setting. -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Serghei Iakovlev <sadhooklay@gmail.com>

;; Author: Serghei Iakovlev <sadhooklay@gmail.com>
;; URL: https://github.com/sergeyklay/.emacs.d
;;
;; This file is not part of GNU Emacs.
;;
;; License: GPLv3

;;; Commentary:

;; This file should contain all appearance related settings

;;; Code:

;; Throw away the mouse when typing.
;; Move the mouse to the corner only if the cursor gets too close,
;; and allow it to return once the cursor is out of the way.
(mouse-avoidance-mode 'exile)

;; Don't beep at me
(setq visible-bell t)

;; Just blink the modeline on errors
(setq ring-bell-function
      (lambda ()
        (invert-face 'mode-line)
        (run-with-timer 0.05 nil 'invert-face 'mode-line)))

;; Pretify page breaks
(use-package page-break-lines
  :diminish page-break-lines-mode
  :config
  (global-page-break-lines-mode))

;; Highlight matching parentheses when the point is on them.
(show-paren-mode 1)

;; For more see URL `http://ergoemacs.org/emacs/emacs_list_and_set_font.html'
(cond
 ((string-equal system-type "gnu/linux")
  (when (member "Fira Code" (font-family-list))
    (add-to-list 'initial-frame-alist '(font . "Fira Code-12"))
    (add-to-list 'default-frame-alist '(font . "Fira Code-12"))))
 ((string-equal system-type "darwin")
    (when (member "Fira Code" (font-family-list))
    (add-to-list 'initial-frame-alist '(font . "Fira Code-14"))
    (add-to-list 'default-frame-alist '(font . "Fira Code-14")))))

(use-package unicode-fonts)

(use-package all-the-icons
  :ensure t
  :config
  (unless (package-installed-p 'all-the-icons)
    (all-the-icons:all-the-icons-install-fonts)))

(add-to-list 'face-font-rescale-alist '(".*icons.*" . 0.9))
(add-to-list 'face-font-rescale-alist '(".*FontAwesome.*" . 0.9))

(use-package doom-themes
  :config
  (load-theme 'doom-one t))

;; nice scrolling
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

;; This makes it so C-n and C-p  won't make the buffer
;; jump around so much.
(use-package smooth-scrolling
  :config
  (smooth-scrolling-mode))

(provide 'appearance)
;;; appearance.el ends here

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
