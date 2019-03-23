;;; tags.el --- Initialise all things tags related. -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Serghei Iakovlev

;; Author: Serghei Iakovlev (concat "sadhooklay" "@" "gmail" ".com")
;; URL: https://github.com/sergeyklay/.emacs.d

;;; Commentary:

;; Setting up tags realated features

;;; Code:

;; A front-end for accessing the gtags-generated tags.
;; For more see URL `https://github.com/leoliu/ggtags'
(when (executable-find "global")
  (use-package ggtags
    :ensure t
    :bind
    (:map ggtags-mode-map
          ("C-c g s" . 'ggtags-find-other-symbol)
          ("C-c g h" . 'ggtags-view-tag-history)
          ("C-c g r" . 'ggtags-find-reference)
          ("C-c g f" . 'ggtags-find-file)
          ("C-c g c" . 'ggtags-create-tags)
          ("C-c g u" . 'ggtags-update-tags)
          ("M-."     . 'ggtags-find-tag-dwim)
          ("M-,"     . 'pop-tag-mark)
          ("C-c <"   . 'ggtags-prev-mark)
          ("C-c >"   . 'ggtags-next-mark))
    :config
    (progn
      (dolist (hook '(sh-mode-hook
                      c-mode-hook
                      makefile-mode-hook))
        (add-hook hook #'ggtags-mode)))))

(use-package ctags-update
  :ensure t
  :if (executable-find "ctags-exuberant")
  :diminish (ctags-auto-update-mode . " τ")
  :hook
  ((c-mode-common . turn-on-ctags-auto-update-mode)))

;; Make Emacs reload the TAGS file automatically
(setq tags-revert-without-query 1)

(provide 'tags)
;;; tags.el ends here
