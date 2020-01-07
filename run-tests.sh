#!/bin/sh

set -eu

${EMACS:=emacs} \
    -nw \
    -u $(pwd)/init.el \
    --batch \
    --eval '(let ((debug-on-error t)
                  (url-show-status nil)
                  (user-emacs-directory default-directory)
                  (user-init-file (expand-file-name "init.el"))
                  (load-path (delq default-directory load-path)))
               (load-file user-init-file))'
