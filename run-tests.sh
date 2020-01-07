#!/bin/sh

set -eu

PROJECT_ROOT=$(cd "$(dirname $0)/../"; pwd)
ln -s $PROJECT_ROOT $HOME/.emacs.d

${EMACS:=emacs} \
    -nw \
    --batch \
    --eval '(let ((debug-on-error t)
    	          (url-show-status nil)
		  (user-emacs-directory default-directory)
		  (user-init-file (expand-file-name "init.el"))
		  (load-path (delq default-directory load-path)))
	       (load-file user-init-file))'
