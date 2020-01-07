#!/bin/sh -xe

echo "Attempting startup..."

( echo "$EMACS" | grep -q "term" ) && EMACS=emacs || EMACS=${EMACS:-emacs}

command -v $EMACS >/dev/null || {
  >&2 echo "Can't find GNU Emacs in your PATH"
  exit 1
}

case "$VERSION" in *\ 2[0-5].[0-9])
  echo "Detected Emacs $VERSION"
  echo "This configuration only supports Emacs 26.0 and newer"
  exit 2
  ;;
esac

[ "$1" = -d ] || [ "$1" = --debug ] && {
    shift
    export DEBUG=1
}

${EMACS} \
    --no-window-system \
    --batch \
    --eval '(let ((debug-on-error t)
                  (url-show-status nil)
                  (user-emacs-directory default-directory)
                  (user-init-file (expand-file-name "init.el"))
                  (load-path (delq default-directory load-path)))
               (load-file user-init-file)
               (run-hooks (quote after-init-hook)))'

echo "Startup successful"
