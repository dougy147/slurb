#!/bin/sh
set -xe

input=${1:-slurb.lisp}
output=${2:-slurb}

sbcl --load "${input}" \
     --eval "(require \"asdf\")" \
     --eval "(setq uiop:*image-entry-point* #'main)" \
     --eval "(uiop:dump-image \"${output}\" :executable t)"
