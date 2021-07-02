#!/usr/bin/env bash
# 
# Run tests on multiple versions of emacs
# 
# Usage:
# 
#     cp ./run-test.{example,}.sh # And edit ‘emacs_bins’
#     ./run-test.sh <ert-testcase-name>
# 
set -e

# Proxy for quelpa
export PROXY=127.0.0.1:7890

emacs_bins=(
    ~/.local/bin/emacs
    # /Applications/Emacs-26.3.app/Contents/MacOS/Emacs
    # /Applications/Emacs-26.1.app/Contents/MacOS/Emacs
    # /Applications/Emacs-25.3.app/Contents/MacOS/Emacs
    # /Applications/Emacs-25.2.app/Contents/MacOS/Emacs
    /Applications/Emacs-25.1.app/Contents/MacOS/Emacs
)

for bin in "${emacs_bins[@]}"; do
    ver=$($bin --batch --eval '(princ (format "%s.%s" emacs-major-version emacs-minor-version))')
    echo "==> $ver: $bin"
    export EMACS=$bin
    $bin --batch -l test/test-bootstrap.el -l test/test-checkinstall.el
    make clean ; make test $1
done

make clean
