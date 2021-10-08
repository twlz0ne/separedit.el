#!/usr/bin/env bash

## Copyright (C) 2021 Gong Qijian <gongqijian@gmail.com>
##
## This program is free software; you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 3 of the License, or
## (at your option) any later version.
##
## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with this program.  If not, see <https://www.gnu.org/licenses/>.

## Commentary:
# Preconfigured `emacs -Q' with a basic Separedit configuration.

## Change Log:
#   0.1.0   2021-03-30 16:03:56.00 +0800 Initial Version

function usage {
    cat >&1 <<EOF
Usage: ${0##*/} [SCRIPT-OPTIONS] [EMACS-OPTIONS-OR-FILENAME]

Script options:

    --path, -p EMACS_BIN    Emacs executable path [default emacs]
    --user-dir, -ud DIR     Set ‘user-emacs-directory’, if not provide, choose from:
                                - ~/.emacs.d/{EMACS-VERSION}/
                                - ~/.emacs.d/{MAJOR-VERSION}.{MINOR-VERSION}/
                                - ~/.emacs.d/
    --elpa-dir, -ed DIR     Set ‘package-user-dir’ (default {user-emacs-directory}/elpa)
    --directory, -L DIR     prepend DIR to ‘load-path’ (can be set multiple times)
    --packages, -P PKGS     Load specified packages (separate with ",") 
    --modes, -M MODES       Emable specified modes (separate with ",")
    --help, -h              Print help info and exit

Any other Emacs options (e.g. -nw, --eval...) or filename must come after.  For
more information execute ‘emacs --help’.

Example:

    ${0##*/} \\
        -p emacs-27 \\
        -ud .cask/27.1 \\
        -L /path/to/foo-pkg \\
        -L /path/to/bar-pkg \\
        -P foo,bar \\
        -M foo-mode,bar-mode -nw

EOF
}

opt_user_dir=
opt_elpa_dir=
opt_load_dirs=("\".\"")
opt_load_pkgs=
opt_enable_modes=
opt_emacs_bin=$EMACS

while [[ $# -gt 0 ]]; do
    case "$1" in
        -h |--help) usage; exit 1 ;;
        -p |--path) shift; opt_emacs_bin="$1"; shift;;
        -ud|--user-dir) shift; opt_user_dir="$1"; shift;;
        -ed|--elpa-dir) shift; opt_elpa_dir="$1"; shift;;
        -L |--directory) shift; opt_load_dirs+=("\"$1\""); shift;;
        -P |--packages) shift; opt_load_pkgs="$1"; shift;;
        -M |--modes) shift; opt_enable_modes="$1"; shift;;
        -*) break;;
        *) echo "Unknown option: $1"; exit 1;;
    esac
done

${opt_emacs_bin:-emacs} -Q \
                --debug-init \
                --eval "(progn
                          (require 'seq)
                          (require 'subr-x)
                          (setq user-emacs-directory
                                (if (string-empty-p \"${opt_user_dir}\")
                                    (car (seq-filter
                                          #'file-exists-p
                                          (list (format \"~/.emacs.d/%s/\" emacs-version)
                                                (format \"~/.emacs.d/%s.%s/\" emacs-major-version emacs-minor-version)
                                                \"~/.emacs.d/\")))
                                  \"${opt_user_dir}\"))
                          (setq package-user-dir
                                (if (string-empty-p \"${opt_elpa_dir}\")
                                    (expand-file-name \"elpa/\" user-emacs-directory)
                                  \"${opt_elpa_dir}\"))
                          (dolist (dir '(${opt_load_dirs[@]}))
                            (add-to-list 'load-path (expand-file-name dir)))
                          (package-initialize)
                          (unless (string-empty-p \"${opt_load_pkgs}\")
                            (dolist (pkg (split-string \"${opt_load_pkgs}\" \",\"))
                              (require (intern pkg))))
                          (unless (string-empty-p \"${opt_enable_modes}\")
                            (dolist (mode (split-string \"${opt_enable_modes}\" \",\"))
                              (funcall (intern mode))))
                          ;; (setq separedit-remove-trailing-spaces-in-comment t)
                          ;; (require 'separedit)
                          ;; (global-set-key (kbd \"C-c '\") #'separedit)
                          ;; (with-current-buffer \"*scratch*\"
                          ;;   (insert \";; ‘separedit’ is bound to C-c ' by default.\"))
                          )" "$@"

# emacs-q.sh ends here
