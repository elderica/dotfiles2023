#!/bin/bash
set -e

EMACS_D="${HOME}/.emacs.d"
REPO_URL="https://github.com/jamescherti/minimal-emacs.d.git"

if [ ! -f "${EMACS_D}/init.el" ]; then
    tmpdir=$(mktemp -d)
    for f in post-init.el pre-init.el pre-early-init.el post-early-init.el; do
        [ -f "${EMACS_D}/${f}" ] && mv "${EMACS_D}/${f}" "${tmpdir}/"
    done

    git clone --depth 1 "${REPO_URL}" "${EMACS_D}"

    mv "${tmpdir}/"* "${EMACS_D}/" 2>/dev/null || true
fi
