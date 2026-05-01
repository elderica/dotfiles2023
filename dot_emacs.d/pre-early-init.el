;;; pre-early-init.el --- config for minimal-emacs.d -*- no-byte-compile: t; lexical-binding: t; -*-

(setopt debug-on-error t)

(setq site-run-file nil)
(setq inhibit-default-init t)

(setopt minimal-emacs-ui-features '(context-menu menu-bar dialogs tooltips))

(setopt user-emacs-directory (expand-file-name "var/" minimal-emacs-user-directory))
(setopt package-user-dir (expand-file-name "elpa" user-emacs-directory))
