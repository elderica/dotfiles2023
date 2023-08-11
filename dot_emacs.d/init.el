(setq custom-file (concat user-emacs-directory "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

(require 'package)
(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("org" . "https://orgmode.org/elpa/")
        ("gnu" . "https://elpa.gnu.org/packages/")))
(package-initialize)
(package-refresh-contents)

(load-theme 'misterioso)

(delete-selection-mode t)

(show-paren-mode t)

(global-auto-revert-mode t)

(setq inferior-lisp-program "sbcl")

(add-hook 'c-mode-hook
	  (lambda ()
	    (c-set-style "linux")))
(add-hook 'c++-mode-hook
	  (lambda ()
	    (c-set-style "linux")))

(when (package-installed-p 'elcord)
  (require 'elcord)
  (elcord-mode t))
