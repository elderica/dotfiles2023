(setq custom-file (concat user-emacs-directory "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

(require 'package)
(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("org" . "https://orgmode.org/elpa/")
        ("gnu" . "https://elpa.gnu.org/packages/")))
(package-initialize)

(defvar my/favorite-packages
  '(
    elcord

    magit
    git-gutter

    company

    sly
    sly-quicklisp
    sly-named-readtables
    sly-macrostep
    sly-asdf

    dockerfile-mode

    flycheck
    ))

(defun install-my/favorite-packages ()
  (interactive)

  (package-refresh-contents)
  (dolist (package my/favorite-packages)
    (unless (package-installed-p package)
      (package-install package))))

(load-theme 'misterioso)

(delete-selection-mode t)

(show-paren-mode t)

(which-function-mode t)

(transient-mark-mode t)

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

(global-set-key (kbd "M-[") 'switch-to-prev-buffer)
(global-set-key (kbd "M-]") 'switch-to-next-buffer)
