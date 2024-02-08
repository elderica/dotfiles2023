;;; init --- init.el
;;; Commentary:
;;; My Emacs initialization file.

;;; Code:

;;; Customization
(setq custom-file (concat user-emacs-directory "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))


;;; Default Frame Position
(customize-set-value 'default-frame-alist
                     '((width . 120) (height . 35)
		       (left . 0) (top . 0)))


;;; Package management
;; Initialize `package.el`.
(require 'package)
(customize-set-variable
 'package-archives
 (add-to-list 'package-archives
	      '("melpa" . "https://melpa.org/packages/")))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Ensure my favorite packages are installed.
(defvar my/favorite-packages
  '(
    ; Discord integration
    ;elcord

    ; Git integration
    magit
    git-gutter

    ; Completion
    company

    ; Syntax checker
    flycheck

    ; Common Lisp Development
    sly
    sly-quicklisp
    sly-named-readtables
    sly-macrostep
    sly-asdf

    ; Rust
    rust-mode

    ; Dockerfile
    dockerfile-mode
    ))

(dolist (package my/favorite-packages)
  (unless (package-installed-p package)
    (package-install package)))

;; Don't suspend my Emacs!
(global-unset-key (kbd "C-z"))

;;; Package setup
;; Enable completion `company-mode`.
(add-hook 'after-init-hook 'global-company-mode)

;; Enable syntax checking with Flycheck
(add-hook 'after-init-hook 'global-flycheck-mode)

;; Enable git-gutter
(add-hook 'after-init-hook 'global-git-gutter-mode)

;; Use Steel Bank Common Lisp.
(eval-after-load 'sly
  (customize-set-variable 'inferior-lisp-program
			  "sbcl --dynamic-space-size 8192 --control-stack-size 8 --noinform --no-sysinit"))


;;; General configurations
;; Use dark theme.
(load-theme 'misterioso)

;; Replace the active region just by typing text.
(delete-selection-mode t)

;; Highlight corresponding parenthesis.
(show-paren-mode t)

;; Display current function name in the mode line.
(which-function-mode t)

;; Highlight region.
(transient-mark-mode t)

;; Reload when the file is just updated.
(global-auto-revert-mode t)

;; Don't insert TABs by default.
(setq-default indent-tabs-mode nil)

;; Get rid of trailing whitespace automatically.
(add-hook 'prog-mode-hook
 	  (lambda ()
 	    (add-hook 'before-save-hook
 		      (lambda () (delete-trailing-whitespace))
 		      :local t)))


;;; Language specific configuration
;; C/C++ configurations.
(dolist (hook '(c-mode-hook c++-mode-hook))
  (add-hook hook (lambda ()
		   (flyspell-prog-mode)
		   (c-set-style "linux"))))
;; Rust
(add-hook 'rust-mode-hook 'eglot-ensure)
(with-eval-after-load 'rust-mode
  (customize-set-variable 'rust-format-on-save t))


;;; Key bindings
;; Switch buffers.
(global-set-key (kbd "M-[") 'switch-to-prev-buffer)
(global-set-key (kbd "M-]") 'switch-to-next-buffer)
(global-set-key (kbd "C-^")
		(lambda () (interactive)
		  (switch-to-buffer (other-buffer))))

;; Scroll buffer without cursor movement.
(global-set-key (kbd "M-n") (lambda () (interactive) (scroll-up 1)))
(global-set-key (kbd "M-p") (lambda () (interactive) (scroll-down 1)))

(provide 'init)
;;; init.el ends here.
