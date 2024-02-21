;;; init.el --- init.el -*- lexical-binding: t; -*-
;;; Commentary:
;;; My Emacs initialization file.

;;; Code:
;; this enables this running method
;; emacs -q -l ~/.debug.emacs.d/init.el
(eval-and-compile
  (when (or load-file-name byte-compile-current-file)
    (setq user-emacs-directory
          (expand-file-name
           (file-name-directory (or load-file-name byte-compile-current-file))))))

(eval-and-compile
  (require 'profiler)
  (defconst c/env-profile "PROFILE_INIT_EL"))

(when (getenv c/env-profile)
  (profiler-start 'cpu))

(eval-and-compile
  (customize-set-variable
   'package-archives '(("org" . "https://orgmode.org/elpa/")
                       ("melpa" . "https://melpa.org/packages/")
                       ("gnu" . "https://elpa.gnu.org/packages/")))
  (package-initialize)
  (unless (package-installed-p 'leaf)
    (package-refresh-contents)
    (package-install 'leaf))

  (leaf leaf-keywords
    :ensure t
    :config
    (leaf-keywords-init)))

(leaf cus-edit
  :doc "tools for customizing Emacs and Lisp packages"
  :tag "builtin" "faces" "help"
  :custom `((custom-file . ,(locate-user-emacs-file "custom.el")))
  :config (when (file-exists-p custom-file)
            (load custom-file)))

(leaf startup
  :custom (inhibit-startup-screen . t))

(leaf cus-start
  :doc "define customization properties of builtins"
  :tag "builtin" "internal"
  :custom
  ((use-dialog-box . t)
   (use-file-dialog . t)
   (menu-bar-mode . t)
   (tool-bar-mode . nil)))

(leaf eval
  :tag "builtin" "internal"
  :custom
  (;; launch debbuger if something went wrong
   (debug-on-error . t)))

(leaf frame
  :tag "builtin" "internal"
  :custom
  ((frame-resize-pixelwise . t)))

(leaf minibuf
  :tag "builtin" "internal"
  :custom
  ((history-delete-duplicates . t)))

(leaf buffer
  :tag "builtin" "internal"
  :custom
  (;; enable wordwrap
   (truncate-lines . nil)))

(leaf xdisp
  :tag "builtin" "internal"
  :custom
  (;; gentle scrolling
   (scroll-conservatively . 10)))

(leaf window
  :tag "builtin" "internal"
  :custom
  (;; gentle scrolling
   (next-screen-context-lines . 1)
   ;; do not move my cursor when scrolling
   (scroll-preserve-screen-position . t)))

(leaf doc
  :tag "builtin" "internal"
  :custom
  (;; use same quote charactor
   (text-quoting-style . 'straight)))

(leaf autorevert
  :doc "revert buffers when files on disk change"
  :tag "builtin"
  :custom ((auto-revert-interval . 1))
  :global-minor-mode global-auto-revert-mode)

(leaf delsel
  :doc "delete selection if you insert"
  :tag "builtin"
  :global-minor-mode delete-selection-mode)

(leaf paren
  :doc "highlight matching paren"
  :tag "builtin"
  :global-minor-mode show-paren-mode)

(leaf which-func
  :doc "print current function in mode line"
  :tag "builtin"
  :global-minor-mode which-function-mode)

(leaf simple
  :doc "basic editing commands for Emacs"
  :tag "builtin" "internal"
  :preface
  (defun c/delete-trailing-whitespaces ()
    (interactive)
    (when (derived-mode-p 'prog-mode)
      (delete-trailing-whitespace)))
  :global-minor-mode transient-mark-mode
  :hook
  (before-save-hook . c/delete-trailing-whitespaces)
  :custom
  (;; indent with spaces
   (indent-tabs-mode . nil)))

(leaf my-gui
  :custom
  (initial-frame-alist . '((width . 120) (height . 35)
                           (left . 0) (top . 0))))
(leaf my-theme
  :config
  (load-theme 'misterioso))

(leaf switch-buffers
  :preface
  (defun c/switch-to-other-buffer () (interactive)
         (switch-to-buffer (other-buffer)))
  :bind
  ("M-[" . switch-to-prev-buffer)
  ("M-]" . switch-to-next-buffer)
  ("C-^" . c/switch-to-other-buffer))

(leaf scroll-buffer-without-cursor-movement
  :preface
  (defun c/scroll-up () (interactive) (scroll-up 1))
  (defun c/scroll-down () (interactive) (scroll-down 1))
  :bind
  ("M-n" . c/scroll-up)
  ("M-p" . c/scroll-down))

(leaf do-not-suspend-emacs
  :config
  (global-unset-key (kbd "C-z")))

(leaf company
  :doc "Modular text completion framework"
  :ensure t
  :hook (after-init-hook . global-company-mode))

(leaf flycheck
  :doc "On-the-fly syntax checking"
  :ensure t
  :hook (after-init-hook . global-flycheck-mode))

(leaf magit
  :doc "A Git porcelain inside Emacs"
  :ensure t)

(leaf git-gutter
  :doc "Manage Git hunks straight from the buffer"
  :ensure t
  :hook (after-init-hook . global-git-gutter-mode))

(leaf dockerfile-mode
  :doc "Major mode for editing Docker's Dockerfiles"
  :ensure t)

(leaf cc-mode
  :doc "major mode for editing C and similar languages"
  :tag "builtin"
  :preface
  (defun c/set-cc-style ()
    (interactive)
    (c-set-style "bsd"))
  :hook
  (c-mode-hook . c/set-cc-style)
  (c++-mode-hook . c/set-cc-style))

(leaf rust-mode
  :doc "Emacs configuration for Rust"
  :ensure t
  :hook (rust-mode-hook . eglot-ensure)
  :custom (rust-format-on-save . t))

(leaf sly
  :doc "Sylvester the Cat's Common Lisp IDE"
  :ensure t
  :custom
  `(inferior-lisp-program . ,(string-join
                              '("sbcl"
                                "--dynamic-space-size" "8192"
                                "--control-stack-size" "8"
                                "--noinform"
                                "--no-sysinit")
                              " ")))

(when (getenv c/env-profile)
  (profiler-report)
  (profiler-stop))

(provide 'init)
;;; init.el ends here
