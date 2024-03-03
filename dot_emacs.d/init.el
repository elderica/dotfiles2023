;;; init.el --- init.el -*- lexical-binding: t; -*-
;;; Commentary:
;;; My Emacs initialization file.
;;; Code:
;; this enables this running method
;; emacs -q -l ~/.debug.emacs.d/init.el
(eval-and-compile
  (when (or load-file-name byte-compile-current-file)
    (setq user-emacs-directory
          (expand-file-name (file-name-directory (or load-file-name byte-compile-current-file))))))
(eval-and-compile
  (customize-set-variable
   'package-archives '(("org" . "https://orgmode.org/elpa/")
                       ("melpa" . "https://melpa.org/packages/")
                       ("gnu" . "https://elpa.gnu.org/packages/")
                       ("nongnu" . "https://elpa.nongnu.org/nongnu/")))
  (package-initialize)
  (unless (package-installed-p 'leaf)
    (package-refresh-contents)
    (package-install 'leaf))
  (leaf leaf-keywords :ensure t :config (leaf-keywords-init)))

(leaf cus-edit
  :doc "tools for customizing Emacs and Lisp packages"
  :tag "builtin" "faces" "help"
  :custom `((custom-file . ,(locate-user-emacs-file "custom.el")))
  :config (when (file-exists-p custom-file) (load custom-file)))

(leaf startup :custom (inhibit-startup-screen . t))

(leaf cus-start
  :doc "define customization properties of builtins"
  :tag "builtin" "internal"
  :custom ((use-dialog-box . t)
           (use-file-dialog . t)
           (menu-bar-mode . t)
           (tool-bar-mode . nil)))

(leaf eval
  :tag "builtin" "internal"
  :custom (debug-on-error . t))

(leaf xdisp
  :tag "builtin" "internal" "scrolling"
  :custom (scroll-conservatively . 10))

(leaf window
  :tag "builtin" "internal" "scrolling"
  :custom ((next-screen-context-lines . 1)
           ;; do not move my cursor when scrolling
           (scroll-preserve-screen-position . t)))

(leaf doc
  :tag "builtin" "internal"
  :custom (text-quoting-style . 'straight))

(leaf autorevert
  :doc "revert buffers when files on disk change"
  :tag "builtin"
  :global-minor-mode global-auto-revert-mode)

(leaf delsel
  :doc "delete selection if you insert"
  :tag "builtin"
  :global-minor-mode delete-selection-mode)

(leaf paren
  :doc "highlight matching paren"
  :tag "builtin"
  :global-minor-mode show-paren-mode
  :custom ((show-paren-style . 'mixed)
           (show-paren-when-point-inside-paren . t)
           (show-paren-when-point-in-periphery . t)))

(leaf which-func
  :doc "print current function in mode line"
  :tag "builtin"
  :global-minor-mode which-function-mode)

(leaf simple
  :doc "basic editing commands for Emacs"
  :tag "builtin" "internal"
  :custom (indent-tabs-mode . nil)
  :global-minor-mode transient-mark-mode
  :hook (before-save-hook . (lambda () (interactive)
                              (when (derived-mode-p 'prog-mode)
                                (delete-trailing-whitespace)))))

(leaf my-gui :custom (initial-frame-alist . '((width . 120) (height . 35)
                                              (left . 0) (top . 0))))

(leaf my-theme :config (load-theme 'misterioso))

(leaf switch-buffers :bind
  ("M-[" . switch-to-prev-buffer)
  ("M-]" . switch-to-next-buffer)
  ("C-^" . (lambda () (interactive) (switch-to-buffer (other-buffer)))))

(leaf do-not-suspend-emacs :config (global-unset-key (kbd "C-z")))

(leaf misc :bind ("C-c d" . duplicate-dwim))

(leaf windmove :config (windmove-default-keybindings 'meta))

(leaf winner :global-minor-mode winner-mode)

(leaf macrostep :ensure t :bind (("C-c e" . macrostep-expand)))

(leaf pulsar :ensure t
  :global-minor-mode pulsar-global-mode
  :hook ((next-error-hook . pulsar-pulse-line)
         (minibuffer-setup-hook . pulsar-pulse-line)
         (imenu-after-jump-hook . pulsar-recenter-top)))

(leaf icomplete
  :doc "minibuffer completion incremental feedback"
  :global-minor-mode fido-vertical-mode)

(leaf history
  :custom (history-delete-duplicates . t)
  :custom ((savehist-additional-variables . '(corfu-history)))
  :config (savehist-mode 1))

(leaf which-key :ensure t
  :doc "a minor mode for Emacs that displays the key bindings"
  :global-minor-mode which-key-mode)

(leaf magit :ensure t :doc "A Git porcelain inside Emacs"
  :config (leaf magit-delta :ensure t
            :after magit :hook (magit-mode . magit-delta-mode)))

(leaf git-gutter :ensure t
  :doc "Manage Git hunks straight from the buffer"
  :global-minor-mode global-git-gutter-mode)

(leaf flycheck :ensure t
  :doc "On-the-fly syntax checking"
  :global-minor-mode global-flycheck-mode
  :config
  (leaf flycheck-eglot :ensure t
    :after eglot flycheck :global-minor-mode global-flycheck-eglot-mode)
  (leaf flycheck-inline :ensure t
    :after flycheck :hook (flycheck-mode-hook . flycheck-inline-mode))
  (leaf flycheck-rust :ensure t
    :doc "Flycheck for Rust"
    :after rust-mode :hook (flycheck-mode-hook . flycheck-rust-setup)))

(leaf corfu :ensure t
  :doc "COmpletion in Region FUnction"
  :global-minor-mode global-corfu-mode
  :global-minor-mode corfu-history
  :global-minor-mode corfu-indexed-mode
  :global-minor-mode corfu-echo-mode
  :hook ((minibuffer-setup-hook . (lambda ()
                                    (unless (eq (current-local-map) read-passwd-map)
                                      (setq-local corfu-auto nil
                                                  corfu-echo-delay nil
                                                  corfu-popupinfo-delay nil)
                                      (corfu-mode 1))))
         (eshell-mode-hook  . (lambda ()
                                (setq-local corfu-auto nil)
                                (corfu-mode 1))))
  :custom ((corfu-auto . t)
           (corfu-cycle . t))
  :config (leaf corfu-terminal :ensure t
            :after corfu :unless (display-graphic-p) :global-minor-mode corfu-terminal-mode))

(leaf cape :ensure t
  :doc "Let your completions fly!"
  :config (progn (add-to-list 'completion-at-point-functions #'cape-dabbrev)
                 (add-to-list 'completion-at-point-functions #'cape-keyword)
                 (add-to-list 'completion-at-point-functions #'cape-emoji)
                 (add-to-list 'completion-at-point-functions #'cape-file)))

(leaf marginalia :ensure t
  :doc "Marginalia in the minibuffer"
  :global-minor-mode marginalia-mode)

(leaf orderless :ensure t
  :custom ((completion-styles . '(orderless basic))
           (orderless-matching-styles . '(orderless-literal
                                          orderless-flex
                                          orderless-regexp))))

(leaf dockerfile-mode :ensure t :doc "Major mode for editing Docker's Dockerfiles")

(leaf markdown-mode :ensure t :mode ("README\\.md\\'" . gfm-mode))

(eval-and-compile
  ;; https://github.com/freebsd/freebsd-src/blob/main/tools/tools/editing/freebsd.el
  (defun bsd-knf ()
    (interactive)
    (c-set-style "bsd")
    (indent-tabs-mode 1)
    (c-set-offset 'defun-block-intro 8)
    (c-set-offset 'statement-block-intro 8)
    (c-set-offset 'statement-case-intro 8)
    (c-set-offset 'substatement-open 4)
    (c-set-offset 'substatement 8)
    (c-set-offset 'arglist-cont-nonempty 4)
    (c-set-offset 'inclass 8)
    (c-set-offset 'knr-argdecl-intro 8)))

(leaf cc-mode
  :doc "major mode for editing C and similar languages"
  :tag "builtin"
  :hook (c-mode-common-hook . (lambda () (bsd-knf))))

(leaf rust-mode :ensure t
  :doc "Emacs configuration for Rust"
  :hook (rust-mode-hook . eglot-ensure)
  :custom (rust-format-on-save . t))

(eval-and-compile
  (defvar init/sbcl-command
    '("sbcl"
      "--dynamic-space-size" "8192"
      "--control-stack-size" "8"
      "--noinform"
      "--no-sysinit")))

(leaf sly :ensure t
  :doc "Sylvester the Cat's Common Lisp IDE"
  :bind ("C-c l" . sly-eval-print-last-expression)
  :custom `((inferior-lisp-program . ,(string-join init/sbcl-command " "))
            (sly-lisp-implementations . `((sbcl ,init/sbcl-command)
                                          (ecl ("ecl"))))))

(provide 'init)
;;; init.el ends here
