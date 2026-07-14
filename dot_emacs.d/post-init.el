;;; post-init.el --- My main config -*- no-byte-compile: t; lexical-binding: t; -*-

(setopt custom-file (locate-user-emacs-file "custom.el"))
(setopt package-install-upgrade-built-in t)

;;
;; Beginning of lines take from `https://github.com/jamescherti/minimal-emacs.d'
;;
;; Native compilation enhances Emacs performance by converting Elisp code into
;; native machine code, resulting in faster execution and improved
;; responsiveness.
(use-package compile-angel
  :demand t
  :config
  ;; The following disables compilation of packages during installation;
  ;; compile-angel will handle it.
  (setq package-native-compile nil)

  ;; Set `compile-angel-verbose' to nil to disable compile-angel messages.
  ;; (When set to nil, compile-angel won't show which file is being compiled.)
  (setq compile-angel-verbose nil)

  ;; The following directive prevents compile-angel from compiling your init
  ;; files. If you choose to remove this push to `compile-angel-excluded-files'
  ;; and compile your pre/post-init files, ensure you understand the
  ;; implications and thoroughly test your code. For example, if you're using
  ;; the `use-package' macro, you'll need to explicitly add:
  ;; (eval-when-compile (require 'use-package))
  ;; at the top of your init file.
  (push "/init.el" compile-angel-excluded-files)
  (push "/early-init.el" compile-angel-excluded-files)
  (push "/pre-init.el" compile-angel-excluded-files)
  (push "/post-init.el" compile-angel-excluded-files)
  (push "/pre-early-init.el" compile-angel-excluded-files)
  (push "/post-early-init.el" compile-angel-excluded-files)

  ;; A local mode that compiles .el files whenever the user saves them.
  ;; (add-hook 'emacs-lisp-mode-hook #'compile-angel-on-save-local-mode)

  ;; A global mode that compiles .el files prior to loading them via `load' or
  ;; `require'. Additionally, it compiles all packages that were loaded before
  ;; the mode `compile-angel-on-load-mode' was activated.
  (compile-angel-on-load-mode 1))
;;
;; End of lines taken from `https://github.com/jamescherti/minimal-emacs.d'
;;

(use-package windmove
  :ensure nil
  :config (windmove-default-keybindings)
          (windmove-swap-states-default-keybindings)
          (windmove-display-default-keybindings)
          (windmove-delete-default-keybindings))

;; Don't touch "M-[" (Control Sequence Introducer)!
(global-unset-key (kbd "C-z"))
(bind-keys ("C-^" . (lambda () (interactive) (switch-to-buffer (other-buffer))))
           ("M-m" . (lambda () (interactive) (forward-whitespace 1)))
           ("M-n" . (lambda () (interactive) (forward-whitespace -1)))
           ("C-c d" . duplicate-dwim))
;; https://www.masteringemacs.org/article/text-expansion-hippie-expand
(global-set-key [remap dabbrev-expand] 'hippie-expand)

;; OSC 52 clipboard integration
(setopt xterm-extra-capabilities '(getSelection setSelection modifyOtherKeys))

(delete-selection-mode 1)

(setopt use-default-font-for-symbols nil)
(when (window-system)
  (set-face-attribute 'default nil :height 130 :weight 'normal :family "PlemolJP Text")
  (set-fontset-font t 'emoji (font-spec :family "Noto Color Emoji"))
  (set-fontset-font t 'symbol (font-spec :family "Noto Color Emoji") nil 'append))

(setopt scroll-preserve-screen-position t)
(setopt text-quoting-style 'straight)

(use-package which-func
  :ensure nil
  :config
  (which-function-mode 1)
  :custom-face
  (which-func ((t (:foreground "lawn green")))))

(let ((inhibit-redisplay t))
  (mapc #'disable-theme custom-enabled-themes)
  (load-theme 'wombat t))

(use-package cc-mode
  :ensure nil
  :config
  ;; https://www.emacswiki.org/emacs/IndentingC
  (c-add-style "emacswiki-openbsd"
               '("bsd"
                 (c-backspace-function . delete-backward-char)
                 (c-syntactic-indentation-in-macros . nil)
                 (c-tab-always-indent . nil)
                 (c-hanging-braces-alist
                  (block-close . c-snug-do-while))
                 (c-offsets-alist
                  (arglist-cont-nonempty . *)
                  (statement-cont . *))
                 (indent-tabs-mode . t)))
  ;; https://github.com/hogand/openbsd-knf-emacs
  (when (file-directory-p "~/.elisp/openbsd-knf-emacs")
    (add-to-list 'load-path "~/.elisp/openbsd-knf-emacs")
    (require 'openbsd-knf-style)
    (c-add-style "openbsd-knf-emacs" openbsd-knf-style)
    (setf (alist-get 'other c-default-style) "openbsd-knf-emacs")))

(transient-mark-mode 1)

(setopt show-trailing-whitespace nil)

(let ((sbcl-command
       '("sbcl"
         "--dynamic-space-size" "8192"
         "--control-stack-size" "8"
         "--noinform"
         "--no-sysinit"))
      (qlot-ros-command
       '("qlot" "exec" "ros" "run")))
  (use-package sly
    :ensure t
    :commands (sly sly-mode)
    ;:hook (lisp-mode . sly-mode)
    :hook (lisp-mode . (lambda () (setq indent-tabs-mode nil)))
    :bind ("C-c l" . sly-eval-print-last-expression)
    :config
    (setopt inferior-lisp-program (string-join sbcl-command " ")
            sly-lisp-implementations `((qlot ,qlot-ros-command)
                                       (sbcl ,sbcl-command)
                                       (ccl ("ccl"))
                                       (ecl ("ecl")))
            sly-common-lisp-style-default "modern")))

(use-package mwim
  :ensure t
  :bind (("C-a" . mwim-beginning-of-code-or-line)
         ("C-e" . mwim-end-of-code-or-line)))

(use-package pulsar
  :ensure t
  :commands (pulsar-pulse-line pulsar-recenter-top)
  :hook ((next-error-hook . pulsar-pulse-line)
         (minibuffer-setup-hook . pulsar-pulse-line)
         (imenu-after-jump-hook . pulsar-recenter-top))
  :config (pulsar-global-mode))


(use-package rust-mode
  :ensure t
  :commands rust-mode
  :mode ("\\.rs\\'")
  :hook ((rust-mode . eglot-ensure))
  :custom ((rust-format-on-save t)
           (rust-indent-offset 2)))

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package graphviz-dot-mode
  :ensure t
  :mode ("\\.dot\\'"))

(use-package eglot
  :ensure nil
  :commands (eglot-ensure
             eglot-rename
             eglot-format-buffer)
  ;; Do not mess up indentation
  :custom (eglot-ignored-server-capabilities
           '(:documentFormattingProvider
             :documentRangeFormattingProvider
             :documentOnTypeFormattingProvider)))

(use-package xterm-color
  :ensure t)

(use-package vterm
  :if (memq system-type '(gnu/linux cygwin))
  :ensure t
  :bind (:map vterm-mode-map
              ("C-q" . vterm-send-next-key)))

(use-package eat
  :ensure t)

;;
;; Beginning of lines take from `https://github.com/jamescherti/minimal-emacs.d'
;;
;; Auto-revert in Emacs is a feature that automatically updates the
;; contents of a buffer to reflect changes made to the underlying file
;; on disk.
(use-package autorevert
  :ensure nil
  :commands (auto-revert-mode global-auto-revert-mode)
  :hook
  (after-init . global-auto-revert-mode)
  :init
  ;; (setq auto-revert-verbose t)
  (setq auto-revert-interval 3)
  (setq auto-revert-remote-files nil)
  (setq auto-revert-use-notify t)
  (setq auto-revert-avoid-polling nil))

;; Recentf is an Emacs package that maintains a list of recently
;; accessed files, making it easier to reopen files you have worked on
;; recently.
(use-package recentf
  :ensure nil
  :commands (recentf-mode recentf-cleanup)
  :hook
  (after-init . recentf-mode)

  :init
  (setq recentf-auto-cleanup (if (daemonp) 300 'never))
  (setq recentf-exclude
        (list "\\.tar$" "\\.tbz2$" "\\.tbz$" "\\.tgz$" "\\.bz2$"
              "\\.bz$" "\\.gz$" "\\.gzip$" "\\.xz$" "\\.zip$"
              "\\.7z$" "\\.rar$"
              "COMMIT_EDITMSG\\'"
              "\\.\\(?:gz\\|gif\\|svg\\|png\\|jpe?g\\|bmp\\|xpm\\)$"
              "-autoloads\\.el$" "autoload\\.el$"))

  :config
  ;; A cleanup depth of -90 ensures that `recentf-cleanup' runs before
  ;; `recentf-save-list', allowing stale entries to be removed before the list
  ;; is saved by `recentf-save-list', which is automatically added to
  ;; `kill-emacs-hook' by `recentf-mode'.
  (add-hook 'kill-emacs-hook #'recentf-cleanup -90))

;; savehist is an Emacs feature that preserves the minibuffer history between
;; sessions. It saves the history of inputs in the minibuffer, such as commands,
;; search strings, and other prompts, to a file. This allows users to retain
;; their minibuffer history across Emacs restarts.
(use-package savehist
  :ensure nil
  :commands (savehist-mode savehist-save)
  :hook
  (after-init . savehist-mode)
  :init
  (setq history-length 300)
  (setq savehist-autosave-interval 600))

;; save-place-mode enables Emacs to remember the last location within a file
;; upon reopening. This feature is particularly beneficial for resuming work at
;; the precise point where you previously left off.
(use-package saveplace
  :ensure nil
  :commands (save-place-mode save-place-local-mode)
  :hook
  (after-init . save-place-mode)
  :init
  (setq save-place-limit 400))

(use-package diff-hl
  :ensure t
  :commands (diff-hl-mode
             global-diff-hl-mode)
  :hook (prog-mode . diff-hl-mode)
  :init
  (setq diff-hl-flydiff-delay 0.4)  ; Faster
  (setq diff-hl-show-staged-changes nil)  ; Realtime feedback
  (setq diff-hl-update-async t)  ; Do not block Emacs
  (setq diff-hl-global-modes '(not pdf-view-mode image-mode)))

;; Corfu enhances in-buffer completion by displaying a compact popup with
;; current candidates, positioned either below or above the point. Candidates
;; can be selected by navigating up or down.
(use-package corfu
  :commands (corfu-mode global-corfu-mode)

  :hook ((prog-mode . corfu-mode)
         (shell-mode . corfu-mode)
         (eshell-mode . corfu-mode))

  :custom
  ;; Hide commands in M-x which do not apply to the current mode.
  (read-extended-command-predicate #'command-completion-default-include-p)
  ;; Disable Ispell completion function. As an alternative try `cape-dict'.
  (text-mode-ispell-word-completion nil)
  (tab-always-indent 'complete)

  ;; Enable Corfu
  :config
  (global-corfu-mode))

;; Cape, or Completion At Point Extensions, extends the capabilities of
;; in-buffer completion. It integrates with Corfu or the default completion UI,
;; by providing additional backends through completion-at-point-functions.
(use-package cape
  :commands (cape-dabbrev cape-file cape-elisp-block)
  :bind ("C-c p" . cape-prefix-map)
  :init
  ;; Add to the global default value of `completion-at-point-functions' which is
  ;; used by `completion-at-point'.
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-elisp-block))

;; Vertico provides a vertical completion interface, making it easier to
;; navigate and select from completion candidates (e.g., when `M-x` is pressed).
(use-package vertico
  ;; (Note: It is recommended to also enable the savehist package.)
  :config
  (vertico-mode))

;; Vertico leverages Orderless' flexible matching capabilities, allowing users
;; to input multiple patterns separated by spaces, which Orderless then
;; matches in any order against the candidates.
(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

;; Marginalia allows Embark to offer you preconfigured actions in more contexts.
;; In addition to that, Marginalia also enhances Vertico by adding rich
;; annotations to the completion candidates displayed in Vertico's interface.
(use-package marginalia
  :commands (marginalia-mode marginalia-cycle)
  :hook (after-init . marginalia-mode))

;; The markdown-mode package provides a major mode for Emacs for syntax
;; highlighting, editing commands, and preview support for Markdown documents.
;; It supports core Markdown syntax as well as extensions like GitHub Flavored
;; Markdown (GFM).
(use-package markdown-mode
  :commands (gfm-mode
             gfm-view-mode
             markdown-mode
             markdown-view-mode)
  :mode (("\\.markdown\\'" . markdown-mode)
         ("\\.md\\'" . markdown-mode)
         ("README\\.md\\'" . gfm-mode))
  :bind
  (:map markdown-mode-map
        ("C-c C-e" . markdown-do)))

;; This automates the process of updating installed packages
(use-package auto-package-update
  :custom
  ;; Set the number of days between automatic updates.
  ;; Here, packages will only be updated if at least 7 days have passed
  ;; since the last successful update.
  (auto-package-update-interval 7)

  ;; Suppress display of the *auto-package-update results* buffer after updates.
  ;; This keeps the user interface clean and avoids unnecessary interruptions.
  (auto-package-update-hide-results t)

  ;; Automatically delete old package versions after updates to reduce disk
  ;; usage and keep the package directory clean. This prevents the accumulation
  ;; of outdated files in Emacs's package directory, which consume
  ;; unnecessary disk space over time.
  (auto-package-update-delete-old-versions t)

  ;; Uncomment the following line to enable a confirmation prompt
  ;; before applying updates. This can be useful if you want manual control.
  ;; (auto-package-update-prompt-before-update t)

  :config
  ;; Run package updates automatically at startup, but only if the configured
  ;; interval has elapsed.
  (auto-package-update-maybe)

  ;; Schedule a background update attempt daily at 10:00 AM.
  ;; This uses Emacs' internal timer system. If Emacs is running at that time,
  ;; the update will be triggered. Otherwise, the update is skipped for that
  ;; day. Note that this scheduled update is independent of
  ;; `auto-package-update-maybe` and can be used as a complementary or
  ;; alternative mechanism.
  (auto-package-update-at-time "10:00"))

;; Helpful is an alternative to the built-in Emacs help that provides much more
;; contextual information.
(use-package helpful
  :commands (helpful-callable
             helpful-variable
             helpful-key
             helpful-command
             helpful-at-point
             helpful-function)
  :bind
  ([remap describe-command] . helpful-command)
  ([remap describe-function] . helpful-callable)
  ([remap describe-key] . helpful-key)
  ([remap describe-symbol] . helpful-symbol)
  ([remap describe-variable] . helpful-variable)
  :custom
  (helpful-max-buffers 7))

(use-package bufferfile
  :commands (bufferfile-copy
             bufferfile-rename
             bufferfile-delete)
  :custom
  ;; If non-nil, display messages during file renaming operations
  (bufferfile-verbose t)

  ;; If non-nil, enable using version control (VC) when available
  (bufferfile-use-vc t)

  ;; Specifies the action taken after deleting a file and killing its buffer.
  (bufferfile-delete-switch-to 'parent-directory))

;;; Enable automatic insertion and management of matching pairs of characters
;;; (e.g., (), {}, "") globally using `electric-pair-mode'.
(use-package elec-pair
  :ensure nil
  :commands (electric-pair-mode
             electric-pair-local-mode
             electric-pair-delete-pair)
  :hook (after-init . electric-pair-mode))

(use-package which-key
  :ensure nil ; builtin
  :commands which-key-mode
  :hook (after-init . which-key-mode)
  :custom
  (which-key-idle-delay 1.5)
  (which-key-idle-secondary-delay 0.25)
  (which-key-add-column-padding 1)
  (which-key-max-description-length 40))

;; Display the time in the modeline
(add-hook 'after-init-hook #'display-time-mode)

;; Track changes in the window configuration, allowing undoing actions such as
;; closing windows.
(setq winner-boring-buffers '("*Completions*"
                                "*Minibuf-0*"
                                "*Minibuf-1*"
                                "*Minibuf-2*"
                                "*Minibuf-3*"
                                "*Minibuf-4*"
                                "*Compile-Log*"
                                "*inferior-lisp*"
                                "*Fuzzy Completions*"
                                "*Apropos*"
                                "*Help*"
                                "*cvs*"
                                "*Buffer List*"
                                "*Ibuffer*"
                                "*esh command on file*"))
(add-hook 'after-init-hook #'winner-mode)

;;
;; End of lines taken from `https://github.com/jamescherti/minimal-emacs.d'
;;

(provide 'post-init)
