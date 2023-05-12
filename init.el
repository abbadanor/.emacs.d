;; -*- lexical-binding: t; -*-

;; The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))

;; Profile emacs startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "*** Emacs loaded in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

;; Silence compiler warnings as they can be pretty disruptive
(setq native-comp-async-report-warnings-errors nil)

;; Set the right directory to store the native comp cache
(add-to-list 'native-comp-eln-load-path (expand-file-name "eln-cache/" user-emacs-directory))

(add-to-list 'exec-path "~/.local/share/pnpm/")

(defvar elpaca-installer-version 0.4)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil
                              :files (:defaults (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                 ((zerop (call-process "git" nil buffer t "clone"
                                       (plist-get order :repo) repo)))
                 ((zerop (call-process "git" nil buffer t "checkout"
                                       (or (plist-get order :ref) "--"))))
                 (emacs (concat invocation-directory invocation-name))
                 ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                       "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                 ((require 'elpaca))
                 ((elpaca-generate-autoloads "elpaca" repo)))
            (kill-buffer buffer)
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

(elpaca elpaca-use-package
	(elpaca-use-package-mode)
	(setq elpaca-use-package-by-default t))

(elpaca-wait)

;; Thanks, but no thanks
(setq inhibit-startup-message t)
(setq scroll-margin 4)
(setq scroll-step 1)
(setq switch-to-buffer-obey-display-actions t)

(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)


(savehist-mode)
(save-place-mode)
(recentf-mode)
(column-number-mode)
(display-line-numbers-mode)

(load-theme 'modus-operandi t)

;; Set the fixed pitch face
(set-face-attribute 'default nil
                    :font "JetBrains Mono"
                    :height 120)

(defun scroll-half-page (direction)
  "Scrolls half page up if `direction' is non-nil, otherwise will scroll half page down."
  (let ((opos (cdr (nth 6 (posn-at-point)))))
    ;; opos = original position line relative to window
    (move-to-window-line nil)  ;; Move cursor to middle line
    (if direction
        (recenter-top-bottom -1)  ;; Current line becomes last
      (recenter-top-bottom 0))  ;; Current line becomes first
    (move-to-window-line opos)))  ;; Restore cursor/point position

(defun scroll-half-page-down ()
  "Scrolls exactly half page down keeping cursor/point position."
  (interactive)
  (scroll-half-page nil))

(defun scroll-half-page-up ()
  "Scrolls exactly half page up keeping cursor/point position."
  (interactive)
  (scroll-half-page t))

(defun meow-setup ()
  (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
  (meow-motion-overwrite-define-key
   '("j" . meow-next)
   '("k" . meow-prev)
   '("<escape>" . ignore))
  (meow-leader-define-key
   ;; SPC j/k will run the original command in MOTION state.
   '("j" . "H-j")
   '("k" . "H-k")
   '("f r" . consult-recent-file)
   '("b i" . ibuffer)
   '("b k" . kill-this-buffer)
   ;; Use SPC (0-9) for digit arguments.
   '("1" . meow-digit-argument)
   '("2" . meow-digit-argument)
   '("3" . meow-digit-argument)
   '("4" . meow-digit-argument)
   '("5" . meow-digit-argument)
   '("6" . meow-digit-argument)
   '("7" . meow-digit-argument)
   '("8" . meow-digit-argument)
   '("9" . meow-digit-argument)
   '("0" . meow-digit-argument)
   '("/" . meow-keypad-describe-key)
   '("?" . meow-cheatsheet))
  (meow-normal-define-key
   '("0" . meow-expand-0)
   '("9" . meow-expand-9)
   '("8" . meow-expand-8)
   '("7" . meow-expand-7)
   '("6" . meow-expand-6)
   '("5" . meow-expand-5)
   '("4" . meow-expand-4)
   '("3" . meow-expand-3)
   '("2" . meow-expand-2)
   '("1" . meow-expand-1)
   '("-" . negative-argument)
   '(";" . meow-reverse)
   '("," . meow-inner-of-thing)
   '("." . meow-bounds-of-thing)
   '("[" . meow-beginning-of-thing)
   '("]" . meow-end-of-thing)
   '("a" . meow-append)
   '("A" . meow-open-below)
   '("b" . meow-back-word)
   '("B" . meow-back-symbol)
   '("c" . meow-change)
   '("x" . delete-char) ;; changed
   ;; '("X" . meow-backward-delete) ;; changed
   '("e" . meow-next-word)
   '("E" . meow-next-symbol)
   '("f" . meow-find)
   '("g" . meow-cancel-selection)
   '("G" . meow-grab)
   '("h" . meow-left)
   '("H" . meow-left-expand)
   '("i" . meow-insert)
   '("I" . meow-open-above)
   '("j" . meow-next)
   '("J" . meow-next-expand)
   '("k" . meow-prev)
   '("K" . meow-prev-expand)
   '("l" . meow-right)
   '("L" . meow-right-expand)
   '("m" . meow-join)
   '("n" . meow-search)
   '("o" . meow-block)
   '("O" . meow-to-block)
   '("p" . meow-yank)
   '("q" . meow-quit)
   '("Q" . meow-goto-line)
   '("r" . meow-replace)
   '("R" . meow-swap-grab)
   '("d" . meow-kill) ;; changed
   '("t" . meow-till)
   '("u" . meow-undo)
   '("U" . meow-undo-in-selection)
   '("v" . meow-visit)
   '("w" . meow-mark-word)
   '("W" . meow-mark-symbol)
   '("s" . meow-line) ;; changed
   '("S" . meow-goto-line) ;; changed
   '("y" . meow-save)
   '("Y" . meow-sync-grab)
   '("z" . meow-pop-selection)
   ;; '("C-u" . scroll-half-page-up)
   ;; '("C-d" . scroll-half-page-down)
   '("'" . repeat)
   '("=" . meow-indent)
   '("<escape>" . ignore)))

(use-package meow
  :custom
  (meow-use-enhanced-selection-effect t)
  (meow-expand-hint-remove-delay 0)
  (meow-char-thing-table
   '((114 . round)
     (104 . square)
     (99 . curly)
     (115 . string)
     (101 . symbol)
     (119 . window)
     (98 . buffer)
     (112 . paragraph)
     (108 . line)
     (118 . visual-line)
     (100 . defun)
     (46 . sentence)))
  :config
  (meow-setup)
  (meow-global-mode 1))

(global-set-key (kbd "C-u") 'scroll-half-page-up)
(global-set-key (kbd "C-d") 'scroll-half-page-down)
(global-set-key (kbd "C-SPC") 'completion-at-point)

(use-package vertico
  :bind (:map vertico-map
	            ("C-j" . vertico-next)
	            ("C-k" . vertico-previous))
  :init
  (vertico-mode))

(use-package vertico-directory
  :elpaca nil
  :load-path "elpaca/repos/vertico/extensions/"
  :after vertico
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word)))

(use-package orderless
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package consult
  :init
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref))

(use-package corfu
  :elpaca (corfu :files (:defaults "extensions/*"))
  :bind (:map corfu-map
	            ("<escape>" . corfu-quit)
	            ("C-j" . corfu-next)
	            ("C-k" . corfu-previous))
  ;; Optional customizations
  :custom
  (corfu-auto t)                 ;; Enable auto completion
  ;; (corfu-separator ?\s)          ;; Orderless field separator
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect 'prompt)      ;; Preselect the prompt
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  ;; (corfu-scroll-margin 5)        ;; Use scroll margin

  :init
  (global-corfu-mode))

;; Add extensions
(use-package cape
  :init
  ;; Add `completion-at-point-functions', used by `completion-at-point'.
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block))

(use-package avy
  :config
  (global-set-key (kbd "C-;") 'avy-goto-char-timer))

(use-package ace-window
  :bind (("M-o" . ace-window))
  :custom
  (aw-scope 'frame)
  (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (aw-minibuffer-flag t)
  (aw-dispatch-always t)
  :config
  (ace-window-display-mode 1))

(use-package web-mode
  :mode "(\\.\\(html?\\|tsx\\|jsx\\)\\'"
  :config
  (setq-default web-mode-code-indent-offset 2)
  (setq-default web-mode-markup-indent-offset 2)
  (setq-default web-mode-attribute-indent-offset 2))

(define-derived-mode vue-mode web-mode "Vue Mode")
(add-to-list 'auto-mode-alist '("\\.vue\\'" . vue-mode))

(use-package tree-sitter
  :config
  (global-tree-sitter-mode))

(add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)

(use-package tree-sitter-langs
  :after tree-sitter)

(use-package smartparens
  :hook (prog-mode . smartparens-mode))

(use-package eglot
  :hook
  (python-mode . eglot-ensure)
  (vue-mode . eglot-ensure)
  :config
  (add-to-list 'eglot-server-programs
               '(vue-mode . (eglot-volar "vue-language-server" "--stdio")))
  (defclass eglot-volar (eglot-lsp-server) ()
    :documentation "A custom class for volar")

  (cl-defmethod eglot-initialization-options ((server eglot-volar))
    "Passes through required volar initialization options"
    (let*
        ((serverPath
          (expand-file-name
           "lib/tsserverlibrary.js"
           (shell-command-to-string "pnpm -g --parseable list typescript | head -n1"))))
      (list :typescript
            (list :serverPath serverPath)
            :languageFeatures
            (list :completion
                  (list :defaultTagNameCase "both"
                        :defaultAttrNameCase "kebabCase"
                        :getDocumentNameCasesRequest nil
                        :getDocumentSelectionRequest nil)
                  :diagnostics
                  (list :getDocumentVersionRequest nil))
            :documentFeatures
            (list :documentFormatting
                  (list :defaultPrintWidth 100
                        :getDocumentPrintWidthRequest nil)
                  :documentSymbol t
                  :documentColor t)))))

;; Make gc pauses faster by decreasing the threshold.
(setq gc-cons-threshold (* 2 1000 1000))
