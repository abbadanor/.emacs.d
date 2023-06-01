;; -*- lexical-binding: t; -*-

;; Plugins to add
;; TODO: eslint for web-mode and typescript, with auto-fix
;; TODO: navigate windows like with C-w in vim
;; TODO: add emoji support
;; TODO: only highlight uppercase todo keywords
;; TODO: doom-modeline + nerd-icons
;; TODO: org-modern
;; TODO: chatgpt
;; TODO: corfu icons (kind-icons) 
;; DONE: add highlighting for todo keywords

;; Meow
;; TODO: add paste above with P (no idea how to do this, maybe ask discord or reddit)
;;       apparently this doesn't exist in vim?
;; TODO: add "thing" configuration to meow-setup
;; TODO: make C-o and C-i work properly
;; TODO: zd for goto definition etc.
;; TODO: disable lsp-ui elements
;; DONE: add angle brackets to meow "things" list
;; DONE: replace mode
;; DONE: c should change to end of line
;; KILL: SPC-y, SPC-d and SPC-p to interact with clipboard
;; (C-y for paste and M-w for copy works fine)
;; KILL: D should change to end of line
;; (using d in normal mode works by default)
;; KILL: isearch or maybe avy with /
;; (will test out meow-visit and see if it works fine)

;; Bugs & fixes
;; TODO: get rid of chinese thing in modeline
;; TODO: smart-parens should not insert two ' in elisp-mode
;; TODO: C-u should go to top of file when spammed
;; TODO: fix trash appearing in user-emacs-folder

;; Aesthetics
;; TODO: minimal startup screen

;; Emacs
;; TODO: modularize init.el, and add keybind to search modules with consult
;; TODO: isearch alternative or at least case-insensitivity and wrap-around
;; TODO: linear undo and redo system (with vundo support)
;; TODO: consult-line with / or C-f
;; TODO: "lazy" isearch (show 1/9 results in modeline, search reddit)
;; DONE: disable line wrapping in prog-mode
;; DONE: S-return inserts line below in normal mode
;; DONE: something like evil-nerd-commenter for commenting line in normal mode

;; Things to do
;; meow: SPC-c-x-a and some other keys are the only ones that turn into C-c C-x etc. Find out these keys.
;; meow: use tree-sitter instead or combined with things, ask GitHub about ci" equivalant when at beginning of line
;; treesit: redo native treesitter with vue grammar
;; vscode: compare features with vscode, copy over keybinds and settings

;; Things to consider
;; meow: add hook when exiting insert mode:
;; (add-hook 'meow-insert-exit-hook 'quit-something)

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

(setq native-comp-async-report-warnings-errors nil)

;; Set the right directory to store the native comp cache
(add-to-list 'native-comp-eln-load-path (expand-file-name "eln-cache/" user-emacs-directory))

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

(elpaca use-package)

(elpaca elpaca-use-package
	(elpaca-use-package-mode)
	(setq elpaca-use-package-by-default t))

(elpaca-wait)

(server-start)

;; Thanks, but no thanks
(setq inhibit-startup-message t)
(setq scroll-margin 4)
(setq scroll-step 1)
(setq scroll-conservatively 10000)
(setq scroll-preserve-screen-position 1)
(setq switch-to-buffer-obey-display-actions t)

(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)
(setq-default indicate-empty-lines t)

;; sets empty line indicator to tilde and removes arrows in the right margin
(define-fringe-bitmap 'tilde [0 0 0 113 219 142 0 0] nil nil 'center)
(setq-default fringe-indicator-alist '((empty-line . tilde)))

(savehist-mode)
(save-place-mode)
(recentf-mode)
(column-number-mode) ;; see column number in modeline (row,col)

;; Enable line numbers for some modes
(dolist (mode '(text-mode-hook
                prog-mode-hook
                conf-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 1))))

;; Override org-mode, which derives from the above
(add-hook 'org-mode-hook (lambda () (display-line-numbers-mode 0)))

;; Do not wrap lines in prog-mode
(add-hook 'prog-mode-hook (lambda () (setq truncate-lines t)))

(load-theme 'modus-operandi t)

;; Set the fixed pitch face
(set-face-attribute 'default nil
                    :font "JetBrains Mono"
                    :height 120)

(use-package vundo
  :bind (:map vundo-mode-map
              ("h". vundo-backward)
              ("j". vundo-next)
              ("k". vundo-previous)
              ("l". vundo-forward)
              ("q". vundo-quit)
              ("RET". vundo-confirm)))

(use-package drag-stuff
  :init
  (drag-stuff-mode t))

 ;; Go to next mark

(defun my-meow-replace ()
  (interactive)
  ;; if theres a selection
  (if (use-region-p)
      (meow-replace)
    ;; if theres no selection
  (setq cursor-type 'hbar)
  (setq mychar (read-char))
  ;; \e is escape key
  (if (= ?\e mychar)
      (message "Cancelled replace")
    (delete-char 1)
    (insert mychar)
    (backward-char))
  (setq cursor-type 'box)))

;; c does not behave like d by default, this function fixes this
(defun my-meow-change ()
  (interactive)
  ;; if there's no selection, select to end of line
  (unless (use-region-p)
    (let ((line (car (rassoc 'line meow-char-thing-table))))
      (meow-end-of-thing line)))
  (meow-change))

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
   '("f p" . (lambda () (interactive) (find-file user-init-file)))
   '("f P" . (lambda () (interactive) (find-file (expand-file-name "early-init.el" user-emacs-directory))))
   '("p f" . project-find-file)
   '("p p" . project-switch-project)
   '("," . consult-buffer)
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
   '("c" . my-meow-change)
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
   '("J" . drag-stuff-down)
   '("k" . meow-prev)
   '("K" . drag-stuff-up)
   '("l" . meow-right)
   '("L" . meow-right-expand)
   '("m" . meow-join)
   '("n" . meow-search)
   '("o" . meow-block)
   '("O" . meow-to-block)
   '("p" . meow-yank)
   ;; TODO: '("P" . meow-yank-above)
   '("q" . meow-quit)
   '("Q" . meow-goto-line)
   '("r" . my-meow-replace)
   '("d" . meow-kill)
   '("t" . meow-till)
   '("u" . meow-undo) 
   '("U" . vundo)
   '("/" . meow-visit)
   '("w" . meow-mark-word)
   '("W" . meow-mark-symbol)
   '("s" . meow-line)
   '("S" . meow-goto-line) ;; quite useless
   '("y" . meow-save)
   '("Y" . meow-sync-grab)
   '("z" . meow-pop-selection)
   '("'" . repeat)
   '("=" . meow-indent)
   '("<escape>" . ignore)))

(use-package meow
  :custom
  (meow-use-enhanced-selection-effect t)
  (meow-expand-hint-remove-delay 0)
  :config
  (meow-thing-register 'tags
                       '(regexp "<.+>" "</.+>")
                       '(regexp "<.+>" "</.+>"))
  (meow-thing-register 'angle '(regexp "<" ">") '(regexp "<" ">"))
  (setq meow-char-thing-table
        '((?\( . round)
          (?\[ . square)
          (?\{ . curly)
          (?s . string)
          (?S . symbol)
          (?w . window)
          (?b . buffer)
          (?p . paragraph)
          (?l . line)
          (?v . visual-line)
          (?f . defun)
          (?t . tags)
          (?a . angle)
          (?. . sentence)))
  (meow-setup)
  (meow-global-mode 1))

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

(defun my-open-line-below ()
  (interactive)
  (move-end-of-line 1)
  (newline))

(defun my-pop-local-mark-ring ()
  (interactive)
  (set-mark-command t))

(defun my-comment-dwim ()
  (interactive)
  ;; if there is selection comment selection, otherwise comment line
  (if (use-region-p)
      (comment-or-uncomment-region (region-beginning) (region-end))
    (comment-line 1)))

(global-set-key (kbd "S-<return>") 'my-open-line-below)
(global-set-key (kbd "M-;") 'my-comment-dwim)

(global-set-key (kbd "C-o") 'pop-global-mark) ;; Go to previous mark
(global-set-key (kbd "C-i") 'my-pop-local-mark-ring)

(global-set-key (kbd "C-u") 'scroll-half-page-up)
(global-set-key (kbd "C-d") 'scroll-half-page-down)
(global-set-key (kbd "C-SPC") 'completion-at-point)

(use-package vertico
  :custom (vertico-cycle t)
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

;; TODO: decrease delay
(use-package corfu
  :bind (:map corfu-map
	            ("<escape>" . corfu-quit)
	            ("C-j" . corfu-next)
	            ("C-k" . corfu-previous))
  ;; Optional customizations
  :custom
  (corfu-auto t)                 ;; Enable auto completion
  (corfu-auto-prefix 2)                 ;; Enable auto completion
  (corfu-auto-delay 0.1)                 ;; Enable auto completion
  ;; (corfu-separator ?\s)          ;; Orderless field separator
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect 'prompt)      ;; Preselect the prompt
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  ;; (corfu-scroll-margin 5)        ;; Use scroll margin
  :init
  (global-corfu-mode))

(use-package corfu-popupinfo
  :elpaca nil
  :load-path "elpaca/repos/corfu/extensions/"
  :after corfu
  :config
  (corfu-popupinfo-mode))

(use-package corfu-history
  :elpaca nil
  :load-path "elpaca/repos/corfu/extensions/"
  :after corfu
  :config
  (corfu-history-mode))

(use-package marginalia
  :after vertico
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  :init
  (marginalia-mode))

(use-package cape
  ;; Add `completion-at-point-functions', used by `completion-at-point'.
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block))

(use-package helpful
  :bind
  ([remap describe-function] . helpful-function)
  ([remap describe-symbol] . helpful-symbol)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-command] . helpful-command)
  ([remap describe-key] . helpful-key))

(use-package avy
  :config
  (global-set-key (kbd "C-;") 'avy-goto-char-timer))

(use-package dirvish
  :config
  (dirvish-override-dired-mode))

;; TODO: add more modes (html, css, jsx, tsx)
(use-package web-mode
  :mode "\\.vue\\'"
  :config
  (setq-default web-mode-code-indent-offset 2)
  (setq-default web-mode-markup-indent-offset 2)
  (setq-default web-mode-attribute-indent-value 2))

(use-package typescript-mode
  :mode "\\.ts\\'"
  :config
  (setq typescript-indent-level 2))

(use-package tree-sitter
  :config
  (global-tree-sitter-mode))

(add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)

(use-package tree-sitter-langs
  :after tree-sitter)

(use-package smartparens
  :hook (prog-mode . smartparens-mode))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(defun toggle-darkroom-mode ()
  (interactive)
  (if (bound-and-true-p darkroom-mode)
      (progn (unless (eq major-mode 'org-mode)
               (display-line-numbers-mode 1))
             (darkroom-mode -1))
    (display-line-numbers-mode -1)
    (darkroom-mode 1)))

(use-package darkroom
  :commands darkroom-mode
  :config
  (setq darkroom-text-scale-increase 0))

;; the color comments correspond to modus-themes-operandi-colors colors
(use-package hl-todo
  :custom
  (hl-todo-keyword-faces '(("TODO" . "#4faa09") ;; green-graph-0-bg
                           ("DONE". "#505050") ;; fg-alt
                           ("KILL" . "#b60000"))) ;; red-intense
  :init
  (global-hl-todo-mode))

;; For colored hex strings
(use-package rainbow-mode
  :defer) 

(use-package lsp-mode
  :hook ((web-mode typescript-mode) . lsp)
  :custom (lsp-headerline-breadcrumb-enable nil)
  :config
  (setq lsp-eslint-auto-fix-on-save t))

(use-package flycheck
  :hook (lsp-mode . flycheck-mode))
  
(defun lsp--eslint-before-save (orig-fun)
  "Run lsp-eslint-apply-all-fixes and then run the original lsp--before-save."
  (when lsp-eslint-auto-fix-on-save (lsp-eslint-fix-all))
  (funcall orig-fun))

(advice-add 'lsp--before-save :around #'lsp--eslint-before-save)

;; Make gc pauses faster by decreasing the threshold.
(setq gc-cons-threshold (* 2 1000 1000))

