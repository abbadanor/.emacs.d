;; -*- lexical-binding: t; -*-
;; This buffer is for text that is not saved, and for Lisp evaluation.

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

;; Bootstrap straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Always use straight to install on systems other than Linux
(setq straight-use-package-by-default t)

;; Add elisp code to load path
(add-to-list 'load-path (expand-file-name "elisp/" user-emacs-directory))
(require 'org-dwim)

(add-to-list 'exec-path (expand-file-name "~/.local/share/nvm/versions/node/v16.20.0/bin/"))
(add-to-list 'exec-path (expand-file-name "~/.local/share/pnpm/"))

;; Change the user-emacs-directory to keep unwanted things out of ~/.emacs.d
(setq user-emacs-directory (expand-file-name "~/.cache/emacs/")
      url-history-file (expand-file-name "url/history" user-emacs-directory)
      save-place-file (expand-file-name "save-place" user-emacs-directory))

(use-package no-littering)

(server-start)

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(use-package undo-tree
  :init
  (global-undo-tree-mode 1))

(use-package drag-stuff
  :init
  (drag-stuff-mode t))

(defun an/evil-shift-right ()
  (interactive)
  (evil-shift-right evil-visual-beginning evil-visual-end)
  (evil-normal-state)
  (evil-visual-restore))

(defun an/evil-shift-left ()
  (interactive)
  (evil-shift-left evil-visual-beginning evil-visual-end)
  (evil-normal-state)
  (evil-visual-restore))

(defun an/evil-yank-advice (orig-fn beg end &rest args)
  (pulse-momentary-highlight-region beg end)
  (apply orig-fn beg end args))

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump t)
  (setq evil-respect-visual-line-mode t)
  (setq evil-undo-system 'undo-tree)
  (setq evil-kill-on-visual-paste nil)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-visual-state-map (kbd "<") 'an/evil-shift-left)
  (define-key evil-visual-state-map (kbd ">") 'an/evil-shift-right)
  (define-key evil-visual-state-map (kbd "J") 'drag-stuff-down)
  (define-key evil-visual-state-map (kbd "K") 'drag-stuff-up)
  (define-key evil-normal-state-map (kbd "gr") 'xref-find-references)
  (define-key evil-motion-state-map (kbd "C-f") 'consult-line)

  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

(advice-add 'evil-yank :around #'an/evil-yank-advice)

(use-package evil-collection
  :after evil
  :custom
  (evil-collection-outline-bind-tab-p nil)
  :config
  (delete 'org-present evil-collection-mode-list)
  (evil-collection-init))

;; From org-modern readme
;; Add frame borders and window dividers
;; (modify-all-frames-parameters
;;  '((right-divider-width . 12)
;;    (internal-border-width . 12)))
;; (dolist (face '(window-divider
;;                 window-divider-first-pixel
;;                 window-divider-last-pixel))
;;   (face-spec-reset-face face)
;;   (set-face-foreground face (face-attribute 'default :background)))
;; (set-face-background 'fringe (face-attribute 'default :background))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-show-early-on-C-h t)
  (setq which-key-idle-delay 10000)
  (setq which-key-idle-secondary-delay 0.05))

(use-package which-key-posframe
  :config
  (which-key-posframe-mode))

(defun an/reload-init-file ()
  (interactive)
  (when (string-equal buffer-file-name user-init-file)
    (eval-region (evil-line-position 44) (point-max))))

(use-package general
  :config
  (general-evil-setup t)

  (general-create-definer an/leader-def
    :keymaps '(normal visual emacs)
    :prefix "SPC"
    :global-prefix "C-c C-v")

  (an/leader-def
    "." '(find-file :which-key "Dired")
    "f"  '(:ignore t :which-key "Find")
    "fr" '(consult-recent-file :which-key "Find recent file")
    "b"  '(:ignore t :which-key "Buffers")
    "bp" '(previous-buffer :which-key "Go to previous buffer")
    "bn" '(next-buffer :which-key "Go to next buffer")
    "bk" '(kill-this-buffer :which-key "Kill buffer")
    "bs" '(consult-buffer :which-key "Search buffers")
    "bi" '(ibuffer :which-key "Open IBuffer")
    "r"  '(:ignore t :which-key "Search")
    "sh" '(consult-org-heading :which-key "Org headings")
    "sb" '(consult-bookmark :which-key "Bookmarks")
    "sm" '(consult-minor-mode-menu :which-key "Minor modes")
    "r"  '(:ignore t :which-key "Org Roam")
    "rf" '(org-roam-node-find :which-key "Find or create node")
    "ri" '(org-roam-node-insert :which-key "Insert node at point")
    "e"  '(:ignore t :which-key "Eval")
    "eb" '(eval-buffer :which-key "Evaluate buffer")
    "er" '(eval-region :which-key "Evaluate region")
    "ec" '(an/reload-init-file :which-key "Reload init.el")
    "t"  '(:ignore t :which-key "Toggle")
    "tf" '(an/toggle-focus-mode :which-key "Toggle darkroom mode")
    "tu" '(an/toggle-focus-mode :which-key "Open undo tree")
    "o"  '(:ignore t :which-key "Org mode")
    "oi" '(org-insert-link :which-key "Insert link at point")
    "on"  'org-toggle-narrow-to-subtree ;; TODO check out
    "oc"  'org-capture ;; TODO check out
    "l"  '(:ignore t :which-key "LSP")
    "ln" 'flycheck-next-error
    "lp" 'flycheck-previous-error
    "le" 'lsp-ui-flycheck-list
    "lx" 'lsp-execute-code-action
    "SPC" '(project-find-file :which-key "Find file in project"))

  (general-define-key
   "C-s" 'save-buffer)

  (general-imap
    "C-SPC" 'completion-at-point))

;; (defun an/cheat-sheet ()
;;   (interactive)
;;   (which-key--create-buffer-and-show (kbd "SPC")))

(setq inhibit-startup-message t)

(setq scroll-conservatively 101
      scroll-margin 6
      scroll-preserve-screen-position 't)

(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse

(column-number-mode)

;; Enable line numbers for some modes
(dolist (mode '(text-mode-hook
                prog-mode-hook
                conf-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 1))))

;; Override some modes which derive from the above
(dolist (mode '(org-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(recentf-mode)
(save-place-mode)

(setq large-file-warning-threshold nil)
(setq vc-follow-symlinks t)
(setq ad-redefinition-action 'accept)


(use-package doom-themes :defer t)
(load-theme 'doom-palenight t)
;; (load-theme 'modus-operandi t)

;; Choose some fonts
(set-face-attribute 'default nil :family "JetBrains Mono" :height 110)
(set-face-attribute 'fixed-pitch nil :family "JetBrains Mono" :height 1.0)
(set-face-attribute 'variable-pitch nil :family "Iosevka Aile" :height 110)

(use-package emojify
  :commands emojify-mode)

(setq display-time-format "%l:%M %p %b %y"
      display-time-default-load-average nil)

(use-package diminish)

(use-package all-the-icons)

(use-package minions
  :hook (doom-modeline-mode . minions-mode))

(use-package doom-modeline
  ;; :custom-face
  ;; (mode-line ((t (:height 0.85))))
  ;; (mode-line-inactive ((t (:height 0.85))))
  :custom
  (doom-modeline-height 40)
  ;; (doom-modeline-bar-width 6)
  (doom-modeline-lsp t)
  (doom-modeline-github nil)
  (doom-modeline-mu4e nil)
  ;; (doom-modeline-irc t)
  (doom-modeline-minor-modes t)
  (doom-modeline-persp-name nil)
  (doom-modeline-buffer-file-name-style 'truncate-except-project)
  (doom-modeline-major-mode-icon t)
  :config
  (doom-modeline-mode))

(use-package super-save
  :defer 1
  :diminish super-save-mode
  :config
  (super-save-mode +1)
  (setq super-save-auto-save-when-idle t))

;; Revert Dired and other buffers
(setq global-auto-revert-non-file-buffers t)

;; Revert buffers when the underlying file has changed
(global-auto-revert-mode 1)

(use-package paren
  :config
  (show-paren-mode 1))

(setq-default tab-width 2)
(setq-default evil-shift-width tab-width)
(setq-default indent-tabs-mode nil)

(use-package evil-nerd-commenter
  :config
  (general-nvmap "gc" 'evilnc-comment-or-uncomment-lines))

(use-package origami
  :hook (yaml-mode . origami-mode))

(use-package ws-butler
  :hook ((text-mode . ws-butler-mode)
         (prog-mode . ws-butler-mode)))

(an/leader-def
  "fh" (lambda () (interactive) (consult-find "~/"))
  "fd" (lambda () (interactive) (consult-find "~/dev/"))
  "fP" (lambda () (interactive) (find-file "~/.config/emacs/early-init.el"))
  "fp" (lambda () (interactive) (find-file "~/.config/emacs/init.el")))
(which-key-add-key-based-replacements "SPC f h" "Find in home directory")
(which-key-add-key-based-replacements "SPC f d" "Find in dev directory")
(which-key-add-key-based-replacements "SPC f P" "Edit early-init.el")
(which-key-add-key-based-replacements "SPC f p" "Edit init.el")

(use-package savehist
  :config
  (setq history-length 25)
  (savehist-mode 1))

(use-package vertico
  :straight (vertico :files (:defaults "extensions/*")
                     :includes (vertico-directory vertico-multiform))
  :bind (:map vertico-map
              ("C-j" . vertico-next)
              ("C-k" . vertico-previous)
              ("\r" . vertico-directory-enter)
              ("\d" . vertico-directory-delete-char)
              ("\M-\d" . vertico-directory-delete-word))
  :custom
  (vertico-cycle t)
  ;; :config
  ;; (setq vertico-multiform-commands
  ;;       '((consult-line buffer)
  ;;         (consult-imenu reverse buffer)))
          ;; (execute-extended-command flat)))

  ;; :custom-face
  ;; (vertico-current ((t (:background "#3a3f5a"))))
  :init
  ;; (vertico-multiform-mode)
  (vertico-mode))

;; (use-package vertico-posframe
;;   :after vertico
;;   :config
;;   (setq vertico-multiform-commands
;;         '((execute-extended-command
;;            posframe
;;            (vertico-posframe-poshandler . posframe-poshandler-frame-top-center)
;;            (vertico-posframe-width . 75)
;;            ;; NOTE: This is useful when emacs is used in both in X and
;;            ;; terminal, for posframe do not work well in terminal, so
;;            ;; vertico-buffer-mode will be used as fallback at the
;;            ;; moment.
;;            (vertico-posframe-fallback-mode . vertico-buffer-mode))
;;           )))

(use-package corfu
  :straight (corfu :files (:defaults "extensions/*")
                   :includes (corfu-popupinfo corfu-history))
  :bind (:map corfu-map
              ("C-j" . corfu-next)
              ("C-k" . corfu-previous))
  :custom
  (corfu-cycle t)                 ; Allows cycling through candidates
  (corfu-auto t)                  ; Enable auto completion
  (corfu-auto-prefix 2)
  (corfu-auto-delay 0.2)
  (corfu-popupinfo-delay '(0.5 . 0.2))
  (corfu-preview-current 'insert) ; Do not preview current candidate
  (corfu-preselect-first nil)
  (corfu-on-exact-match nil)      ; Don't auto expand tempel snippets
  :init
  (global-corfu-mode)
  (corfu-popupinfo-mode)
  (corfu-history-mode))

(use-package orderless
  :custom
  (completion-styles '(orderless))
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package consult
  :demand t
  :bind (:map minibuffer-local-map ("C-r" . consult-history))
  :init
  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  :custom
  (completion-in-region-function #'consult-completion-in-region))

(use-package consult-dir
  :bind(:map vertico-map
             ("M-d" . consult-dir)
             ("M-f" . consult-dir-jump-file)))

(use-package marginalia
  :after vertico
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  :init
  (marginalia-mode))

(use-package embark
  :bind (:map minibuffer-local-map
              ("M-e" . embark-act)))

(use-package embark-consult
  :after embark)

;;The keybindings for this are <C+M+=> and <C+M+->
(use-package default-text-scale
  :defer 1
  :config
  (default-text-scale-mode))

(use-package winner
  :after evil
  :config
  (winner-mode)
  (define-key evil-window-map "u" 'winner-undo)
  (define-key evil-window-map "U" 'winner-redo))

(defun an/org-mode-visual-fill ()
  (setq visual-fill-column-width 110
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :defer t
  :hook (org-mode . an/org-mode-visual-fill))

(setq even-window-sizes nil)

(use-package expand-region
  :bind (("M-[" . er/expand-region)
         ("C-(" . er/mark-outside-pairs)))

(use-package all-the-icons-dired)

(use-package dired
  :ensure nil
  :straight nil
  :defer 1
  :commands (dired dired-jump)
  :config
  (setq dired-listing-switches "-agho --group-directories-first" ;; TODO: what is -agho (ls flags)
        dired-omit-files "^\\.[^.].*" ;; TODO: what does this regex do?
        dired-omit-verbose nil
        dired-hide-details-hide-symlink-targets nil
        delete-by-moving-to-trash t)

  (autoload 'dired-omit-mode "dired-x")

  (add-hook 'dired-load-hook
            (lambda ()
              (interactive)
              (dired-collapse)))

  (add-hook 'dired-mode-hook
            (lambda ()
              (interactive)
              (dired-omit-mode 1)
              (dired-hide-details-mode 1)
              (all-the-icons-dired-mode 1))
            (hl-line-mode 1))

  (use-package dired-single
    :defer t)

  (use-package dired-ranger
    :defer t)

  (use-package dired-collapse
    :defer t)

  (general-def 'normal dired-mode-map
    "C-h" 'dired-omit-mode
    "SPC" nil)
  (evil-collection-define-key 'normal 'dired-mode-map
    "h" 'dired-single-up-directory
    "l" 'dired-single-buffer
    "y" 'dired-ranger-copy
    "X" 'dired-ranger-move
    "p" 'dired-ranger-paste))

;; TODO: fix this
(use-package hl-todo
  :config
  (setq hl-todo-keyword-faces
        '(("TODO"  .  (doom-color 'green))
          ("DONE"  .  (doom-color 'fg-alt))))
  (global-hl-todo-mode))


;; DONE: Mode this to another section
(setq-default fill-column 80)

;; Turn on indentation and auto-fill mode for Org files
(defun an/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (auto-fill-mode 0)
  (visual-line-mode 1)
  (setq evil-auto-indent nil)
  (diminish org-indent-mode))

(use-package org
  :defer t
  :hook (org-mode . an/org-mode-setup)
  :config
  (setq org-ellipsis " ▾"
        org-hide-emphasis-markers t
        org-src-fontify-natively t
        org-fontify-quote-and-verse-blocks t
        org-src-tab-acts-natively t
        org-edit-src-content-indentation 2
        org-hide-block-startup nil
        org-src-preserve-indentation nil
        org-startup-folded 'no-fold
        org-cycle-separator-lines 2
        org-capture-bookmark nil)

  (evil-define-key '(normal insert visual) org-mode-map (kbd "C-j") 'org-next-visible-heading)
  (evil-define-key '(normal insert visual) org-mode-map (kbd "C-k") 'org-previous-visible-heading)

  (org-babel-do-load-languages
    'org-babel-load-languages
    '((emacs-lisp . t)))

  (use-package org-superstar
    :after org
    :hook (org-mode . org-superstar-mode)
    :custom
    (org-superstar-remove-leading-stars t)
    (org-superstar-headline-bullets-list '("◉" "○" "●" "○" "●" "○" "●")))

  (use-package svg-tag-mode
    :after org
    :hook (org-mode . svg-tag-mode)
    :config
    (setq svg-tag-tags
          '(("TODO" . ((lambda (tag) (svg-tag-make "TODO"))))
            ("DONE" . ((lambda (tag) (svg-tag-make "DONE"))))
            ("\\(:#[A-Za-z0-9]+\\)" . ((lambda (tag) (svg-tag-make tag :beg 2))))
            ("\\(:#[A-Za-z0-9]+:\\)$" . ((lambda (tag) (svg-tag-make tag :beg 2 :end -1))))
            )))

;; Increase the size of various headings
(set-face-attribute 'org-document-title nil :font "Iosevka Aile" :weight 'bold :height 1.3)
(dolist (face '((org-level-1 . 1.2)
                (org-level-2 . 1.1)
                (org-level-3 . 1.05)
                (org-level-4 . 1.0)
                (org-level-5 . 1.1)
                (org-level-6 . 1.1)
                (org-level-7 . 1.1)
                (org-level-8 . 1.1)))
  (set-face-attribute (car face) nil :font "Iosevka Aile" :weight 'medium :height (cdr face)))

;; Make sure org-indent face is available
(require 'org-indent)

;; Ensure that anything that should be fixed-pitch in Org files appears that way
(set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
(set-face-attribute 'org-table nil  :inherit 'fixed-pitch)
(set-face-attribute 'org-formula nil  :inherit 'fixed-pitch)
(set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
(set-face-attribute 'org-indent nil :inherit '(org-hide fixed-pitch))
(set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
(set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
(set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
(set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch)

;; Get rid of the background on column views
(set-face-attribute 'org-column nil :background nil)
(set-face-attribute 'org-column-title nil :background nil)

;; This is needed as of Org 9.2
(require 'org-tempo)

(add-to-list 'org-structure-template-alist '("sh" . "src sh"))
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("li" . "src lisp"))
(add-to-list 'org-structure-template-alist '("sc" . "src scheme"))
(add-to-list 'org-structure-template-alist '("ts" . "src typescript"))
(add-to-list 'org-structure-template-alist '("py" . "src python"))
(add-to-list 'org-structure-template-alist '("go" . "src go"))
(add-to-list 'org-structure-template-alist '("yaml" . "src yaml"))
(add-to-list 'org-structure-template-alist '("json" . "src json"))

(use-package evil-org
  :after org
  :hook ((org-mode . evil-org-mode))
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys)))


;; (an/leader-def
;;   "o"   '(:ignore t :which-key "org mode")

;;   "oi"  '(:ignore t :which-key "insert")
;;   "oil" '(org-insert-link :which-key "insert link")

;;   "on"  '(org-toggle-narrow-to-subtree :which-key "toggle narrow")

;;   "os"  '(dw/counsel-rg-org-files :which-key "search notes")

;;   "oa"  '(org-agenda :which-key "status")
;;   "ot"  '(org-todo-list :which-key "todos")
;;   "oc"  '(org-capture t :which-key "capture")
;;   "ox"  '(org-export-dispatch t :which-key "export"))


(use-package org-roam
  :demand t
  :straight t
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory "~/org/roam/")
  (org-roam-completion-everywhere t)
  (org-roam-capture-templates
   '(("d" "default" plain
      "%?"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
      :unnarrowed t)))
  :config
  ;; (an/leader-def
  ;;   :keymaps 'org-mode-map
  ;;   "ri"  'org-roam-node-insert)
  (org-roam-db-autosync-mode))

(use-package org-appear
  :hook (org-mode . org-appear-mode))

(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . helpful-function)
  ([remap describe-symbol] . helpful-symbol)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-command] . helpful-command)
  ([remap describe-key] . helpful-key))

(use-package smartparens
  :hook (prog-mode . smartparens-mode)
  :config
  (sp-with-modes 'emacs-lisp-mode
    ;; disable ', it's the quote character!
    (sp-local-pair "'" nil :actions nil)
    ;; also only use the pseudo-quote inside strings where it
    ;; serves as hyperlink.
    (sp-local-pair "`" "'" :when '(sp-in-string-p sp-in-comment-p))))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package darkroom
  :commands darkroom-mode
  :config
  (setq darkroom-text-scale-increase 0))

(defun an/enter-focus-mode ()
  (interactive)
  (darkroom-mode 1)
  (display-line-numbers-mode 0))

(defun an/leave-focus-mode ()
  (interactive)
  (darkroom-mode 0)
  (display-line-numbers-mode 1))

(defun an/toggle-focus-mode ()
  (interactive)
  (if (and (boundp 'darkroom-mode) (symbol-value darkroom-mode))
      (an/leave-focus-mode)
    (an/enter-focus-mode)))

;; TODO: configure eshell

(use-package tree-sitter
  :config
  (global-tree-sitter-mode))

(add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)

(use-package tree-sitter-langs
  :after tree-sitter)

(use-package lsp-mode
  :straight t
  :commands lsp
  :hook ((typescript-mode js2-mode web-mode) . lsp)
  :custom (lsp-headerline-breadcrumb-enable nil))

(use-package lsp-ui
  :straight t
  :hook (lsp-mode . lsp-ui-mode)
  :bind
  (:map lsp-ui-mode-map
        ("K" . lsp-ui-doc-glance))
  :config
  (setq lsp-ui-sideline-enable t)
  (setq lsp-ui-sideline-show-hover nil)
  (setq lsp-ui-doc-position 'at-point)
  (setq lsp-ui-doc-show-with-mouse nil)
  (setq lsp-ui-doc-show-with-cursor nil)
  (setq lsp-eslint-auto-fix-on-save t)
  (lsp-ui-doc-enable t)
  (lsp-ui-peek-enable t)
  (define-key lsp-ui-mode-map [remap evil-lookup] #'lsp-ui-doc-glance)
  (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)
  (define-key lsp-ui-doc-mode-map (kbd "C-k") #'lsp-ui-doc-focus-frame)
  (define-key lsp-ui-doc-frame-mode-map (kbd "C-g") #'lsp-ui-doc-hide)
  (define-key lsp-ui-doc-frame-mode-map (kbd "ESC") #'lsp-ui-doc-hide)
  (define-key lsp-ui-doc-frame-mode-map (kbd "q") #'lsp-ui-doc-hide)
  (define-key lsp-ui-peek-mode-map (kbd "C-k") #'lsp-ui-peek--select-prev)
  (define-key lsp-ui-peek-mode-map (kbd "C-j") #'lsp-ui-peek--select-next))

(use-package typescript-mode
  :mode "\\.ts\\'"
  :config
  (setq typescript-indent-level 2))

(defun dw/set-js-indentation ()
  (setq js-indent-level 2)
  (setq evil-shift-width js-indent-level)
  (setq-default tab-width 2))

(use-package js2-mode
  :mode "\\.jsx?\\'"
  :config
  ;; Use js2-mode for Node scripts
  (add-to-list 'magic-mode-alist '("#!/usr/bin/env node" . js2-mode))

  ;; Don't use built-in syntax checking
  (setq js2-mode-show-strict-warnings nil)

  ;; Set up proper indentation in JavaScript and JSON files
  (add-hook 'js2-mode-hook #'dw/set-js-indentation)
  (add-hook 'json-mode-hook #'dw/set-js-indentation))

(use-package apheleia
  :config
  (apheleia-global-mode +1))

(use-package prettier-js
  ;; :hook ((js2-mode . prettier-js-mode)
  ;;        (typescript-mode . prettier-js-mode))
  :config
  (setq prettier-js-show-errors nil))

(use-package ccls
  :hook ((c-mode c++-mode objc-mode cuda-mode) .
         (lambda () (require 'ccls) (lsp))))

(use-package markdown-mode
  :straight t
  :mode "\\.md\\'"
  :config
  (setq markdown-command "marked")
  (defun dw/set-markdown-header-font-sizes ()
    (dolist (face '((markdown-header-face-1 . 1.2)
                    (markdown-header-face-2 . 1.1)
                    (markdown-header-face-3 . 1.0)
                    (markdown-header-face-4 . 1.0)
                    (markdown-header-face-5 . 1.0)))
      (set-face-attribute (car face) nil :weight 'normal :height (cdr face))))

  (defun dw/markdown-mode-hook ()
    (dw/set-markdown-header-font-sizes))

  (add-hook 'markdown-mode-hook 'dw/markdown-mode-hook))

(use-package web-mode
  :mode "(\\.\\(html?\\|ejs\\|tsx\\|jsx\\)\\'"
  :config
  (setq-default web-mode-code-indent-offset 2)
  (setq-default web-mode-markup-indent-offset 2)
  (setq-default web-mode-attribute-indent-offset 2))

;; 1. Start the server with `httpd-start'
;; 2. Use `impatient-mode' on any buffer
(use-package impatient-mode
  :straight t)

(use-package skewer-mode
  :straight t)

(use-package yaml-mode
  :mode "\\.ya?ml\\'")

(use-package flycheck
  :defer t
  :hook (lsp-mode . flycheck-mode))

(use-package vterm
  :after evil-collection
  :commands vterm
  :config
  (setq vterm-max-scrollback 10000)
  (advice-add 'evil-collection-vterm-insert :before #'vterm-reset-cursor-point))

(use-package mpv ;; TODO: check out
  :straight t)

;; Make gc pauses faster by decreasing the threshold.
(setq gc-cons-threshold (* 2 1000 1000))
