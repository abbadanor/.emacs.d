(setq
 ;; Do not make installed packages available when Emacs starts
 package-enable-at-startup nil
 ;; HACK: Increase the garbage collection (GC) threshold for faster startup.
 ;; This will be overwritten when `gcmh-mode' (a.k.a. the Garbage Collector
 ;; Magic Hack) gets loaded in the `me-gc' module (see "init.el").
 gc-cons-threshold (* 50 1000 1000)
 ;; Do not wast time checking the modification time of each file
 load-prefer-newer noninteractive)
 ;; Remove some unneeded UI elements
(setq default-frame-alist '((undecorated . t))) ; Disable window decorations
(scroll-bar-mode -1)                            ; Disable visible scrollbar
(tool-bar-mode -1)                              ; Disable the toolbar
(tooltip-mode -1)                               ; Disable tooltips
(menu-bar-mode -1)                              ; Disable the menu bar
