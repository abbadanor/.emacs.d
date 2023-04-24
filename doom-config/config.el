;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq default-frame-alist '((undecorated . t)))

(setq user-full-name "Adam Nord"
      user-mail-address "adam.nord04@gmail.com")

(setq doom-font (font-spec :family "JetBrains Mono" :size 15)
      doom-variable-pitch-font (font-spec :family "Iosevka Aile" :size 13)
      doom-big-font (font-spec :family "Iosevka Aile" :size 24))

(load-theme 'doom-one t)

(setq display-line-numbers-type 'relative)

(setq org-directory "~/org/")
