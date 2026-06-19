;;; early-init.el --- Early startup -*- lexical-binding: t; -*-

(setq gc-cons-threshold most-positive-fixnum)
(setq gc-cons-percentage 0.6)

(setq package-enable-at-startup nil)
(setq frame-inhibit-implied-resize t)
(setq inhibit-startup-screen t)
(setq inhibit-startup-message t)
(setq inhibit-startup-buffer-menu t)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(set-face-attribute 'default nil :font "Adwaita Sans" :height 120)
