;;; -*- lexical-binding: t; -*-

;; Use Org mode for writing the config.
(require 'org)
(org-babel-load-file (expand-file-name "config.org" user-emacs-directory))

;; Using native-comp with Emacs 28.
(setq native-comp-async-report-warnings-errors 'silent)
