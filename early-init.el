;;; -*- lexical-binding: t; -*-

;; Disable package.el so we can use Elpaca.
(setq package-enable-at-startup nil)
;;;  Speed up emacs with a garbage collector trick.
(setq gc-cons-threshold most-positive-fixnum)
