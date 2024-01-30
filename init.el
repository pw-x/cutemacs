;;; -*- lexical-binding: t; -*-

;; Bootstrap the Elpaca package manager.

(defvar elpaca-installer-version 0.6)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
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
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

;; Install use-package support.
(elpaca elpaca-use-package
  (elpaca-use-package-mode)
  (setq elpaca-use-package-by-default t))

;; Block until current queue is processed.
(elpaca-wait)

;; Restore normal garbage collector settings.
(defun cleanup-gc ()
  "Clean up gc."
  (setq gc-cons-threshold  67108864) ; 64M or whatever value you like
  (garbage-collect))
(run-with-idle-timer 4 nil #'cleanup-gc)

;; Use Org mode for writing the rest of the config.
(require 'org)
(org-babel-load-file (expand-file-name "config.org" user-emacs-directory))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("f5661fd54b1e60a4ae373850447efc4158c23b1c7c9d65aa1295a606278da0f8" default))
 '(warning-suppress-types '((comp))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(fixed-pitch ((t (:family "SF Mono" :height 130))))
 '(org-block ((t (:height 115 :inherit fixed-pitch))))
 '(org-document-title ((t (:inherit default "-APPL-New York-extrabold-italic-normal-*-*-*-*-*-*-0-iso10646-1" "-APPL-New York-semibold-normal-normal-*-*-*-*-*-*-0-iso10646-1" "-APPL-New York-semibold-italic-normal-*-*-*-*-*-*-0-iso10646-1" "-APPL-New York-regular-italic-normal-*-*-*-*-*-*-0-iso10646-1" "-APPL-New York-regular-normal-normal-*-*-*-*-*-*-0-iso10646-1" "-APPL-New York-black-italic-normal-*-*-*-*-*-*-0-iso10646-1" "-APPL-New York-medium-italic-normal-*-*-*-*-*-*-0-iso10646-1" "-APPL-New York-bold-italic-normal-*-*-*-*-*-*-0-iso10646-1" "-APPL-New York-bold-normal-normal-*-*-*-*-*-*-0-iso10646-1" "-APPL-New York-medium-normal-normal-*-*-*-*-*-*-0-iso10646-1" "-APPL-New York-black-normal-normal-*-*-*-*-*-*-0-iso10646-1" "-APPL-New York-extrabold-normal-normal-*-*-*-*-*-*-0-iso10646-1" :height 1.2 :weight bold))))
 '(org-quote ((t (:inherit variable-pitch))))
 '(variable-pitch ((t (:family "New York" :height 1.2 :weight regular)))))
