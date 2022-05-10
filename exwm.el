;; Shift windows on a specified workspace.
(defun my-exwm/exwm-shift-by-class ()
  (interactive)
  (pcase exwm-class-name
    ("mpv"
     (exwm-workspace-move-window 5)
     (exwm-floating-toggle-floating)
     (exwm-layout-toggle-mode-line))))

;; Evaluate code upon creating new windows.
(defun my-exwm/exwm-update-class ()
  (exwm-workspace-rename-buffer exwm-class-name))

;; Start things upon initialising `exwm`.
(defun my-exwm/exwm-init-hook ()
  (fringe-mode 3)
  (display-time-mode 1)
  (setq display-time-day-and-date t)
  (exwm-workspace-switch-create 1))

;; Set up keybindings.
(exwm-input-set-key (kbd "s-r") #'exwm-reset)
(exwm-input-set-key (kbd "s-w") #'exwm-workspace-swap)
(exwm-input-set-key (kbd "s-t") #'exwm-layout-toggle-fullscreen)

;; Set up keybindings for switching workspaces.
(defun exwm/make-ws-switch (n)
  (exwm-input-set-key (kbd (format "s-%d" n)) `(lambda () (interactive) (exwm-workspace-switch-create ,n))))
(cl-loop for n to 9 do (exwm/make-ws-switch n))

;; Set up functions for quickly opening applications.
(defun exwm/run-in-background (command)
  (let ((command-parts (split-string command "[ ]+")))
    (apply #'call-process `(,(car command-parts) nil 0 nil ,@(cdr command-parts)))))

(defun exwm/firefox ()
  (interactive)
  (exwm/run-in-background "firefox")
  (exwm-workspace-switch-create 1))

(defun exwm/nautilus ()
  (interactive)
  (exwm/run-in-background "nautilus")
  (exwm-workspace-switch-create 3))

(defun exwm/libreoffice ()
  (interactive)
  (exwm/run-in-background "libreoffice")
  (exwm-workspace-switch-create 0))

(defun exwm/qutebrowser ()
  (interactive)
  (exwm/run-in-background "qutebrowser")
  (exwm-workspace-switch-create 2))

;; `C-q` will make the next key passthrough to emacs.
(define-key exwm-mode-map [?\C-q] 'exwm-input-send-next-key)

;; Highest priority global keys.
(setq
 exwm-input-global-keys
 `(([?\s-r] . exwm-reset)
   ([?\s-q] . kill-current-buffer)

   ;; Move between windows.
   ;; ([?\s-j] . windmove-left)
   ;; ([?\s-k] . windmove-right)
   ;; ([?\s-l] . windmove-up)
   ;; ([?\s-h] . windmove-down)

   ;; App-launchers.
   ([?\s-b] . exwm/firefox)
   ([?\s-z] . exwm/qutebrowser)
   ([?\s-f] . exwm/nautilus)
   ([?\s-o] . exwm/libreoffice)
   
   ;; Switch workspace.
   ([?\s-w] . exwm-workspace-switch)
   ([?\s-`] . (lambda () (interactive) (exwm-workspace-switch 0)))))

;; Set keys with passthrough priority to emacs.
(setq
 exwm-input-prefix-keys
 '(?\C-x
   ?\C-u
   ?\C-h
   ?\M-x
   ?\M-&
   ?\M-:
   ?\C-\)))

(add-hook 'exwm-update-class-hook #'my-exwm/exwm-update-class)
(add-hook 'exwm-init-hook #'my-exwm/exwm-init-hook)
(add-hook 'exwm-manage-finish-hook #'my-exwm/exwm-shift-by-class)
(add-hook 'exwm-floating-setup-hook (lambda () (exwm-layout-hide-mode-line)))

(exwm-enable)
