;;; Customize
;; Most of my configured variables are in a custom file. Packages should be
;; initialized first because some of the customize variables depend on them.
(package-initialize)
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))
(setq custom-file (locate-user-emacs-file "custom.el"))
(load custom-file)

;;; Packages
;; Load package index if it does not exist (new installation) and then install
;; all selected packages.
(unless package-archive-contents
  (package-refresh-contents))
(package-install-selected-packages)

;;; Modes

;;;; Auto Mode
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsx?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.s?css\\'" . web-mode))

;;;; Hooks
;; Set mode hook variables that don't support customize.
(add-hook 'css-mode-hook 'emmet-mode)
(add-hook 'sgml-mode-hook 'emmet-mode)
(add-hook 'web-mode-hook 'emmet-mode)

;;; Convenience
(projectile-discover-projects-in-directory "~/Repos")
(defalias 'yes-or-no-p 'y-or-n-p)

;;; CLI
;; Improve mouse support and margin display in terminals.
(unless window-system
  (global-set-key (kbd "<mouse-4>") 'scroll-down-line)
  (global-set-key (kbd "<mouse-5>") 'scroll-up-line)
  (setq linum-format "%d "))

;;; Keys
;; Set magit binds everywhere so it can be launched from dired and other non-
;; file buffers.
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x M-g") 'magit-dispatch-popup)

;;;; Helm
;; Replace bindings for some existing features to use Helm equivalents.
(helm-projectile-on)
(global-set-key [remap find-file] 'helm-find-files)
(global-set-key [remap occur] 'helm-occur)
(global-set-key [remap list-buffers] 'helm-buffers-list)
(global-set-key [remap dabbrev-expand] 'helm-dabbrev)
(global-set-key [remap execute-extended-command] 'helm-M-x)
(unless (boundp 'completion-in-region-function)
  (define-key lisp-interaction-mode-map [remap completion-at-point] 'helm-lisp-completion-at-point)
  (define-key emacs-lisp-mode-map       [remap completion-at-point] 'helm-lisp-completion-at-point))
