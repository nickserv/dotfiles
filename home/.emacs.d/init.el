;;; Customize
;; Most of my configured variables are in a custom file. Packages should be
;; initialized first because some of the customize variables depend on them.
(package-initialize)
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))
(setq custom-file (locate-user-emacs-file "custom.el"))
(load custom-file)

;;; Packages
;; Load package index and then install all selected packages.
(package-refresh-contents)
(package-install-selected-packages)

;;;; Magithub
;; Force the package to load after Magit to prevent failure.
(with-eval-after-load "magit" (require 'magithub))

;;; Modes

;;;; Auto Mode
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'interpreter-mode-alist '("node" . js2-mode))

;;;; Flycheck
(flycheck-pos-tip-mode)

;;;; Hooks

;; Set mode hook variables that don't support customize.
(add-hook 'css-mode-hook 'emmet-mode)
(add-hook 'sgml-mode-hook 'emmet-mode)

;; Ask to save customizations on quit
(add-hook 'kill-emacs-query-functions 'custom-prompt-customize-unsaved-options)

;; Use ANSI colors for compilation
(defun colorize-compilation-buffer ()
  (ansi-color-apply-on-region compilation-filter-start (point)))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

;; Clean whitespace on save (obeys whitespace style)
(add-hook 'before-save-hook 'whitespace-cleanup)

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

;;;; Browse at remote
(global-set-key (kbd "C-c g g") 'browse-at-remote)

;;;; Helm
;; Replace bindings for some existing features to use Helm equivalents.
(helm-projectile-on)
(global-set-key [remap find-file] 'helm-find-files)
(global-set-key [remap occur] 'helm-occur)
(global-set-key [remap list-buffers] 'helm-buffers-list)
(global-set-key [remap dabbrev-expand] 'helm-dabbrev)
(global-set-key [remap execute-extended-command] 'helm-M-x)
(global-set-key [remap describe-mode] 'helm-describe-modes)
(global-set-key [remap isearch-forward] 'swiper-helm)
(unless (boundp 'completion-in-region-function)
  (define-key lisp-interaction-mode-map [remap completion-at-point] 'helm-lisp-completion-at-point)
  (define-key emacs-lisp-mode-map       [remap completion-at-point] 'helm-lisp-completion-at-point))

;;;; Magit
;; Set binds everywhere so it can be launched from dired and other non-file
;; buffers.
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x M-g") 'magit-dispatch-popup)

;;;; Org
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c b") 'org-iswitchb)

;;;; Sane Term

;; Enable sane term
(global-set-key (kbd "C-x t") 'sane-term)
(global-set-key (kbd "C-x T") 'sane-term-create)

;; Optional convenience binding. This allows C-y to paste even when in term-char-mode (see below).
(add-hook 'term-mode-hook (lambda() (define-key term-raw-map (kbd "C-y") (lambda () (interactive) (term-line-mode) (yank) (term-char-mode)))))
