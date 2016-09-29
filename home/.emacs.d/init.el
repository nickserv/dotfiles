;; custom
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;; packages
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))
(package-install-selected-packages)

;; features
(global-magit-file-mode)
(diff-hl-mode)
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
(projectile-global-mode)
(projectile-discover-projects-in-directory "~/Repos")
(global-linum-mode)
(setq linum-format "%d ")
(global-diff-hl-mode)
(add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
(nyan-mode)

;; indentation
(setq-default indent-tabs-mode nil
              tab-width 2)
(setq tab-always-indent 'complete)

;; binds
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x M-g") 'magit-dispatch-popup)

;; hooks
(add-hook 'after-init-hook 'global-flycheck-mode)
(add-hook 'css-mode-hook 'emmet-mode)
(add-hook 'prog-mode-hook 'auto-fill-mode)
(add-hook 'prog-mode-hook 'electric-pair-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
(add-hook 'sgml-mode-hook 'emmet-mode)
(add-hook 'text-mode-hook 'auto-fill-mode)
(add-hook 'text-mode-hook 'flyspell-mode)

;; ui
(if window-system
  (progn
    (set-frame-font "Source Code Pro 14" nil t)
    (tool-bar-mode -1))
  (menu-bar-mode -1)
  (xterm-mouse-mode t)
  (global-set-key (kbd "<mouse-4>") 'scroll-down-line)
  (global-set-key (kbd "<mouse-5>") 'scroll-up-line))

;; helm
(blink-cursor-mode -1)
(require 'helm-config)
(helm-mode 1)
(define-key global-map [remap find-file] 'helm-find-files)
(define-key global-map [remap occur] 'helm-occur)
(define-key global-map [remap list-buffers] 'helm-buffers-list)
(define-key global-map [remap dabbrev-expand] 'helm-dabbrev)
(global-set-key (kbd "M-x") 'helm-M-x)
(unless (boundp 'completion-in-region-function)
  (define-key lisp-interaction-mode-map [remap completion-at-point] 'helm-lisp-completion-at-point)
  (define-key emacs-lisp-mode-map       [remap completion-at-point] 'helm-lisp-completion-at-point))
(setq helm-mode-fuzzy-match t)
(setq helm-completion-in-region-fuzzy-match t)
