;; custom
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;; packages
(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))
(package-install-selected-packages)

;; features
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
(projectile-discover-projects-in-directory "~/Repos")
(add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsx?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.s?css\\'" . web-mode))
(defalias 'yes-or-no-p 'y-or-n-p)

;; binds
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x M-g") 'magit-dispatch-popup)

;; hooks
(add-hook 'after-init-hook 'global-flycheck-mode)
(add-hook 'css-mode-hook 'emmet-mode)
(add-hook 'prog-mode-hook 'electric-pair-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
(add-hook 'sgml-mode-hook 'emmet-mode)
(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'web-mode-hook 'emmet-mode)

;; cli
(if (not window-system)
    (progn
      (global-set-key (kbd "<mouse-4>") 'scroll-down-line)
      (global-set-key (kbd "<mouse-5>") 'scroll-up-line)
      (setq linum-format "%d ")))

;; helm
(define-key global-map [remap find-file] 'helm-find-files)
(define-key global-map [remap occur] 'helm-occur)
(define-key global-map [remap list-buffers] 'helm-buffers-list)
(define-key global-map [remap dabbrev-expand] 'helm-dabbrev)
(global-set-key (kbd "M-x") 'helm-M-x)
(unless (boundp 'completion-in-region-function)
  (define-key lisp-interaction-mode-map [remap completion-at-point] 'helm-lisp-completion-at-point)
  (define-key emacs-lisp-mode-map       [remap completion-at-point] 'helm-lisp-completion-at-point))
