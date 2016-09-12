;; packages
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))
(mapc 'package-install '(emmet-mode flycheck git-gutter magit markdown-mode projectile rainbow-delimiters scss-mode yaml-mode))

;; features
(global-magit-file-mode)
(ido-mode t)
(setq vc-handled-backends (delq 'Git vc-handled-backends))
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
(projectile-global-mode)
(projectile-discover-projects-in-directory "~/Repos")
(global-linum-mode)
(setq linum-format "%d ")
(global-git-gutter-mode)

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

;; mouse
(unless window-system
  (xterm-mouse-mode t)
  (global-set-key (kbd "<mouse-4>") 'scroll-down-line)
  (global-set-key (kbd "<mouse-5>") 'scroll-up-line))
