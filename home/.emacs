;; packages
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)
(mapc #'package-install '(base16-theme flycheck magit rainbow-delimiters))

;; themes
(load-theme 'base16-default-light t)

;; features
(add-hook 'after-init-hook #'global-flycheck-mode)
(global-magit-file-mode)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
(ido-mode t)

;; mouse
(unless window-system
  (xterm-mouse-mode t)
  (global-set-key (kbd "<mouse-4>") 'scroll-down-line)
  (global-set-key (kbd "<mouse-5>") 'scroll-up-line))
