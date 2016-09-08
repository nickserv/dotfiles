;; packages
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)
(mapc #'package-install '(base16-theme flycheck magit rainbow-delimiters))

;; themes
(load-theme 'base16-default-light t)

;; features
(global-magit-file-mode)
(ido-mode t)

;; hooks
(add-hook 'after-init-hook #'global-flycheck-mode)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;; mouse
(unless window-system
  (xterm-mouse-mode t)
  (global-set-key (kbd "<mouse-4>") 'scroll-down-line)
  (global-set-key (kbd "<mouse-5>") 'scroll-up-line))
