(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blink-cursor-mode nil)
 '(browse-url-browser-function (quote browse-url-default-browser))
 '(column-number-mode t)
 '(default-frame-alist (quote ((fullscreen . fullheight))))
 '(global-diff-hl-mode t)
 '(global-flycheck-mode t)
 '(global-linum-mode t)
 '(global-magit-file-mode t)
 '(helm-completion-in-region-fuzzy-match t)
 '(helm-mode t)
 '(helm-mode-fuzzy-match t)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(magit-post-refresh-hook (quote (diff-hl-magit-post-refresh)))
 '(menu-bar-mode nil)
 '(package-archives
   (quote
    (("melpa" . "https://melpa.org/packages/")
     ("gnu" . "http://elpa.gnu.org/packages/"))))
 '(package-selected-packages
   (quote
    (js2-mode web-mode helm-projectile helm diff-hl yaml-mode scss-mode rainbow-delimiters projectile markdown-mode magit flycheck emmet-mode)))
 '(prog-mode-hook
   (quote
    (rainbow-delimiters-mode flyspell-prog-mode electric-pair-mode)))
 '(projectile-global-mode t)
 '(projectile-mode t nil (projectile))
 '(scroll-bar-mode nil)
 '(show-paren-mode t)
 '(tab-always-indent (quote complete))
 '(tab-width 2)
 '(text-mode-hook (quote (flyspell-mode)))
 '(tool-bar-mode nil)
 '(web-mode-code-indent-offset 2)
 '(web-mode-css-indent-offset 2)
 '(web-mode-markup-indent-offset 2)
 '(web-mode-script-padding 2)
 '(web-mode-style-padding 2)
 '(xterm-mouse-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:height 140 :family "Fira Code")))))
