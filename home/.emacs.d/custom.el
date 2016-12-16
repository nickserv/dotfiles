(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-for-comint-mode t)
 '(auto-save-default nil)
 '(backup-directory-alist (quote ((".*" . "~/.emacs.d/backup/"))))
 '(blink-cursor-mode nil)
 '(browse-url-browser-function (quote browse-url-default-browser))
 '(cua-enable-cua-keys nil)
 '(cua-mode t nil (cua-base))
 '(custom-enabled-themes (quote (leuven)))
 '(custom-safe-themes
   (quote
    ("51897d0e185a9d350a124afac8d5e95cda53e737f3b33befc44ab02f2b03dab1" default)))
 '(default-frame-alist (quote ((fullscreen . fullboth))))
 '(ediff-diff-options "-w")
 '(ediff-split-window-function (quote split-window-horizontally))
 '(ediff-window-setup-function (quote ediff-setup-windows-plain))
 '(electric-pair-mode nil)
 '(fill-column 80)
 '(global-auto-revert-non-file-buffers t)
 '(global-diff-hl-mode t)
 '(global-flycheck-mode t)
 '(global-linum-mode nil)
 '(global-magit-file-mode t)
 '(global-whitespace-mode t)
 '(guru-global-mode t)
 '(helm-completion-in-region-fuzzy-match t)
 '(helm-flx-mode t)
 '(helm-mode t)
 '(helm-mode-fuzzy-match t)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(initial-scratch-message nil)
 '(ispell-program-name "/usr/local/bin/aspell")
 '(magit-diff-arguments (quote ("--no-ext-diff" "-w" "--stat" "-C")))
 '(line-number-mode nil)
 '(magit-diff-refine-hunk (quote all))
 '(magit-diff-section-arguments
   (quote
    ("--ignore-space-change" "--ignore-all-space" "--no-ext-diff" "-M" "-C")))
 '(magit-post-refresh-hook (quote (diff-hl-magit-post-refresh)))
 '(menu-bar-mode nil)
 '(mouse-wheel-scroll-amount (quote (1 ((control)))))
 '(org-agenda-files (quote ("~/Google Drive/Todo.org")))
 '(org-fontify-whole-heading-line t)
 '(org-modules
   (quote
    (org-bbdb org-bibtex org-docview org-gnus org-info org-irc org-mhe org-mouse org-rmail org-w3m)))
 '(package-archives
   (quote
    (("melpa" . "https://melpa.org/packages/")
     ("gnu" . "http://elpa.gnu.org/packages/"))))
 '(package-selected-packages
   (quote
    (diminish browse-at-remote evil leuven-theme helm-flx super-save helm-ag smartparens guru-mode transpose-frame exec-path-from-shell js2-mode web-mode helm-projectile helm diff-hl yaml-mode scss-mode rainbow-delimiters projectile markdown-mode magit flycheck emmet-mode)))
 '(prog-mode-hook
   (quote
    (rainbow-delimiters-mode flyspell-prog-mode electric-pair-mode linum-mode)))
 '(projectile-global-mode t)
 '(projectile-mode t nil (projectile))
 '(scroll-bar-mode nil)
 '(sh-basic-offset 2)
 '(show-paren-mode t)
 '(smartparens-global-mode t)
 '(super-save-auto-save-when-idle t)
 '(super-save-mode t)
 '(tab-always-indent (quote complete))
 '(tab-width 2)
 '(text-mode-hook (quote (flyspell-mode auto-fill-mode)))
 '(tool-bar-mode nil)
 '(vc-diff-switches "-w")
 '(vc-follow-symlinks t)
 '(vc-git-diff-switches "-w -C")
 '(visible-bell t)
 '(web-mode-code-indent-offset 2)
 '(web-mode-css-indent-offset 2)
 '(web-mode-markup-indent-offset 2)
 '(web-mode-script-padding 2)
 '(web-mode-style-padding 2)
 '(whitespace-style (quote (face trailing tabs lines-tail empty tab-mark)))
 '(xterm-mouse-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Source Code Pro")))))
