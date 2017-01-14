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
 '(custom-safe-themes t)
 '(default-frame-alist (quote ((fullscreen . fullboth))))
 '(ediff-diff-options "-w")
 '(ediff-split-window-function (quote split-window-horizontally))
 '(ediff-window-setup-function (quote ediff-setup-windows-plain))
 '(electric-layout-mode t)
 '(electric-pair-mode t)
 '(fill-column 80)
 '(global-auto-revert-non-file-buffers t)
 '(global-company-mode t)
 '(global-diff-hl-mode t)
 '(global-flycheck-mode t)
 '(global-linum-mode nil)
 '(global-magit-file-mode t)
 '(global-undo-tree-mode t)
 '(global-whitespace-mode t)
 '(helm-completion-in-region-fuzzy-match t)
 '(helm-descbinds-mode t)
 '(helm-flx-mode t)
 '(helm-mode t)
 '(helm-mode-fuzzy-match t)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(initial-scratch-message nil)
 '(ispell-program-name "/usr/local/bin/aspell")
 '(js-indent-level 2)
 '(js2-mode-show-parse-errors nil)
 '(js2-mode-show-strict-warnings nil)
 '(line-number-mode nil)
 '(magit-diff-arguments (quote ("--no-ext-diff" "-w" "--stat" "-C")))
 '(magit-diff-refine-hunk (quote all))
 '(magit-diff-section-arguments
   (quote
    ("--ignore-space-change" "--ignore-all-space" "--no-ext-diff" "-M" "-C")))
 '(magit-post-refresh-hook (quote (diff-hl-magit-post-refresh)))
 '(magit-repository-directories (quote (("~/Repos" . 1))))
 '(magit-save-repository-buffers (quote dontask))
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
     ("gnu" . "https://elpa.gnu.org/packages/"))))
 '(package-selected-packages
   (quote
    (company helm-describe-modes helm-descbinds "undo-tree" gist restart-emacs magithub diminish browse-at-remote evil leuven-theme helm-flx super-save transpose-frame exec-path-from-shell js2-mode helm-projectile helm diff-hl yaml-mode scss-mode rainbow-delimiters projectile markdown-mode magit flycheck emmet-mode)))
 '(prog-mode-hook
   (quote
    (rainbow-delimiters-mode flyspell-prog-mode linum-mode)))
 '(projectile-global-mode t)
 '(projectile-mode t nil (projectile))
 '(projectile-use-git-grep t)
 '(savehist-mode t)
 '(scroll-bar-mode nil)
 '(sh-basic-offset 2)
 '(show-paren-mode t)
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
 '(whitespace-style (quote (face trailing tabs lines-tail empty tab-mark)))
 '(xterm-mouse-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Source Code Pro")))))
