;;; Customize
;; Most of my configured variables are in a custom file. Packages should be
;; initialized first because some of the customize variables depend on them.
(package-initialize)
(load (setq custom-file (locate-user-emacs-file "custom.el")))

;;; Packages
;; Load package index and then install all selected packages.
(package-refresh-contents)
(package-install-selected-packages)

;;; Set paths on macOS
(when (eq window-system 'ns)
  (exec-path-from-shell-initialize))

;;;; Magithub
;; Force the package to load after Magit to prevent failure.
(with-eval-after-load
    "magit"
  (require 'magithub)
  (magithub-feature-autoinject t))

;;; Ivy
;; Fuzzy matching
(setq ivy-re-builders-alist '((t . ivy--regex-fuzzy)))
(setq ivy-initial-inputs-alist nil)

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

;; prog-mode
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)
(add-hook 'prog-mode-hook 'linum-mode)

;;; Convenience
(projectile-mode)
(projectile-discover-projects-in-directory "~/Repos")
(defalias 'yes-or-no-p 'y-or-n-p)

;;; CLI
;; Improve mouse support and margin display in terminals.
(unless window-system
  (global-set-key "[mouse-4]" 'scroll-down-line)
  (global-set-key "[mouse-5]" 'scroll-up-line)
  (setq linum-format "%d "))

;;; Keys

;;;; Restart
(global-set-key (kbd "C-x C-S-c") 'restart-emacs)

;;;; Browse at remote
(global-set-key "\C-cr" 'browse-at-remote)

;;;; Swiper
(global-set-key "\C-s" 'swiper)

;; Counsel interfaces
(global-set-key "\C-cg" 'counsel-git)
(global-set-key "\C-cj" 'counsel-git-grep)

;; Ivy
(global-set-key "\C-c\C-r" 'ivy-resume)

;;;; Magit
;; Set binds everywhere so it can be launched from dired and other non-file
;; buffers.
(global-set-key "\C-xg" 'magit-status)
(global-set-key "\C-x\M-g" 'magit-dispatch-popup)

;;;; Org
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-iswitchb)

;;;; Sane Term

;; Enable sane term
(global-set-key "\C-xt" 'sane-term)
(global-set-key "\C-xT" 'sane-term-create)

;; Optional convenience binding. This allows C-y to paste even when in term-char-mode (see below).
(add-hook 'term-mode-hook (lambda() (define-key term-raw-map "\C-y" (lambda () (interactive) (term-line-mode) (yank) (term-char-mode)))))
