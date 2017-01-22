;;; init.el --- Nick's Emacs configuration

;;; Commentary:
;; My personal GNU Emacs 25.1 configuration for macOS, using MELPA and
;; customize.  Dependencies are installed automatically, but you may need to
;; restart Emacs to apply customizations for newly installed packages.

;;; Code:

;;; Initial setup

;;;; Customize
;; Initialize installed packages and enable customized variables.
(package-initialize)
(load (setq custom-file (locate-user-emacs-file "custom.el")))

;;;; Packages
;; Download package data and install packages selected in customize.
(package-refresh-contents)
(package-install-selected-packages)

;;;; Set paths on macOS
(when (eq window-system 'ns)
  (exec-path-from-shell-initialize))

;;; Auto mode
;; Use yaml-mode and replace js-mode with js2-mode.
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'interpreter-mode-alist '("node" . js2-mode))

;;; Hooks

;;;; Programming modes

(add-hook 'prog-mode-hook 'flyspell-prog-mode)
(add-hook 'prog-mode-hook 'linum-mode)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;; Use Emmet to complete CSS and HTML.
(add-hook 'css-mode-hook 'emmet-mode)
(add-hook 'sgml-mode-hook 'emmet-mode)

;;;; Usability fixes

;; Ask to save customizations on quit.
(add-hook 'kill-emacs-query-functions 'custom-prompt-customize-unsaved-options)

(defun colorize-compilation-buffer ()
  "Use ANSI colors for compilation."
  (ansi-color-apply-on-region compilation-filter-start (point)))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

(defun bind-term-paste ()
  "Bind `term-paste'."
  (bind-key "C-c C-y" 'term-paste term-raw-map))
(add-hook 'term-mode-hook 'bind-term-paste)

;;; Keys
(bind-keys
 ;; Restart
 ("C-x C-S-c" . restart-emacs)

 ;; Browse at remote
 ("C-c r" . browse-at-remote)

 ;; Swiper
 ("C-s" . swiper)

 ;; Counsel interfaces
 ("C-c g" . counsel-git)
 ("C-c j" . counsel-git-grep)

 ;; Ivy
 ("C-c C-r" . ivy-resume)

 ;; Magit
 ;; Set binds everywhere so it can be launched from dired and other non-file
 ;; buffers.
 ("C-x g" . magit-status)
 ("C-x M-g" . magit-dispatch-popup)

 ;; Org
 ("C-c l" . org-store-link)
 ("C-c a" . org-agenda)
 ("C-c c" . org-capture)
 ("C-c b" . org-iswitchb)

 ;; Sane Term
 ("C-x t" . sane-term)
 ("C-x T" . sane-term-create))

;;; CLI
;; Improve mouse support and margin display in terminals.
(unless window-system
  (bind-keys
   ("<mouse-4>" . scroll-down-line)
   ("<mouse-5>" . scroll-up-line))
  (setq linum-format "%d "))

;;; Flycheck
(flycheck-pos-tip-mode)

;;; Ivy
;; Enable fuzzy matching against any part of candidate strings.
(setq ivy-re-builders-alist '((t . ivy--regex-fuzzy)))
(setq ivy-initial-inputs-alist nil)

;;; Magithub
;; Load after Magit and enable all features.
(with-eval-after-load
    "magit"
  (require 'magithub)
  (magithub-feature-autoinject t))

;;; Projectile
(projectile-mode)
(projectile-discover-projects-in-directory "~/Repos")

;;; Abbreviate yes/no prompts.
(defalias 'yes-or-no-p 'y-or-n-p)

;;; init.el ends here
