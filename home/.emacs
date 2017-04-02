;;; .emacs --- Nick's Emacs configuration

;;; Commentary:
;; My GNU Emacs 25.1 configuration for macOS and Windows, using MELPA
;; and use-package.

;;; Code:

;;; Helpers

(defun listify (object)
  "If OBJECT is a list, return it, else wrap it in a list."
  (if (listp object) object (list object)))

(defun mapflat (function sequence)
  "Apply FUNCTION to each element of SEQUENCE, and make a list of the results.
Unlike `mapcar', the list is flattened nondestructively before it is returned."
  (apply #'append (mapcar function sequence)))

(defmacro add-hooks (&rest args)
  "Call `add-hook' on each cons pair in ARGS.
Each pair has a `car' for setting hooks and a `cdr' for setting
functions to add to those hooks.  Either side of the pair can be
a single symbol or a list of symbols, in which case a function
can be added to multiple hooks and/or multiple functions can be
added to a hook."
  (macroexp-progn
   (mapflat (lambda (arg)
              (let ((hooks (listify (car arg)))
                    (functions (listify (cdr arg))))

                (mapflat (lambda (hook)
                           (mapcar (lambda (function)
                                     `(add-hook ',hook ',function))
                                   functions))
                         hooks)))
            args)))

;;; Variables

(setq auto-save-default nil
      backup-directory-alist '((".*" . "~/.emacs.d/backup/"))
      custom-file (locate-user-emacs-file "custom.el")
      default-frame-alist '((fullscreen . maximized))
      fill-column 80
      inhibit-startup-screen t
      initial-buffer-choice "~/Google Drive/Organizer.org"
      initial-scratch-message nil
      mouse-wheel-scroll-amount '(1 ((control)))
      package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("gnu" . "https://elpa.gnu.org/packages/"))
      tab-always-indent 'complete
      tab-width 2
      visible-bell t)

(setq-default indent-tabs-mode nil
              tab-width 2)

(ansi-color-for-comint-mode-on)
(set-frame-font "Source Code Pro" nil t)
(defalias 'yes-or-no-p 'y-or-n-p)

;; Use full screen on macOS only.
(when (eq window-system 'ns)
  (setq default-frame-alist '((fullscreen . fullboth))))

;;; Minor Modes
(blink-cursor-mode 0)
(electric-layout-mode)
(electric-pair-mode)
(line-number-mode 0)
(menu-bar-mode 0)
(save-place-mode)
(savehist-mode)
(show-paren-mode)
(xterm-mouse-mode)

;;; Package Setup
(package-initialize)
(package-refresh-contents)

;; Install use-package
(package-install 'use-package)

;;; Binds
(bind-keys ("<mouse-4>" . scroll-down-line)
           ("<mouse-5>" . scroll-up-line))

;;; Packages

(use-package autorevert
  :config
  (setq global-auto-revert-non-file-buffers t))

(use-package browse-at-remote
  :ensure
  :bind ("C-c r" . browse-at-remote))

(use-package compile
  :init
  (setq compilation-ask-about-save nil)
  (defun colorize-compilation-buffer ()
    "Use ANSI colors for compilation."
    (defvar compilation-filter-start)
    (ansi-color-apply-on-region compilation-filter-start (point)))
  (add-hook 'compilation-filter-hook 'colorize-compilation-buffer))

(use-package counsel
  :ensure
  :init
  (counsel-mode)
  (ivy-mode)
  :bind (("C-c g" . counsel-git)
         ("C-c j" . counsel-git-grep)
         ("C-c C-r" . ivy-resume)
         ("C-s" . swiper))
  :config
  (setq ivy-count-format "(%d/%d) "
        ivy-display-style 'fancy
        ivy-use-virtual-buffers t))

(use-package css-mode
  :config
  (setq css-indent-offset 2))

(use-package ediff
  :config
  (setq ediff-diff-options "-w"
        ediff-split-window-function 'split-window-horizontally
        ediff-window-setup-function 'ediff-setup-windows-plain))

(use-package emmet-mode
  :ensure
  :init
  (add-hooks ((css-mode-hook sgml-mode-hook) . emmet-mode)))

(use-package exec-path-from-shell
  :ensure
  :if (eq window-system 'ns)
  :init
  (exec-path-from-shell-initialize))

(use-package flx
  :ensure)

(use-package flycheck
  :ensure
  :init
  (global-flycheck-mode))

(use-package flycheck-pos-tip
  :ensure
  :after flycheck
  :init
  (flycheck-pos-tip-mode))

(use-package flyspell
  :init
  (add-hooks (text-mode-hook . flyspell-mode)
             (prog-mode-hook . flyspell-prog-mode)))

(use-package gist
  :ensure)

(use-package hippie-expand
  :bind ("M-/" . hippie-expand))

(use-package ibuffer
  :bind ("C-x C-b" . ibuffer))

(use-package ispell
  :config
  (setq ispell-program-name "/usr/local/bin/aspell"))

(use-package js
  :config
  (setq js-indent-level 2))

(use-package leuven-theme
  :ensure
  :init
  (load-theme 'leuven t))

(use-package linum
  :init
  (unless window-system
    (setq linum-format "%d "))
  (add-hook 'prog-mode-hook 'linum-mode))

(use-package magit
  :ensure
  :init
  (global-magit-file-mode)
  :bind (("C-x g" . magit-status)
         ("C-x M-g" . magit-dispatch-popup))
  :config
  (setq magit-completing-read-function 'ivy-completing-read
        magit-diff-arguments '("--no-ext-diff" "-w" "-C")
        magit-diff-refine-hunk 'all
        magit-diff-section-arguments '("--ignore-space-change"
                                       "--ignore-all-space"
                                       "--no-ext-diff"
                                       "-M"
                                       "-C")
        magit-repository-directories '(("~/Repos" . 1))
        magit-save-repository-buffers 'dontask))

(use-package magithub
  :ensure
  :after magit
  :config
  (magithub-feature-autoinject t))

(use-package markdown-mode
  :ensure)

(use-package org
  :bind (("C-c a" . org-agenda)
         ("C-c b" . org-iswitchb)
         ("C-c c" . org-capture)
         ("C-c l" . org-store-link)
         ("C-c o" . find-org-default-notes-file))
  :config
  (setq org-agenda-files '("~/Google Drive/Organizer.org")
        org-default-notes-file "~/Google Drive/Organizer.org"
        org-enforce-todo-checkbox-dependencies t
        org-enforce-todo-dependencies t
        org-fontify-whole-heading-line t
        org-log-done 'time
        org-log-repeat 'time
        org-modules '(org-mouse)
        org-outline-path-complete-in-steps nil
        org-refile-targets '((nil :maxlevel . 10))
        org-refile-use-outline-path 'file)
  (defun find-org-default-notes-file ()
    "Open the default Org notes file."
    (interactive)
    (find-file org-default-notes-file)))

(use-package org-capture
  :config
  (setq org-capture-templates '(("t"
                                 "Task"
                                 entry
                                 (file+headline "" "Tasks")
                                 "* TODO %?
  %u
  %a")
                                ("n"
                                 "Note"
                                 entry
                                 (file+headline "" "Notes")
                                 "* %?
  %i
  %a"))))

(use-package paredit
  :ensure
  :init
  (add-hooks ((emacs-lisp-mode-hook
               eval-expression-minibuffer-setup-hook
               ielm-mode-hook
               lisp-mode-hook
               lisp-interaction-mode-hook
               scheme-mode-hook)
              . enable-paredit-mode)))

(use-package projectile
  :ensure
  :init
  (projectile-mode)
  (projectile-discover-projects-in-directory "~/Repos")
  (projectile-register-project-type 'npm
                                    '("package.json")
                                    "npm install"
                                    "npm test"
                                    "npm start")
  (projectile-register-project-type 'jekyll
                                    '("_config.yml")
                                    "bundle exec jekyll build"
                                    nil
                                    "bundle exec jekyll serve")
  :config
  (setq projectile-completion-system 'ivy))

(use-package rainbow-mode
  :ensure
  :init
  (add-hook 'prog-mode-hook 'rainbow-mode))

(use-package restart-emacs
  :ensure
  :bind ("C-x C-S-c" . restart-emacs))

(use-package sane-term
  :ensure
  :bind (("C-x t" . sane-term)
         ("C-x T" . sane-term-create)))

(use-package scroll-bar
  :config
  (set-scroll-bar-mode nil))

(use-package sh-script
  :config
  (setq sh-basic-offset 2))

(use-package simple
  :init
  (add-hook 'text-mode-hook 'auto-fill-mode))

(use-package smex
  :ensure)

(use-package super-save
  :ensure
  :init
  (super-save-mode)
  :config
  (setq super-save-auto-save-when-idle t))

(use-package term
  :bind (:map term-raw-map ("C-c C-y" . term-paste)))

(use-package tern
  :ensure
  :init
  (add-hook 'js-mode-hook 'tern-mode)
  :config
  (setq tern-command '("tern" "--no-port-file")))

(use-package tool-bar
  :init
  (tool-bar-mode 0))

(use-package undo-tree
  :ensure
  :init
  (global-undo-tree-mode)
  :config
  (setq undo-tree-visualizer-diff t
        undo-tree-visualizer-timestamps t))

(use-package vc
  :init
  (setq vc-follow-symlinks t)
  :config
  (setq vc-diff-switches "-w"))

(use-package vc-git
  :config
  (setq vc-git-diff-switches "-w -C"))

(use-package whitespace
  :init
  (global-whitespace-mode)
  (add-hook 'before-save-hook 'whitespace-cleanup)
  :config
  (setq whitespace-style '(face trailing tabs lines-tail empty tab-mark)))

(use-package yaml-mode
  :ensure
  :mode "\\.yml\\'")

;;; .emacs ends here
