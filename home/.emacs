;;; .emacs --- Nick's Emacs configuration

;;; Commentary:

;; My GNU Emacs 25.1 configuration for macOS and Windows, using MELPA
;; and use-package.

;;; Code:

;;; Variables

(defconst nick-indent-level 2)
(defconst nick-mac-window-system (memq window-system '(mac ns)))
(defconst nick-organizer "~/Google Drive/Organizer.org")
(defconst nick-projects-directory "~/Repos")

(setq auto-save-default nil
      backup-directory-alist `((".*" . ,(locate-user-emacs-file "backup/")))
      custom-file (locate-user-emacs-file "custom.el")
      default-frame-alist '((fullscreen . maximized))
      fill-column 80
      inhibit-startup-screen t
      initial-buffer-choice nick-organizer
      initial-scratch-message nil
      mouse-wheel-scroll-amount '(1 ((control)))
      package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("gnu" . "https://elpa.gnu.org/packages/"))
      tab-always-indent 'complete
      visible-bell t)

(setq-default indent-tabs-mode nil
              tab-width nick-indent-level)

(ansi-color-for-comint-mode-on)
(set-frame-font "Source Code Pro" nil t)

;; Abbreviate yes/no prompts.
(defalias 'yes-or-no-p 'y-or-n-p)

;;; Minor Modes
(blink-cursor-mode 0)
(line-number-mode 0)
(menu-bar-mode 0)
(save-place-mode)
(savehist-mode)
(xterm-mouse-mode)

;; macOS GUI
(when nick-mac-window-system
  (setq default-frame-alist '((fullscreen . fullboth)))
  (menu-bar-mode))

;;; Package Setup
(package-initialize)
(package-refresh-contents)

;; Install use-package
(package-install 'use-package)

;;; Bind Improvements
(bind-keys ("<mouse-4>" . scroll-down-line)
           ("<mouse-5>" . scroll-up-line)
           ("C-c s" . sort-lines)
           ("C-s" . swiper)
           ("C-x C-b" . ibuffer)
           ("M-/" . hippie-expand))

;;; Packages

(use-package add-hooks
  :ensure)

(use-package aggressive-indent
  :ensure
  :config
  (global-aggressive-indent-mode))

(use-package autorevert
  :config
  (setq auto-revert-verbose nil
        global-auto-revert-non-file-buffers t)
  (global-auto-revert-mode))

(use-package browse-at-remote
  :ensure
  :bind ("C-c r" . browse-at-remote))

(use-package compile
  :config
  (setq compilation-ask-about-save nil)
  (defun colorize-compilation-buffer ()
    "Use ANSI colors for compilation."
    (defvar compilation-filter-start)
    (ansi-color-apply-on-region compilation-filter-start (point)))
  (add-hooks-pair 'compilation-filter 'colorize-compilation-buffer))

(use-package counsel
  :ensure
  :bind (("C-c g" . counsel-git)
         ("C-c j" . counsel-git-grep))
  :init
  (counsel-mode))

(use-package css-mode
  :config
  (setq css-indent-offset nick-indent-level))

(use-package ediff
  :config
  (setq ediff-diff-options "-w"
        ediff-split-window-function 'split-window-horizontally
        ediff-window-setup-function 'ediff-setup-windows-plain))

(use-package emmet-mode
  :ensure
  :config
  (add-hooks-pair '(css-mode scss-mode sgml-mode) 'emmet-mode))

(use-package ert
  :bind (("C-c e" . ert-run-tests-from-buffer))
  :config
  (defun ert-run-tests-from-buffer ()
    "Eval the current buffer and run all ERT tests."
    (interactive)
    (eval-buffer)
    (ert t)))

(use-package exec-path-from-shell
  :ensure
  :if nick-mac-window-system
  :config
  (exec-path-from-shell-initialize))

(use-package flx
  :ensure)

(use-package flycheck
  :ensure
  :config
  (global-flycheck-mode))

(use-package flycheck-package
  :ensure
  :after flycheck
  :config
  (flycheck-package-setup))

(use-package flycheck-pos-tip
  :ensure
  :after flycheck
  :config
  (flycheck-pos-tip-mode))

(use-package flyspell
  :config
  (add-hooks '((text-mode . flyspell-mode)
               (prog-mode . flyspell-prog-mode))))

(use-package gist
  :ensure)

(use-package ispell
  :config
  (setq ispell-program-name "/usr/local/bin/aspell"))

(use-package ivy
  :bind ("C-c C-r" . ivy-resume)
  :config
  (setq ivy-count-format "(%d/%d) "
        ivy-display-style 'fancy
        ivy-use-virtual-buffers t)
  (ivy-mode))

(use-package ivy-hydra
  :ensure
  :after ivy)

(use-package js
  :mode ("\\.jsx?\\'" . js-jsx-mode)
  :config
  (setq js-indent-level nick-indent-level))

(use-package js-comint
  :ensure)

(use-package leuven-theme
  :ensure
  :config
  (load-theme 'leuven t))

(use-package linum
  :config
  (unless window-system
    (setq linum-format "%d "))
  (add-hooks-pair 'prog-mode 'linum-mode))

(use-package magit
  :ensure
  ;; Set binds everywhere so it can be launched from non-file buffers.
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
        magit-repository-directories `((,nick-projects-directory . 1))
        magit-save-repository-buffers 'dontask)
  (global-magit-file-mode))

(use-package markdown-mode
  :ensure)

(use-package npm-mode
  :ensure
  :config
  (npm-global-mode))

(use-package org
  :bind (("C-c a" . org-agenda)
         ("C-c b" . org-iswitchb)
         ("C-c c" . org-capture)
         ("C-c l" . org-store-link)
         ("C-c o" . find-org-default-notes-file))
  :init
  (setq org-default-notes-file initial-buffer-choice
        org-agenda-files (list org-default-notes-file))
  :config
  (setq org-enforce-todo-checkbox-dependencies t
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

(use-package projectile
  :ensure
  :config
  (setq projectile-completion-system 'ivy)
  (projectile-mode)
  (projectile-discover-projects-in-directory nick-projects-directory)
  (projectile-register-project-type 'jekyll '("_config.yml")
                                    :compile "bundle exec jekyll serve"))

(use-package rainbow-mode
  :ensure
  :config
  (add-hooks-pair 'prog-mode 'rainbow-mode))

(use-package restart-emacs
  :ensure
  :bind ("C-x C-S-c" . restart-emacs))

(use-package restclient
  :ensure
  :bind ("C-c h" . restclient-buffer)
  :config
  (defun restclient-buffer ()
    (interactive)
    (let ((buffer "*HTTP Request*"))
      (get-buffer-create buffer)
      (switch-to-buffer-other-window buffer)
      (restclient-mode))))

(use-package sane-term
  :ensure
  :bind (("C-x t" . sane-term)
         ("C-x T" . sane-term-create)))

(use-package scroll-bar
  :config
  (set-scroll-bar-mode nil))

(use-package sh-script
  :config
  (setq sh-basic-offset nick-indent-level))

(use-package simple
  :config
  (add-hooks-pair 'text-mode 'auto-fill-mode))

(use-package smartparens
  :ensure
  :config
  (require 'smartparens-config)
  (smartparens-global-strict-mode)
  (sp-use-smartparens-bindings)
  (show-smartparens-global-mode))

(use-package smex
  :ensure)

(use-package super-save
  :ensure
  :config
  (setq super-save-auto-save-when-idle t)
  (super-save-mode))

(use-package term
  :bind (:map term-raw-map ("C-c C-y" . term-paste)))

(use-package tern
  :ensure
  :config
  (setq tern-command '("tern" "--no-port-file"))
  (add-hooks-pair 'js-mode 'tern-mode))

(use-package tool-bar
  :config
  (tool-bar-mode 0))

(use-package typescript-mode
  :ensure
  :config
  (setq typescript-indent-level nick-indent-level))

(use-package undo-tree
  :ensure
  :config
  (setq undo-tree-visualizer-diff t
        undo-tree-visualizer-timestamps t)
  (global-undo-tree-mode))

(use-package vc
  :config
  (setq vc-diff-switches "-w"
        vc-follow-symlinks t))

(use-package vc-git
  :config
  (setq vc-git-diff-switches "-w -C"))

(use-package whitespace
  :config
  (setq whitespace-style '(face trailing tabs lines-tail empty tab-mark))
  (add-hooks-pair 'before-save 'whitespace-cleanup)
  (global-whitespace-mode))

(use-package yaml-mode
  :ensure)

;;; .emacs ends here
