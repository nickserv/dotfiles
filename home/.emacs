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
      inhibit-startup-screen t
      initial-buffer-choice nick-organizer
      initial-scratch-message nil
      mouse-wheel-scroll-amount '(1 ((control)))
      package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "http://orgmode.org/elpa/")
                         ("gnu" . "https://elpa.gnu.org/packages/"))
      tab-always-indent 'complete
      visible-bell t)

(setq-default fill-column 80
              indent-tabs-mode nil
              line-spacing 0.4
              tab-width nick-indent-level)

(set-frame-font "SF Pro Text" nil t)

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

(use-package delight
  :ensure)

(use-package add-node-modules-path
  :ensure
  :config
  (add-hooks-pair 'web-mode 'add-node-modules-path))

(use-package aggressive-indent
  :ensure
  :delight
  :config
  (global-aggressive-indent-mode))

(use-package autorevert
  :delight auto-revert-mode
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
  ;; ESLint
  (add-to-list 'compilation-error-regexp-alist
               '("^\\(/.*\\)" 1))
  ;; Node
  (add-to-list 'compilation-error-regexp-alist
               '("at [^ ]+ (\\(.+?\\):\\([[:digit:]]+\\):\\([[:digit:]]+\\)" 1 2 3)))

(use-package counsel
  :ensure
  :delight
  :bind (("C-c g" . counsel-git)
         ("C-c j" . counsel-git-grep)
         ("C-c k" . counsel-rg)
         ("C-x l" . counsel-locate))
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

(use-package emacs-lisp-mode
  :mode "Cask")

(use-package emmet-mode
  :ensure
  :delight
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
  (defun enable-web-mode-linter ()
    (when (equal (file-name-extension buffer-file-name) "js")
      (flycheck-add-mode 'javascript-jshint 'web-mode)
      (flycheck-add-mode 'javascript-eslint 'web-mode)
      (flycheck-add-mode 'javascript-jscs 'web-mode)
      (flycheck-add-mode 'javascript-standard 'web-mode)))
  (setq flycheck-mode-line-prefix nil)
  (global-flycheck-mode)
  (add-hooks-pair 'web-mode 'enable-web-mode-linter))

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
  :delight
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
  :delight
  :config
  (setq ivy-count-format "(%d/%d) "
        ivy-display-style 'fancy
        ivy-use-virtual-buffers t)
  (ivy-mode))

(use-package ivy-hydra
  :ensure
  :after ivy)

(use-package js-comint
  :ensure)

(use-package leuven-theme
  :ensure
  :config
  (load-theme 'leuven t))

(use-package magit
  :ensure
  :config
  (setq magit-completing-read-function 'ivy-completing-read
        magit-diff-arguments '("--no-ext-diff" "-w" "-C")
        magit-diff-refine-hunk 'all
        magit-diff-section-arguments '("--ignore-space-change"
                                       "--ignore-all-space"
                                       "--no-ext-diff"
                                       "-M"
                                       "-C")
        magit-save-repository-buffers 'dontask)
  (global-magit-file-mode))

(use-package markdown-mode
  :ensure)

(use-package org
  :ensure org-plus-contrib
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
  :delight '(:eval (concat " " (projectile-project-name)))
  :config
  (setq projectile-completion-system 'ivy
        projectile-create-missing-test-files t
        projectile-switch-project-action 'projectile-vc)
  (projectile-mode)
  (projectile-discover-projects-in-directory nick-projects-directory)
  (projectile-register-project-type 'jekyll '("_config.yml")
                                    :compile "jekyll build"
                                    :test "jekyll doctor"
                                    :run "jekyll serve")
  (projectile-register-project-type 'jekyll-bundler '("_config.yml")
                                    :compile "bundle exec jekyll build"
                                    :test "bundle exec jekyll doctor"
                                    :run "bundle exec jekyll serve")
  (projectile-register-project-type 'npm '("package.json")
                                    :compile "npm install"
                                    :test "npm test"
                                    :run "npm start"
                                    :test-suffix ".test")
  (projectile-register-project-type 'web '("index.html")
                                    :run 'browse-url-of-buffer)
  (projectile-register-project-type 'yarn '("yarn.lock")
                                    :compile "yarn"
                                    :test "yarn test"
                                    :run "yarn run start"
                                    :test-suffix ".test"))

(use-package rainbow-mode
  :ensure
  :delight
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

(use-package scroll-bar
  :config
  (set-scroll-bar-mode nil))

(use-package sh-script
  :config
  (setq sh-basic-offset nick-indent-level))

(use-package simple
  :delight auto-fill-function
  :config
  (setq kill-whole-line t)
  (add-hooks-pair 'text-mode 'auto-fill-mode))

(use-package smartparens
  :ensure
  :delight
  :config
  (require 'smartparens-config)
  (smartparens-global-strict-mode)
  (sp-use-smartparens-bindings)
  (show-smartparens-global-mode))

(use-package smex
  :ensure)

(use-package super-save
  :ensure
  :delight
  :config
  (setq super-save-auto-save-when-idle t)
  (super-save-mode))

(use-package tern
  :ensure
  :config
  (add-to-list 'tern-command "--no-port-file" t)
  (add-hooks-pair 'web-mode 'tern-mode))

(use-package tool-bar
  :config
  (tool-bar-mode 0))

(use-package typescript-mode
  :ensure
  :config
  (setq typescript-indent-level nick-indent-level))

(use-package undo-tree
  :ensure
  :delight
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

(use-package web-mode
  :ensure
  :mode
  "\\.html?\\'"
  "\\.jsx?\\'"
  "\\.json\\'"
  :config
  (setq web-mode-code-indent-offset nick-indent-level
        web-mode-css-indent-offset nick-indent-level
        web-mode-engines-alist '(("liquid" . "\\.html?\\'"))
        web-mode-markup-indent-offset nick-indent-level)
  (add-to-list 'web-mode-content-types '("jsx" . "\\.jsx?\\'")))

(use-package whitespace
  :delight global-whitespace-mode
  :config
  (setq whitespace-style '(face trailing tabs lines-tail empty tab-mark))
  (add-hooks-pair 'before-save 'whitespace-cleanup)
  (global-whitespace-mode))

(use-package yaml-mode
  :ensure)

;;; .emacs ends here
