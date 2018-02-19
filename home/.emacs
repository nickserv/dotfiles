;;; .emacs --- Nick's Emacs configuration

;;; Commentary:

;; My GNU Emacs 25.1 configuration for macOS and Windows, using MELPA
;; and use-package.

;;; Code:

;;; Variables

(defconst nick-indent-level 2)

(setq auto-save-default nil
      backup-directory-alist `((".*" . ,(locate-user-emacs-file "backup/")))
      custom-file (locate-user-emacs-file "custom.el")
      default-frame-alist `((fullscreen . ,(if (eq window-system 'ns) 'fullboth 'maximized)))
      inhibit-startup-screen t
      initial-buffer-choice "~/Google Drive/Organizer.org"
      initial-scratch-message nil
      mouse-wheel-scroll-amount '(1 ((control)))
      ns-pop-up-frames nil
      package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "http://orgmode.org/elpa/")
                         ("gnu" . "https://elpa.gnu.org/packages/"))
      tab-always-indent 'complete
      visible-bell t)

(setq-default fill-column 80
              indent-tabs-mode nil
              line-spacing 0.4
              tab-width nick-indent-level)

;; Abbreviate yes/no prompts.
(defalias 'yes-or-no-p 'y-or-n-p)

;; Easy quitting
(defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
  "Prevent annoying \"Active processes exist\" query when you quit Emacs."
  (cl-letf (((symbol-function #'process-list) (lambda ())))
    ad-do-it))

;;; Package Setup
(package-initialize)

;; Install use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;;; Binds

(defun cdnm-list ()
  "List CDN dependencies in buffer."
  (interactive)
  (shell-command (format "cdnm list \"%s\"" buffer-file-name) "*cdnm-list*"))

(defun cdnm-update ()
  "Update CDN dependencies in buffer."
  (interactive)
  (shell-command (format "cdnm update \"%s\"" buffer-file-name) "*cdnm-update*"))

(bind-keys ("<mouse-4>" . scroll-down-line)
           ("<mouse-5>" . scroll-up-line)
           ("C-c m l" . cdnm-list)
           ("C-c m u" . cdnm-update)
           ("C-c s" . sort-lines)
           ("C-s" . swiper)
           ("C-x C-b" . ibuffer)
           ("M-/" . hippie-expand))

;;; Packages

(use-package delight
  :ensure)

(use-package exec-path-from-shell
  :ensure
  :if (eq window-system 'ns)
  :config
  (exec-path-from-shell-initialize))

(use-package add-node-modules-path
  :ensure
  :hook rjsx-mode)

(use-package aggressive-indent
  :ensure
  :delight
  :config
  (global-aggressive-indent-mode))

(use-package autorevert
  :custom
  (auto-revert-verbose nil)
  (global-auto-revert-non-file-buffers t)
  :delight auto-revert-mode
  :config
  (global-auto-revert-mode))

(use-package browse-at-remote
  :ensure
  :bind ("C-c r" . browse-at-remote))

(use-package company
  :ensure
  :custom
  (company-idle-delay 0)
  (company-minimum-prefix-length 1)
  :delight
  :config
  (global-company-mode))

(use-package compile
  :custom
  (compilation-ask-about-save nil)
  :config
  ;; ESLint
  (add-to-list 'compilation-error-regexp-alist
               '("^\\(/.*\\)" 1))
  ;; Node
  (add-to-list 'compilation-error-regexp-alist
               '("at [^ ]+ (\\(.+?\\):\\([[:digit:]]+\\):\\([[:digit:]]+\\))" 1 2 3))
  ;; Standard
  (add-to-list 'compilation-error-regexp-alist
               '("^  \\(.+?\\)::\\([[:digit:]]+\\):\\([[:digit:]]+\\)" 1 2 3)))

(use-package counsel
  :ensure
  :delight
  :bind (("C-c g" . counsel-git)
         ("C-c j" . counsel-git-grep)
         ("C-c k" . counsel-rg)
         ("C-x l" . counsel-locate))
  :config
  (counsel-mode))

(use-package counsel-projectile
  :ensure
  :config
  (counsel-projectile-mode))

(use-package css-mode
  :custom
  (css-indent-offset nick-indent-level))

(use-package dired
  :hook (dired-mode . dired-hide-details-mode)
  :custom
  (dired-recursive-copies 'always)
  (dired-recursive-deletes 'always))

(use-package ediff
  :custom
  (ediff-diff-options "-w")
  (ediff-split-window-function 'split-window-horizontally)
  (ediff-window-setup-function 'ediff-setup-windows-plain))

(use-package emacs-lisp-mode
  :mode "Cask")

(use-package emmet-mode
  :ensure
  :hook (css-mode rjsx-mode scss-mode sgml-mode)
  :delight)

(use-package ert
  :bind ("C-c e" . ert-run-tests-from-buffer)
  :config
  (defun ert-run-tests-from-buffer ()
    "Eval the current buffer and run all ERT tests."
    (interactive)
    (eval-buffer)
    (ert t)))

(use-package eshell
  :bind ("C-x t" . eshell))

(use-package flycheck
  :ensure
  :custom
  (flycheck-mode-line-prefix nil)
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
  :hook ((text-mode . flyspell-mode)
         (prog-mode . flyspell-prog-mode))
  :delight)

(use-package frame
  :custom
  (blink-cursor-mode)
  :config
  (let ((system-fonts '((ns .  "SF Mono")
                        (w32 . "Consolas"))))
    (set-frame-font (alist-get window-system system-fonts) nil t)))

(use-package gist
  :ensure)

(use-package ispell
  :custom
  (ispell-program-name "/usr/local/bin/aspell"))

(use-package ivy
  :bind ("C-c C-r" . ivy-resume)
  :custom
  (ivy-count-format "(%d/%d) ")
  (ivy-display-style 'fancy)
  (ivy-use-virtual-buffers t)
  :delight
  :config
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
  :custom
  (magit-completing-read-function 'ivy-completing-read)
  (magit-diff-arguments '("--no-ext-diff" "-w" "-C"))
  (magit-diff-refine-hunk 'all)
  (magit-diff-section-arguments '("--ignore-space-change"
                                  "--ignore-all-space"
                                  "--no-ext-diff"
                                  "-M"
                                  "-C"))
  (magit-save-repository-buffers 'dontask)
  :config
  (global-magit-file-mode))

(use-package magithub
  :ensure
  :after magit
  :config
  (magithub-feature-autoinject t))

(use-package markdown-mode
  :ensure)

(use-package npm-mode
  :ensure
  :delight
  :config
  (npm-global-mode))

(use-package org
  :ensure org-plus-contrib
  :bind (("C-c a" . org-agenda)
         ("C-c b" . org-iswitchb)
         ("C-c c" . org-capture)
         ("C-c l" . org-store-link)
         ("C-c o" . find-org-default-notes-file))
  :custom
  (org-agenda-files (list initial-buffer-choice))
  (org-default-notes-file initial-buffer-choice)
  (org-enforce-todo-checkbox-dependencies t)
  (org-enforce-todo-dependencies t)
  (org-fontify-whole-heading-line t)
  (org-log-done 'time)
  (org-log-repeat 'time)
  (org-modules '(org-mouse))
  (org-outline-path-complete-in-steps nil)
  (org-refile-targets '((nil :maxlevel . 10)))
  (org-refile-use-outline-path 'file)
  :config
  (defun find-org-default-notes-file ()
    "Open the default Org notes file."
    (interactive)
    (find-file org-default-notes-file)))

(use-package org-capture
  :custom
  (org-capture-templates '(("t"
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

(use-package prettier-js
  :ensure
  :hook (rjsx-mode . prettier-js-mode)
  :delight)

(use-package projectile
  :ensure
  :custom
  (projectile-completion-system 'ivy)
  (projectile-create-missing-test-files t)
  :delight '(:eval (concat " " (projectile-project-name)))
  :config
  (projectile-mode)
  (projectile-discover-projects-in-directory "~/Repos")
  (projectile-register-project-type 'web '("index.html")
                                    :run 'browse-url-of-buffer)
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
  (projectile-register-project-type 'yarn '("yarn.lock")
                                    :compile "yarn"
                                    :test "yarn test"
                                    :run "yarn run start"
                                    :test-suffix ".test"))

(use-package rainbow-mode
  :ensure
  :hook prog-mode
  :delight)

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

(use-package rjsx-mode
  :ensure
  :mode "\\.jsx?\\'"
  :custom
  (js-indent-level 2)
  (js2-mode-show-parse-errors nil)
  (js2-mode-show-strict-warnings nil))

(use-package saveplace
  :custom
  (save-place-mode t))

(use-package scroll-bar
  :custom
  (scroll-bar-mode nil))

(use-package sh-script
  :custom
  (sh-basic-offset nick-indent-level))

(use-package simple
  :hook (text-mode . auto-fill-mode)
  :custom
  (kill-whole-line t)
  (line-number-mode)
  :delight auto-fill-function)

(use-package smartparens-config
  :ensure smartparens
  :delight smartparens-mode
  :config
  (smartparens-global-strict-mode)
  (sp-use-smartparens-bindings)
  (show-smartparens-global-mode))

(use-package smex
  :ensure)

(use-package super-save
  :ensure
  :custom
  (super-save-auto-save-when-idle t)
  :delight
  :config
  (super-save-mode))

(use-package tern
  :ensure
  :hook (rjsx-mode . tern-mode)
  :delight
  :config
  (add-to-list 'tern-command "--no-port-file" t))

(use-package tool-bar
  :config
  (tool-bar-mode 0))

(use-package undo-tree
  :ensure
  :custom
  (undo-tree-visualizer-diff t)
  (undo-tree-visualizer-timestamps t)
  :delight
  :config
  (global-undo-tree-mode))

(use-package vc
  :custom
  (vc-diff-switches "-w")
  (vc-follow-symlinks t))

(use-package vc-git
  :custom
  (vc-git-diff-switches "-w -C"))

(use-package wakatime-mode
  :ensure
  :delight
  :config
  (global-wakatime-mode))

(use-package whitespace
  :hook (before-save . whitespace-cleanup)
  :custom
  (whitespace-style '(face trailing tabs lines-tail empty tab-mark))
  :delight global-whitespace-mode
  :config
  (global-whitespace-mode))

(use-package yaml-mode
  :ensure)

;;; .emacs ends here
