;;; .emacs --- Nicky's Emacs configuration

;;; Commentary:

;; My GNU Emacs 29.4 configuration for macOS and Windows, using MELPA,
;; ELPA, and use-package.

;;; Code:

;;; Variables

(setq auto-save-default nil
      backup-directory-alist `((".*" . ,(locate-user-emacs-file "backup/")))
      custom-file (locate-user-emacs-file "custom.el")
      default-frame-alist `((fullscreen . ,(if (eq window-system 'ns) 'fullboth 'maximized)))
      inhibit-startup-screen t
      initial-scratch-message nil
      ns-pop-up-frames nil
      package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("gnu" . "https://elpa.gnu.org/packages/"))
      tab-always-indent 'complete
      visible-bell t)

(setq-default fill-column 80
              tab-width 2)

;; Abbreviate yes/no prompts.
(defalias 'yes-or-no-p 'y-or-n-p)

;; Easy quitting
(defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
  "Prevent annoying \"Active processes exist\" query when you quit Emacs."
  (cl-letf (((symbol-function #'process-list) (lambda ())))
    ad-do-it))

;; use-package
(require 'use-package)
(setq use-package-always-defer t
      use-package-always-ensure t)

;;; Binds

(defun cdnm-list ()
  "List CDN dependencies in buffer."
  (interactive)
  (shell-command (format "cdnm list \"%s\"" buffer-file-name) "*cdnm-list*"))

(defun cdnm-update ()
  "Update CDN dependencies in buffer."
  (interactive)
  (shell-command (format "cdnm update \"%s\"" buffer-file-name) "*cdnm-update*"))

(bind-keys ("C-c m l" . cdnm-list)
           ("C-c m u" . cdnm-update)
           ("C-c s" . sort-lines)
           ("C-x C-b" . ibuffer)
           ("M-/" . hippie-expand))

;;; Packages

(use-package diminish)

(use-package exec-path-from-shell
  :if (eq window-system 'ns)
  :init
  (exec-path-from-shell-initialize))

(use-package add-node-modules-path
  :disabled
  :hook prog-mode)

(use-package aggressive-indent
  :diminish
  :init
  (global-aggressive-indent-mode))

(use-package auto-package-update
  :config
  (auto-package-update-maybe))

(use-package autorevert
  :custom
  (auto-revert-verbose nil)
  (global-auto-revert-non-file-buffers t)
  :diminish auto-revert-mode
  :init
  (global-auto-revert-mode))

(use-package browse-at-remote
  :bind ("C-c r" . browse-at-remote))

(use-package company
  :custom
  (company-idle-delay 0)
  (company-minimum-prefix-length 1)
  :diminish
  :init
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
  :diminish
  :init
  (counsel-mode))

(use-package counsel-projectile
  :after counsel
  :init
  (counsel-projectile-mode))

(use-package dired
  :ensure nil
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
  :ensure nil
  :mode "Cask"
  :init
  (setq-default indent-tabs-mode nil))

(use-package emmet-mode
  :hook (css-mode rjsx-mode scss-mode sgml-mode)
  :diminish)

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
  :custom
  (flycheck-mode-line-prefix nil)
  :init
  (global-flycheck-mode))

(use-package flycheck-package
  :after flycheck
  :init
  (flycheck-package-setup))

(use-package flycheck-pos-tip
  :after flycheck
  :init
  (flycheck-pos-tip-mode))

(use-package flyspell
  :hook ((text-mode . flyspell-mode)
         (prog-mode . flyspell-prog-mode))
  :diminish)

(use-package frame
  :ensure nil
  :custom
  (blink-cursor-mode))

(use-package gist)

(use-package ispell
  :custom
  (ispell-program-name "/usr/local/bin/aspell"))

(use-package ivy
  :bind ("C-c C-r" . ivy-resume)
  :custom
  (ivy-count-format "(%d/%d) ")
  (ivy-display-style 'fancy)
  (ivy-use-virtual-buffers t)
  :diminish
  :init
  (ivy-mode))

(use-package ivy-hydra
  :demand
  :after ivy)

(use-package js-comint)

(use-package lsp-mode
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l"))

(use-package magit
  :custom
  (magit-diff-arguments '("--no-ext-diff" "-w" "-C"))
  (magit-diff-refine-hunk 'all)
  (magit-diff-section-arguments '("--ignore-space-change"
                                  "--ignore-all-space"
                                  "--no-ext-diff"
                                  "-M"
                                  "-C"))
  (magit-save-repository-buffers 'dontask))

(use-package markdown-mode)

(use-package npm-mode
  :diminish
  :init
  (npm-global-mode))

(use-package org
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
  :ensure nil
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
  :hook (prog-mode . prettier-js-mode)
  :custom
  (prettier-js-show-errors nil)
  :diminish)

(use-package projectile
  :custom
  (projectile-create-missing-test-files t)
  (projectile-mode-line '(:eval (concat " " (projectile-project-name))))
  (projectile-use-git-grep t)
  :config
  (projectile-discover-projects-in-directory "~/Repos")
  (projectile-register-project-type 'web '("index.html")
                                    :run 'browse-url-of-buffer)
  (projectile-register-project-type 'npm '("package.json")
                                    :compile "npm install"
                                    :test "npm test"
                                    :run "npm start"
                                    :test-suffix ".test")
  (projectile-register-project-type 'yarn '("package.json" "yarn.lock")
                                    :compile "yarn"
                                    :test "yarn test"
                                    :run "yarn start"
                                    :test-suffix ".test")
  (projectile-register-project-type 'jekyll '("Gemfile" "_config.yml")
                                    :compile "bundle exec jekyll build"
                                    :test "bundle exec jekyll doctor"
                                    :run "bundle exec jekyll serve"))

(use-package rainbow-mode
  :hook prog-mode
  :diminish)

(use-package restclient
  :bind ("C-c h" . restclient-buffer)
  :config
  (defun restclient-buffer ()
    (interactive)
    (let ((buffer "*HTTP Request*"))
      (get-buffer-create buffer)
      (switch-to-buffer-other-window buffer)
      (restclient-mode))))

(use-package rjsx-mode
  :mode "\\.jsx?\\'"
  :custom
  (js-indent-level 2)
  (js2-mode-show-parse-errors nil)
  (js2-mode-show-strict-warnings nil))

(use-package saveplace
  :custom
  (save-place-mode t))

(use-package scroll-bar
  :ensure nil
  :custom
  (scroll-bar-mode nil))

(use-package simple
  :ensure nil
  :hook (prog-mode . auto-fill-mode)
  :custom
  (kill-whole-line t)
  (line-number-mode)
  :diminish auto-fill-function)

(use-package smartparens
  :diminish
  :init
  (require 'smartparens-config)
  (smartparens-global-strict-mode)
  :config
  (sp-use-smartparens-bindings)
  (show-smartparens-global-mode))

(use-package smex)

(use-package super-save
  :custom
  (super-save-auto-save-when-idle t)
  :diminish
  :init
  (super-save-mode))

(use-package swiper
  :bind ("C-s" . swiper))

(use-package tern
  :hook (rjsx-mode . tern-mode)
  :diminish
  :config
  (add-to-list 'tern-command "--no-port-file" t))

(use-package tool-bar
  :ensure nil
  :config
  (tool-bar-mode 0))

(use-package undo-tree
  :custom
  (undo-tree-visualizer-diff t)
  (undo-tree-visualizer-timestamps t)
  :diminish
  :init
  (global-undo-tree-mode))

(use-package vc
  :custom
  (vc-diff-switches "-w")
  (vc-follow-symlinks t))

(use-package vc-git
  :ensure nil
  :custom
  (vc-git-diff-switches "-w -C"))

(use-package whitespace
  :hook (before-save . whitespace-cleanup))

(use-package yaml-mode)

(use-package lsp-mode
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l"))

;;; .emacs ends here
