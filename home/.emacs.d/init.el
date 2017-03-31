;;; init.el --- Nick's Emacs configuration

;;; Commentary:
;; My personal GNU Emacs 25.1 configuration for macOS, using MELPA and
;; use-package.  Dependencies are installed automatically, but you may
;; need to restart Emacs to apply customizations for newly installed
;; packages.

;;; Code:

;;; Initial setup

;;;; Customize Migration

;; Set variables.
(setq auto-save-default nil
      backup-directory-alist '((".*" . "~/.emacs.d/backup/"))
      custom-file (locate-user-emacs-file "custom.el")
      default-frame-alist '((fullscreen . maximized))
      fill-column 80
      inhibit-startup-screen t
      initial-buffer-choice "~/Google Drive/Organizer.org"
      initial-scratch-message nil
      mouse-wheel-scroll-amount '(1 ((control)))
      package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("gnu" . "https://elpa.gnu.org/packages/"))
      tab-always-indent 'complete
      tab-width 2
      visible-bell t)
(setq-default indent-tabs-mode nil
              tab-width 2)
(ansi-color-for-comint-mode-on)
(set-frame-font "Source Code Pro" nil t)
(defalias 'yes-or-no-p 'y-or-n-p)

;; Set included minor modes.
(blink-cursor-mode 0)
(electric-layout-mode)
(electric-pair-mode)
(line-number-mode 0)
(menu-bar-mode 0)
(save-place-mode)
(savehist-mode)
(set-scroll-bar-mode nil)
(show-paren-mode)
(tool-bar-mode 0)
(xterm-mouse-mode)

;; Initialize installed packages.
(package-initialize)
(load custom-file)

;; Download package data and install packages selected in customize.
(package-refresh-contents)
(package-install-selected-packages)

;; OS specific configuration
(cond
 ((eq window-system 'ns)
  (exec-path-from-shell-initialize)
  (setq default-frame-alist '((fullscreen . fullboth))))
 ((not window-system)
  (bind-keys
   ("<mouse-4>" . scroll-down-line)
   ("<mouse-5>" . scroll-up-line))
  (defvar linum-format)
  (setq linum-format "%d ")))

;; Configure packages.
(use-package autorevert
  :config
  (setq global-auto-revert-non-file-buffers t))
(use-package compile
  :init
  (setq compilation-ask-about-save nil))
(use-package counsel
  :init
  (counsel-mode)
  (ivy-mode)
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
(use-package flycheck
  :init
  (global-flycheck-mode))
(use-package flycheck-pos-tip-mode
  :after flycheck
  :init
  (flycheck-pos-tip-mode))
(use-package ispell
  :config
  (setq ispell-program-name "/usr/local/bin/aspell"))
(use-package js
  :config
  (setq js-indent-level 2))
(use-package leuven-theme
  :init
  (load-theme 'leuven t))
(use-package magit
  :init
  (global-magit-file-mode)
  :config
  (setq magit-completing-read-function 'ivy-completing-read
        magit-diff-arguments '("--no-ext-diff" "-w" "-C")
        magit-diff-refine-hunk 'all
        magit-diff-section-arguments '("--ignore-space-change" "--ignore-all-space" "--no-ext-diff" "-M" "-C")
        magit-repository-directories '(("~/Repos" . 1))
        magit-save-repository-buffers 'dontask))
(use-package magithub
  :after magit
  :config
  (magithub-feature-autoinject t))
(use-package org
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
        org-refile-use-outline-path 'file))
(use-package org-capture
  :config
  (setq org-capture-templates '(("t" "Task" entry
                                 (file+headline "" "Tasks")
                                 "* TODO %?
  %u
  %a")
                                ("n" "Note" entry
                                 (file+headline "" "Notes")
                                 "* %?
  %i
  %a"))))
(use-package projectile
  :init
  (projectile-mode)
  (projectile-discover-projects-in-directory "~/Repos")
  (projectile-register-project-type 'npm '("package.json") "npm install" "npm test" "npm start")
  (projectile-register-project-type 'jekyll '("_config.yml") "bundle exec jekyll build" nil "bundle exec jekyll serve")
  :config
  (setq projectile-completion-system 'ivy))
(use-package sh-script
  :config
  (setq sh-basic-offset 2))
(use-package super-save
  :init
  (super-save-mode)
  :config
  (setq super-save-auto-save-when-idle t))
(use-package tern
  :config
  (setq tern-command '("tern" "--no-port-file")))
(use-package undo-tree
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
  :config
  (setq whitespace-style '(face trailing tabs lines-tail empty tab-mark)))
(use-package yaml-mode
  :mode ("\\.yml\\'" . yaml-mode))

;;; Hooks

;;;; Helpers

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

;;;; Hook functions

(defun colorize-compilation-buffer ()
  "Use ANSI colors for compilation."
  (defvar compilation-filter-start)
  (ansi-color-apply-on-region compilation-filter-start (point)))

(defun bind-term-paste ()
  "Bind `term-paste'."
  (defvar term-raw-map)
  (bind-key "C-c C-y" #'term-paste term-raw-map))

;;;; Set hooks
(add-hooks
 ;; Programming modes
 (prog-mode-hook . (flyspell-prog-mode linum-mode rainbow-mode))

 ;; Text modes
 (text-mode-hook . (flyspell-mode auto-fill-mode))

 ;; Clean whitespace when saving.
 (before-save-hook . whitespace-cleanup)

 ;; Use Emmet to complete CSS and HTML.
 ((css-mode-hook sgml-mode-hook) . emmet-mode)

 ;; Use Tern for JS analysis.
 (js-mode-hook . tern-mode)

 ;; Use Paredit for Lisp editing.
 ((emacs-lisp-mode-hook
   eval-expression-minibuffer-setup-hook
   ielm-mode-hook
   lisp-mode-hook
   lisp-interaction-mode-hook
   scheme-mode-hook)
  . enable-paredit-mode)

 ;; Usability fixes
 (compilation-filter-hook . colorize-compilation-buffer)
 (term-mode-hook . bind-term-paste))

;;; Keys
(defun find-org-default-notes-file ()
  "Open the default Org notes file."
  (interactive)
  (find-file org-default-notes-file))
(bind-keys
 ("C-c C-r" . ivy-resume)
 ("C-c r" . browse-at-remote)
 ("C-x C-S-c" . restart-emacs)

 ;; Improved defaults
 ("C-s" . swiper)
 ("C-x C-b" . ibuffer)
 ("M-/" . hippie-expand)

 ;; Counsel interfaces
 ("C-c g" . counsel-git)
 ("C-c j" . counsel-git-grep)

 ;; Magit
 ;; Set binds everywhere so it can be launched from non-file buffers.
 ("C-x g" . magit-status)
 ("C-x M-g" . magit-dispatch-popup)

 ;; Org
 ("C-c a" . org-agenda)
 ("C-c b" . org-iswitchb)
 ("C-c c" . org-capture)
 ("C-c l" . org-store-link)

 ;; Sane term
 ("C-x t" . sane-term)
 ("C-x T" . sane-term-create)

 ("C-c o" . find-org-default-notes-file))

;;; init.el ends here
