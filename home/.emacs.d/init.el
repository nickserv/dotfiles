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

;;;; OS specific configuration
(if (eq window-system 'ns)
    (exec-path-from-shell-initialize)
  (setq default-frame-alist '((fullscreen . maximized))))

;;; Auto mode

;; Use yaml-mode.
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))

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
 (kill-emacs-query-functions . custom-prompt-customize-unsaved-options)
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

;;; CLI
;; Improve mouse support and margin display in terminals.
(unless window-system
  (bind-keys
   ("<mouse-4>" . scroll-down-line)
   ("<mouse-5>" . scroll-up-line))
  (defvar linum-format)
  (setq linum-format "%d "))

;;; Flycheck
(flycheck-pos-tip-mode)

;;; Magithub
;; Load after Magit and enable all features.
(with-eval-after-load
    "magit"
  (require 'magithub)
  (magithub-feature-autoinject t))

;;; Projectile
(projectile-mode)
(projectile-discover-projects-in-directory "~/Repos")
(projectile-register-project-type 'npm '("package.json") "npm install" "npm test" "npm start")
(projectile-register-project-type 'jekyll '("_config.yml") "bundle exec jekyll build" nil "bundle exec jekyll serve")

;;; Tern
(setq tern-command '("tern" "--no-port-file"))

;;; Abbreviate yes/no prompts.
(defalias 'yes-or-no-p 'y-or-n-p)

;;; init.el ends here
