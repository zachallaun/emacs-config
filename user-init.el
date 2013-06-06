;;; Load path ;;;
;;;;;;;;;;;;;;;;;

(add-to-list 'load-path "~/.emacs.d/")

;; Manually installed packages in lib/
(add-to-list 'load-path "~/.emacs.d/lib/")

;;; Packages ;;;
;;;;;;;;;;;;;;;;

;; MEPLA and Marmalade
(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives
	     '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar user-packages
  '(;; language modes
    clojure-mode
    markdown-mode
    haml-mode

    ;; slime-like support for scheme
    ;; requires a recent version of racket or guile
    geiser

    ;; interface for Git through Emacs
    magit

    ;; structured editing for Lisp S-expressions and delimiters
    ;; cheatsheet: http://mumble.net/~campbell/emacs/paredit.html
    paredit

    ;; nested delimiters (parens, brackets, etc.) are colored differently
    rainbow-delimiters

    ;; auto-completion w/ popup box
    auto-complete

    ;; enhanced Ido-mode-like M-x
    smex

    ;; Emacs client for nREPL, an alternative to slime + swank-clojure
    nrepl

    ;; Node.js REPL
    nodejs-repl

    ;; auto-complete extension for use with nrepl
    ac-nrepl

    ;; auto-complete extension for Python
    jedi

    ;; the solarized color theme for use with load-theme
    color-theme-solarized

    ;; notational velocity-like note taking
    deft))

(dolist (p user-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;;; Emacs config ;;;
;;;;;;;;;;;;;;;;;;;;

;; disable things that are dumb
(blink-cursor-mode 0)
(setq visible-bell t) ;; turns off alert bell
(scroll-bar-mode -1)
(mouse-wheel-mode -1)
(if window-system (tool-bar-mode -1))

;; overried "yes" or "no" prompts to always be "y" or "n" prompts
(defadvice yes-or-no-p (around _ activate)
  (setq ad-return-value (y-or-n-p (ad-get-arg 0))))

;; do not confirm before creating a new file
(setq confirm-nonexistent-file-or-buffer nil)

;; display column number
(setq column-number-mode t)

;; display line numbers in programming modes
(add-hook 'prog-mode-hook 'linum-mode)

;; open to an empty *scratch* buffer
(setq initial-scratch-message "")
(setq initial-buffer-choice t)

;; maximize the window on open
(add-hook 'after-init-hook 'toggle-frame-maximized)

;; automatically revert files when they change
(global-auto-revert-mode t)

;; highlight the current line
(global-hl-line-mode 1)

;; automatically delete trailing whitespace before save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; font family
(set-default-font "Source Code Pro-12")

;; highlight matching parentheses when the cursor is on them
(show-paren-mode 1)

;; in programming modes, show trailing whitespace and leading tabs, and
;; highlight characters past column 80
(custom-set-variables
  '(whitespace-line-column 80)
  '(whitespace-style '(face trailing tabs lines-tail)))
(add-hook 'prog-mode-hook 'whitespace-mode)

;; use Emacs terminfo instead of system terminfo
;; this fixes terminal rendering issues
(setq system-uses-terminfo nil)

;;; Package config ;;;
;;;;;;;;;;;;;;;;;;;;;;

;; magit
(global-set-key (kbd "M-g s") 'magit-status)

;; Ido everywhere
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)

;; smex - M-x Ido-like enhancement
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command) ;; old M-x

;; color-theme-solarized
;; XXX: currently broken and shitty in terminal Emacs
(setq solarized-italic nil)
(load-theme 'solarized-light t)

;; nrepl
(add-hook 'nrepl-interaction-mode-hook
	  'nrepl-turn-on-eldoc-mode)

;; paredit hooks
(require 'paredit)
(add-hook 'clojure-mode-hook    'paredit-mode)
(add-hook 'nrepl-mode-hook      'paredit-mode)
(add-hook 'emacs-lisp-mode-hook 'paredit-mode)
(add-hook 'lisp-mode-hook       'paredit-mode)
(add-hook 'scheme-mode-hook     'paredit-mode)

;; paredit: don't insert a space before delimiters
(add-hook 'paredit-mode-hook
	  (lambda ()
	    (add-to-list (make-local-variable
			  'paredit-space-for-delimiter-predicates)
			 (lambda (_ _) nil))))

;; rainbow-delimiters
(add-hook 'prog-mode-hook  'rainbow-delimiters-mode)
(add-hook 'nrepl-mode-hook 'rainbow-delimiters-mode)

;; Julia
(load "julia-mode")

;; auto-complete
(require 'auto-complete-config)
(ac-config-default)

;; auto-complete for nrepl
(require 'ac-nrepl)
(add-hook 'nrepl-mode-hook 'ac-nrepl-setup)
(add-hook 'nrepl-interaction-mode-hook 'ac-nrepl-setup)
(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'nrepl-mode))

;; markdown-mode doesn't have default file extensions, so they need to be set
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; recognize Gemfiles and rake files as Ruby
(add-to-list 'auto-mode-alist '("Gemfile" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rake\\'" . ruby-mode))

;; Common Lisp: use quicklisp's slime
;; XXX: requires that "quicklisp-slime-helper" be installed via quicklisp
(load (expand-file-name "~/quicklisp/slime-helper.el"))
(setq inferior-lisp-program "sbcl")

;; deft: share files with nvALT
(require 'deft)
(setq deft-directory "~/Dropbox/nvALT/")

;; deft: default to markdown-mode
(setq deft-extension "md")
(setq deft-text-mode 'markdown-mode)

;; deft: use the filename as the note title (compatible with nvALT)
(setq deft-use-filename-as-title t)

;; a "stack" note for things that don't deserve their own note
(defun deft-open-stack ()
  (interactive)
  (deft-open-file "~/Dropbox/nvALT/stack.md")
  (end-of-buffer))

(global-set-key (kbd "M-'") 'deft)
(global-set-key (kbd "C-'") 'deft-open-stack)

;; geiser config
(add-hook 'geiser-mode-hook      'paredit-mode)
(add-hook 'geiser-repl-mode-hook 'paredit-mode)
