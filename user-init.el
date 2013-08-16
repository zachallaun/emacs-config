;;; Load path ;;;
;;;;;;;;;;;;;;;;;

(add-to-list 'load-path "~/.emacs.d/")

;; Manually installed packages in lib/ and extra customization in user/
(add-to-list 'load-path "~/.emacs.d/lib/")
(add-to-list 'load-path "~/.emacs.d/user/")

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
    scala-mode
    sml-mode
    jade-mode
    yaml-mode
    js2-mode

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

    ;; like 'f' in vim
    iy-go-to-char

    ;; notational velocity-like note taking
    deft

    ;; vimium-like text jumping
    ace-jump-mode

    ;; text snippets
    yasnippet
    ))

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
(tool-bar-mode -1)
(menu-bar-mode -1)

;; overried "yes" or "no" prompts to always be "y" or "n" prompts
(fset 'yes-or-no-p 'y-or-n-p)

;; do not confirm before creating a new file
(setq confirm-nonexistent-file-or-buffer nil)

;; display column number
(setq column-number-mode t)

;; highlight the current line
(global-hl-line-mode 1)

;; scratch buffer should be empty
(setq initial-scratch-message "")

;; no splash screen
(setq inhibit-splash-screen t)

;; fullscreen on open
(add-hook 'after-init-hook 'toggle-frame-fullscreen)

;; automatically revert files when they change
(global-auto-revert-mode t)

;; insert spaces instead of tabs
(setq-default indent-tabs-mode nil)

;; automatically delete trailing whitespace before save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; font family
(set-default-font "Source Code Pro-12")

;; the cursor should be a bar instead of a block
(setq-default cursor-type 'bar)

;; underline matching parentheses when the cursor is on them
(show-paren-mode 1)
(setq-default show-paren-style 'parentheses)
(set-face-attribute 'show-paren-match-face nil
                    :weight 'bold :underline t)

;; use Emacs terminfo instead of system terminfo
;; this fixes terminal rendering issues
(setq system-uses-terminfo nil)

;; when you start typing after having marked a region, delete that region
;; and replace with what you're typing
(pending-delete-mode 1)

;; display line numbers when goto-line is invoked
(defun goto-line-with-feedback ()
  "Show line numbers temporarily, while prompting for the line number input"
  (interactive)
  (unwind-protect
      (progn
        (linum-mode 1)
        (goto-line (read-number "Goto line: ")))
    (linum-mode -1)))

(global-set-key [remap goto-line] 'goto-line-with-feedback)

;; replace last sexp with evaluated result
(defun replace-last-sexp ()
  (interactive)
  (let ((value (eval (preceding-sexp))))
    (kill-sexp -1)
    (insert (format "%s" value))))

(global-set-key (kbd "C-x C-r") 'replace-last-sexp)

;; uniquify adds more information to the status bar when buffers share names
;; e.g. instead of project.clj<2>, you get project.clj@my-project
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward)
(setq uniquify-separator "@")

;;; Package config ;;;
;;;;;;;;;;;;;;;;;;;;;;

;; magit
(global-set-key (kbd "C-c C-m") 'magit-status)

;; Ido everywhere
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)

;; don't confirm creation when switching to a non-existant buffer
(setq ido-create-new-buffer 'always)

;; yasnippet everywhere
(yas-global-mode 1)

;; slime
(add-to-list 'load-path "~/.emacs.d/lib/slime/")
(setq inferior-lisp-program "sbcl")
(require 'slime)
(slime-setup '(slime-repl slime-js))

;; smex - M-x Ido-like enhancement
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "C-x C-m") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command) ;; old M-x

;; color-theme-solarized
;; XXX: currently broken and shitty in terminal Emacs
(setq solarized-italic nil)
(load-theme 'solarized-dark t)

;; nrepl
(add-hook 'nrepl-interaction-mode-hook
	  'nrepl-turn-on-eldoc-mode)

;; clojure
(load "clojure-conf")

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
(load "julia-repl")
(setq julia-basic-repl-path
      "~/Dropbox/projects/julialang/julia/usr/bin/julia-release-basic")

;; auto-complete
(require 'auto-complete-config)
(ac-config-default)

;; auto-complete for nrepl
(require 'ac-nrepl)
(add-hook 'nrepl-mode-hook 'ac-nrepl-setup)
(add-hook 'nrepl-interaction-mode-hook 'ac-nrepl-setup)
(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'nrepl-mode))

;; auto-complete symbols for various other modes
(add-hook 'julia-mode-hook 'auto-complete-mode)

;; markdown-mode doesn't have default file extensions, so they need to be set
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; recognize Gemfiles and rake files as Ruby
(add-to-list 'auto-mode-alist '("Gemfile" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rake\\'" . ruby-mode))

;; javascript and js2-mode
(add-to-list 'auto-mode-alist (cons (rx ".js" eos) 'js2-mode))
(setq-default js2-basic-offset 2)
(setq js2-include-browser-externs t)
(setq js2-include-node-externs t)
(setq js2-strict-missing-semi-warning nil)
(setq js2-missing-semi-one-line-override t)
(add-hook 'js2-mode-hook 'slime-js-minor-mode)
(add-hook 'js2-mode-hook 'electric-pair-mode)
(global-set-key (kbd "C-c C-r") 'slime-js-reload)

;; js2-mode steals TAB; steal it back for yasnippet
(eval-after-load 'js2-mode
  '(progn
     (define-key js2-mode-map (kbd "TAB")
       (lambda()
         (interactive)
         (let ((yas/fallback-behavior 'return-nil))
           (unless (yas/expand)
             (indent-for-tab-command)
             (if (looking-back "^\s*")
                 (back-to-indentation))))))))

;; deft: share files with nvALT
(require 'deft)
(setq deft-directory "~/Dropbox/nvALT/")

;; deft: turn off autosave
(setq deft-auto-save-interval 0)

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

;; start deft-mode afresh on each invocation so that incremental search resets
(defun do-deft ()
  (interactive)
  (switch-to-buffer deft-buffer)
  (deft-mode))

(global-set-key (kbd "M-'") 'do-deft)
(global-set-key (kbd "C-'") 'deft-open-stack)

;; geiser
(add-hook 'geiser-mode-hook      'paredit-mode)
(add-hook 'geiser-repl-mode-hook 'paredit-mode)

;; iy-go-to-char
(global-set-key (kbd "C-c f") 'iy-go-to-char)
(global-set-key (kbd "C-c F") 'iy-go-to-char-backward)

;; ace-jump-mode
(global-set-key (kbd "C-c j") 'ace-jump-mode)

;; mode-line
(load "mode-line")

;; eshell
(load "eshell-conf")
(add-hook 'after-init-hook 'eshell)

;; proof-general (requires a homebrew install)
(let ((proof-general-el-file "/usr/local/share/emacs/site-lisp/ProofGeneral/generic/proof-site.el"))
  (when (file-exists-p proof-general-el-file)
    (load-file proof-general-el-file)))
