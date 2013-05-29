;;; Load path ;;;
;;;;;;;;;;;;;;;;;

(add-to-list 'load-path "~/.emacs.d/")

;; Manually installed packages in lib/
(add-to-list 'load-path "~/.emacs.d/lib/")

;;; Packages ;;;
;;;;;;;;;;;;;;;;

;; MEPLA
(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar user-packages
  '(paredit
    rainbow-delimiters
    auto-complete

    clojure-mode
    nrepl
    ac-nrepl

    jedi

    color-theme-solarized))

(dolist (p user-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;;; Configuration ;;;
;;;;;;;;;;;;;;;;;;;;;

;; disable things that are dumb
(blink-cursor-mode 0)
(scroll-bar-mode -1)
(mouse-wheel-mode -1)
(if window-system (tool-bar-mode -1))

;; display column number
(setq column-number-mode t)

;; automatically revert files when they change
(global-auto-revert-mode t)

;; highlight the current line
(global-hl-line-mode 1)

;; automatically delete trailing whitespace before save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; font family
(set-default-font "Source Code Pro-12")

;; Ido everywhere
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)

;; Highlight matching parentheses when the cursor is on them.
(show-paren-mode 1)

;; color-theme-solarized
(load-theme 'solarized-light t)

;; nrepl
(add-hook 'nrepl-interaction-mode-hook
	  'nrepl-turn-on-eldoc-mode)

;; paredit
(require 'paredit)
(add-hook 'clojure-mode-hook    'paredit-mode)
(add-hook 'nrepl-mode-hook      'paredit-mode)
(add-hook 'emacs-lisp-mode-hook 'paredit-mode)
(add-hook 'lisp-mode-hook       'paredit-mode)

;; rainbow-delimiters
(add-hook 'prog-mode-hook  'rainbow-delimiters-mode)
(add-hook 'nrepl-mode-hook 'rainbow-delimiters-mode)

;; Julia
(load "julia-mode")

;; auto-complete
(require 'auto-complete-config)
(ac-config-default)

;; ac-nrepl
(require 'ac-nrepl)
(add-hook 'nrepl-mode-hook 'ac-nrepl-setup)
(add-hook 'nrepl-interaction-mode-hook 'ac-nrepl-setup)
(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'nrepl-mode))
