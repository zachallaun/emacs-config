
;;; Emacs display ;;;
;;;;;;;;;;;;;;;;;;;;;

(blink-cursor-mode 0)
(scroll-bar-mode -1)
(mouse-wheel-mode -1)
(if window-system (tool-bar-mode -1))
(setq column-number-mode t)

(set-default-font "Source Code Pro-11")

;;; Packages ;;;
;;;;;;;;;;;;;;;;

;; Manually installed packages in lib/
(add-to-list 'load-path "~/.emacs.d/lib/")

;; MEPLA
(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar user-packages
  '(clojure-mode
    nrepl

    paredit
    rainbow-delimiters
    
    color-theme-solarized
    ))

(dolist (p user-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;;; Configuration ;;;
;;;;;;;;;;;;;;;;;;;;;

;; Highlight matching parentheses when the cursor is on them.
(show-paren-mode 1)

;; Solarized
(load-theme 'solarized-light t)

;; nrepl
(add-hook 'nrepl-interaction-mode-hook
	  'nrepl-turn-on-eldoc-mode)

;; Paredit hooks
(require 'paredit)
(add-hook 'clojure-mode-hook    'paredit-mode)
(add-hook 'nrepl-mode-hook      'paredit-mode)
(add-hook 'emacs-lisp-mode-hook 'paredit-mode)
(add-hook 'lisp-mode-hook       'paredit-mode)

;; Rainbow delimiters
(add-hook 'prog-mode-hook  'rainbow-delimiters-mode)
(add-hook 'nrepl-mode-hook 'rainbow-delimiters-mode)

;; Julia
(load "julia-mode")
