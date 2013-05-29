
;;; Emacs display ;;;
;;;;;;;;;;;;;;;;;;;;;

(blink-cursor-mode 0)
(scroll-bar-mode -1)
(mouse-wheel-mode -1)
(if window-system (tool-bar-mode -1))
(setq column-number-mode t)

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
  '(;; major modes
    clojure-mode

    ;; minor modes
    paredit
    
    ;; themes
    color-theme-solarized
    ))

(dolist (p user-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;;; Configuration ;;;
;;;;;;;;;;;;;;;;;;;;;

;; Solarized
(load-theme 'solarized-light t)

;; Paredit hooks
(require 'paredit)
(add-hook 'clojure-mode-hook    'paredit-mode)
(add-hook 'emacs-lisp-mode-hook 'paredit-mode)
(add-hook 'lisp-mode-hook       'paredit-mode)
