;;; init.el --- zachallaun's emacs config

;; Turn off mouse and wheel interface early to avoid momentary display
(if window-system (scroll-bar-mode -1))
(if window-system (mouse-wheel-mode -1))
(if window-system (tool-bar-mode -1))
(menu-bar-mode -1)

(setq inhibit-splash-screen t)

;;----------------------------------------------------------------------------
;;-- packages.init
;;----------------------------------------------------------------------------

;; MEPLA and Marmalade
(require 'package)
(setq package-user-dir "~/.emacs.d/elpa/")
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives
	     '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

;;----------------------------------------------------------------------------
;;-- packages.list
;;----------------------------------------------------------------------------

(defvar user-packages
  '(;; language modes
    clojure-mode
    markdown-mode
    haml-mode
    scss-mode
    scala-mode
    sml-mode
    jade-mode
    yaml-mode
    js2-mode

    ;; slime-like support for scheme
    ;; requires a recent version of racket or guile
    geiser

    ;; interface for Git through Emacs
    ;;; TODO: temporarily use a magit checkout while the git-commit
    ;;; workflow is being worked out
    ;;; TODO: remove (load "lib/magit")
    ;; magit

    ;; git-related modes
    gitconfig-mode
    gitignore-mode

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

    ;; offline clojure cheatsheet
    clojure-cheatsheet

    ;; fuzzy matching for ido
    flx-ido

    ;; narrowing and selection
    helm

    ;; irc client
    circe))

;;----------------------------------------------------------------------------
;;-- packages.install
;;----------------------------------------------------------------------------

(defun za/install-packages ()
  "You know... install packages."
  (interactive)
  (dolist (p user-packages)
    (when (not (package-installed-p p))
      (package-install p))))

(za/install-packages)

;;----------------------------------------------------------------------------
;;-- bootstrap.macros
;;----------------------------------------------------------------------------

(defmacro after (mode &rest body)
  "After MODE loads, evaluate BODY."
  (declare (indent defun))
  `(eval-after-load ,mode
     '(progn ,@body)))

;;----------------------------------------------------------------------------
;;-- bootstrap.imenu
;;----------------------------------------------------------------------------

(defun imenu-elisp-sections ()
  (setq imenu-prev-index-position-function nil)
  (add-to-list 'imenu-generic-expression '("Sections" "^;;-- \\(.+\\)$" 1) t))

(add-hook 'emacs-lisp-mode-hook 'imenu-elisp-sections)

;;----------------------------------------------------------------------------
;;-- bootstrap.load-path
;;----------------------------------------------------------------------------

(add-to-list 'load-path "~/.emacs.d/")

;; Manually installed packages in lib/ and extra customization in user/
(add-to-list 'load-path "~/.emacs.d/lib/")
(add-to-list 'load-path "~/.emacs.d/user/")

;;----------------------------------------------------------------------------
;;-- bootstrap.private
;;----------------------------------------------------------------------------

(let ((private-file "~/.private.el"))
  (when (file-exists-p private-file)
    (load-file private-file)))

;;----------------------------------------------------------------------------
;;-- init.core
;;----------------------------------------------------------------------------

;; no need to garbage collect every 0.76MB
(setq gc-cons-threshold 20000000)

;; run emacs in server mode if running in a GUI
(require 'server)
(if (and window-system (not (server-running-p)))
    (server-start))

;; disable things that are dumb
(blink-cursor-mode 0)
(setq visible-bell t) ;; turns off alert bell

;; overried "yes" or "no" prompts to always be "y" or "n" prompts
(fset 'yes-or-no-p 'y-or-n-p)

;; do not confirm before creating a new file
(setq confirm-nonexistent-file-or-buffer nil)

;; display column number
(setq column-number-mode t)

;; highlight the current line
(global-hl-line-mode 1)

;; scratch buffer should be... simpler
(setq initial-scratch-message ";; scratch
")

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

;; replace last sexp with evaluated result
(defun replace-last-sexp ()
  (interactive)
  (let ((value (eval (preceding-sexp))))
    (kill-sexp -1)
    (insert (format "%s" value))))

(global-set-key (kbd "C-x C-r") 'replace-last-sexp)

;; Cmd+Ret to toggle fullscreen
(global-set-key (kbd "s-<return>") 'toggle-frame-fullscreen)

;; kill buffers with Cmd-w
(global-set-key (kbd "s-w") '(lambda () (interactive) (kill-buffer (current-buffer))))

;; Don't clutter up directories with files~
(setq backup-directory-alist `(("." . ,(expand-file-name "~/.emacs.d/backups/"))))

;; Don't clutter with #files either
(setq auto-save-file-name-transforms
      `((".*" ,(expand-file-name "~/.emacs.d/backups/"))))

;;----------------------------------------------------------------------------
;;-- init.editor
;;----------------------------------------------------------------------------

;;-- init.editor.ac
(require 'auto-complete-config)
(ac-config-default)

;; auto-complete for nrepl
(after 'auto-complete-autoloads
  (require 'ac-nrepl)
  (add-hook 'nrepl-mode-hook 'ac-nrepl-setup)
  (add-hook 'nrepl-interaction-mode-hook 'ac-nrepl-setup)
  (add-to-list 'ac-modes 'nrepl-mode))

;; auto-complete symbols for various other modes
(add-hook 'julia-mode-hook 'auto-complete-mode)

;;----------------------------------------------------------------------------
;;-- init.git
;;----------------------------------------------------------------------------

;; TODO: remove when switching back to melpa magit
(add-to-list 'load-path "~/.emacs.d/lib/magit/")
(require 'magit)

(global-set-key (kbd "C-c m") 'magit-status)

(after 'git-commit-mode
  ;; C-c C-k during a commit to cancel
  (define-key git-commit-mode-map (kbd "C-c C-k") '(lambda () (interactive)
                                                     (kill-buffer)
                                                     (other-window 1)))

  ;; switch back to status window after committing
  (defadvice git-commit-commit (after switch-to-magit-status activate)
    (other-window 1)))

;;----------------------------------------------------------------------------
;;-- init.color
;;----------------------------------------------------------------------------

(require 'color-theme)
(require 'color-theme-solarized)

;; color theme
;; XXX: currently broken and shitty in terminal Emacs
(setq solarized-italic nil)

;; taken from solarized-definitions.el `solarized-color-definitions`
(defun color-theme-color (name)
  (let ((index (if window-system
                   (if solarized-degrade
                       3
                     (if solarized-broken-srgb 2 1))
                 (case (display-color-cells)
                   (16 4)
                   (8  5)
                   (otherwise 3)))))
    (nth index (assoc name solarized-colors))))

(defun dark ()
  "Load a dark color theme"
  (interactive)
  (load-theme 'solarized-dark t)
  (color-theme-install-frame-params
   '((background-color . "gray240")))
  (set-face-attribute 'fringe nil
                      :background (color-theme-color 'base03))
  (set-face-attribute 'region nil
                      :foreground (color-theme-color 'base01)
                      :background (color-theme-color 'base2))
  (set-face-background hl-line-face (color-theme-color 'base02)))

(defun light ()
  "Load a light color theme"
  (interactive)
  (load-theme 'solarized-light t)
  (set-face-attribute 'fringe nil
                      :background (color-theme-color 'base3))
  (set-face-attribute 'region nil
                      :foreground (color-theme-color 'base1)
                      :background (color-theme-color 'base02)))

(add-hook 'after-init-hook 'light)

;;----------------------------------------------------------------------------
;;-- init.nav
;;----------------------------------------------------------------------------

;;-- init.nav.helm
(defun helm-mini-or-imenu (imenu?) (interactive "P")
  (if imenu? (helm-imenu) (helm-mini)))

(global-set-key (kbd "C-c h") 'helm-mini-or-imenu)

(after 'helm
  (load "color")

  (set-face-attribute 'helm-selection nil
                      :background (color-theme-color 'base02)
                      :foreground (color-theme-color 'base2))

  (set-face-attribute 'helm-source-header nil
                      :height 1.5
                      :foreground (color-theme-color 'magenta)
                      :background nil)

  (after 'helm-match-plugin
    (set-face-attribute 'helm-match nil
                        :underline t
                        :inherit nil)))

;;-- init.nav.ido
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(setq ido-case-fold t)
(ido-mode 1)

;;-- init.nav.smex
;; M-x Ido-like enhancement
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "C-x C-m") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command) ;; old M-x

;; don't confirm creation when switching to a non-existant buffer
(setq ido-create-new-buffer 'always)

;;-- init.nav.flx
(require 'flx-ido)
(flx-ido-mode 1)
(setq ido-use-faces nil) ; to see flx highlights

;;-- init.nav.uniquify
;; uniquify adds more information to the status bar when buffers share names
;; e.g. instead of project.clj<2>, you get project.clj@my-project
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward)
(setq uniquify-separator "@")

;;-- init.nav.goto
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

;;-- init.nav.iy-go-to-char
(global-set-key (kbd "C-c f") 'iy-go-to-char)
(global-set-key (kbd "C-c F") 'iy-go-to-char-backward)

;;-- init.nav.ace-jump
(global-set-key (kbd "C-c j") 'ace-jump-mode)

;;----------------------------------------------------------------------------
;;-- init.yas
;;----------------------------------------------------------------------------

;;(yas-global-mode 1)

;;----------------------------------------------------------------------------
;;-- init.lisp
;;----------------------------------------------------------------------------

(add-to-list 'load-path "~/.emacs.d/lib/slime/")
(setq inferior-lisp-program "sbcl")
(require 'slime)
(slime-setup '(slime-repl slime-js))

;;----------------------------------------------------------------------------
;;-- init.clojure
;;----------------------------------------------------------------------------

(after 'clojure-mode
  (define-clojure-indent
    ;; midje
    (fact 'defun) (facts 'defun)

    ;; core.logic
    (run* 'defun)
    (run 'defun)
    (fresh 'defun)
    (defne 'defun)
    (project 'defun)
    (matche 'defun)

    ;; core.match
    (match 'defun)

    ;; core.async
    (go 'defun)

    ;; core.async helpers
    (pipeline 'defun)
    (while-open 'defun)
    (when-recv 'defun)
    (if-recv 'defun)

    ;; simple-check
    (for-all 'defun)))

;;-- init.clojure.nrepl

(defun nrepl-send-dwim ()
  "Send the appropriate forms to the REPL to be evaluated."
  (interactive)
  (let ((expr (nrepl-last-expression)))
    (pop-to-buffer (nrepl-find-or-create-repl-buffer))
    (goto-char (point-max))
    (insert expr)
    (nrepl-return)
    (other-window 1)))

(defun nrepl-send-reset (refresh?)
  "Send the form '(do (in-ns 'user) (reset)) to the REPL. Given a
prefix, send the form '(do (in-ns 'user) (refresh))."
  (interactive "P")
  (let ((form (if refresh?
                  "(do (in-ns 'user) (refresh))"
                "(do (in-ns 'user) (reset))")))
    (pop-to-buffer (nrepl-find-or-create-repl-buffer))
    (insert form)
    (nrepl-return)
    (other-window 1)))

(after 'nrepl
  (define-key nrepl-interaction-mode-map (kbd "C-c C-c") 'nrepl-send-dwim)
  (define-key nrepl-interaction-mode-map (kbd "C-c C-r") 'nrepl-send-reset)

  (add-hook 'nrepl-interaction-mode-hook
            'nrepl-turn-on-eldoc-mode))

;;----------------------------------------------------------------------------
;;-- init.paredit
;;----------------------------------------------------------------------------

;; don't insert a space before delimiters
(after 'paredit
  (add-hook 'paredit-mode-hook
            (lambda ()
              (add-to-list (make-local-variable
                            'paredit-space-for-delimiter-predicates)
                           (lambda (_ _) nil)))))

(add-hook 'emacs-lisp-mode-hook 'paredit-mode)
(after 'clojure-mode     (add-hook 'clojure-mode-hook 'paredit-mode))
(after 'nrepl            (add-hook 'nrepl-mode-hook 'paredit-mode))
(after 'scheme-mode      (add-hook 'scheme-mode-hook 'paredit-mode))
(after 'common-lisp-mode (add-hook 'lisp-mode-hook 'paredit-mode))
(after 'geiser
  (add-hook 'geiser-mode-hook 'paredit-mode)
  (add-hook 'geiser-repl-mode-hook 'paredit-mode))

;;----------------------------------------------------------------------------
;;-- init.rainbow-delimiters
;;----------------------------------------------------------------------------

(add-hook 'prog-mode-hook  'rainbow-delimiters-mode-enable)
(add-hook 'nrepl-mode-hook 'rainbow-delimiters-mode-enable)

;;----------------------------------------------------------------------------
;;-- init.julia
;;----------------------------------------------------------------------------

(load "julia-mode")
(load "julia-repl")
(setq julia-basic-repl-path
      "~/Dropbox/projects/julialang/julia/usr/bin/julia-release-basic")

;;----------------------------------------------------------------------------
;;-- init.markdown
;;----------------------------------------------------------------------------

(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;;----------------------------------------------------------------------------
;;-- init.html
;;----------------------------------------------------------------------------

(add-hook 'html-mode-hook
          (lambda ()
            (set (make-local-variable 'sgml-basic-offset) 4)))

;; magit wants C-c C-m; take it from html-mode
(after 'sgml-mode
  (define-key html-mode-map (kbd "C-c C-m") nil))

;;----------------------------------------------------------------------------
;;-- init.ruby
;;----------------------------------------------------------------------------

;; recognize Gemfiles and rake files as Ruby
(add-to-list 'auto-mode-alist '("Gemfile" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rake\\'" . ruby-mode))

;;----------------------------------------------------------------------------
;;-- init.javascript
;;----------------------------------------------------------------------------

(add-hook 'javascript-mode 'electric-pair-mode)
(add-hook 'javascript-mode 'rainbow-delimiters-mode)
(setq-default js-indent-level 2)

(after 'js2-mode
  ;;(add-to-list 'auto-mode-alist (cons (rx ".js" eos) 'js2-mode))

  (setq-default js2-basic-offset 2)
  (setq js2-include-browser-externs t)
  (setq js2-include-node-externs t)
  (setq js2-strict-missing-semi-warning nil)
  (setq js2-missing-semi-one-line-override t)
  (add-hook 'js2-mode-hook 'slime-js-minor-mode)
  (add-hook 'js2-mode-hook 'electric-pair-mode)
  (add-hook 'js2-mode-hook 'rainbow-delimiters-mode)
  (global-set-key (kbd "C-c C-r") 'slime-js-reload)

  ;; js2-mode steals TAB; steal it back for yasnippet
  (define-key js2-mode-map (kbd "TAB")
    (lambda()
      (interactive)
      (let ((yas/fallback-behavior 'return-nil))
        (unless (yas/expand)
          (indent-for-tab-command)
          (if (looking-back "^\s*")
              (back-to-indentation)))))))

;;----------------------------------------------------------------------------
;;-- init.javascript.skewer
;;----------------------------------------------------------------------------

(after 'skewer-mode

  ;; so that it's possible to evaluate w/o the js2 parser
  (defun skewer-eval-print-region (start end)
    "Evaluate the region as JavaScript code and insert the result
into the buffer at the end of the region."
    (interactive)
    (let* ((request (skewer-eval (buffer-substring-no-properties start end)
                                 #'skewer-post-print
                                 :verbose t))
           (id (cdr (assoc 'id request)))
           (pos (cons (current-buffer) end)))
      (setf (get-cache-table id skewer-eval-print-map) pos)))

  (defun skewer-eval-region (start end &optional print?)
    "Evaluate the region as JavaScript."
    (interactive "r\nP")
    (if print?
        (skewer-eval-print-region start end)
      (skewer-eval (buffer-substring-no-properties start end)
                   #'skewer-post-minibuffer)))

  (define-key js-mode-map (kbd "C-x C-e") 'skewer-eval-region))

;;----------------------------------------------------------------------------
;;-- init.deft
;;----------------------------------------------------------------------------

(require 'deft)

;; deft: share files with nvALT
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

;;----------------------------------------------------------------------------
;;-- init.eshell
;;----------------------------------------------------------------------------

;;-- init.eshell.multishell

(require 'multishell)
(global-set-key (kbd "C-x m") 'multishell-switch-to-current-or-create)
(global-set-key (kbd "C-x M") 'multishell)
(global-set-key (kbd "s-}") 'multishell-next)
(global-set-key (kbd "s-{") 'multishell-prev)

;;-- init.eshell.prompt
(defun git-dir-branch-string (dir)
  "Returns the git branch of the repo containing dir, or nil if
   dir is not in a git repo."
  (interactive)
  (let* ((git-output (shell-command-to-string
                      (concat "cd " dir " "
                              "&& git symbolic-ref HEAD 2>/dev/null | awk -F / {'print $NF'}")))
         (branch-name (replace-regexp-in-string "\n" "" git-output)))
    (when (> (length git-output) 0)
      (substring git-output 0 -1))))

; ~/.emacs.d/user [master] > command
(setq eshell-prompt-function
  (lambda ()
    (concat
     ;; directory
     (propertize (abbreviate-file-name (eshell/pwd))
                 'face `(:foreground ,(color-theme-color 'blue)))
     ;; git branch information
     (let ((git-branch (git-dir-branch-string ".")))
       (if git-branch
         (propertize (concat " [" git-branch "]")
                     'face `(:foreground ,(color-theme-color 'yellow)))
         ""))
     ;; prompt
     (propertize " >" 'face `(:foreground ,(color-theme-color 'orange)
                              :weight bold))
     (propertize " " 'face 'default))))

;; this unfortunately has to match the output of `eshell-prompt-function`
(setq eshell-prompt-regexp "^[^>]* > ")

;; highlighting the prompt prevents all other prompt styling
(setq eshell-highlight-prompt nil)

;; ignore case during completion
(setq eshell-cmpl-ignore-case t)

;;----------------------------------------------------------------------------
;;-- init.eshell.commands
;;----------------------------------------------------------------------------

(defun eshell/d (&optional dirname switches)
  (if (null dirname)
    (dired "." switches)
    (dired dirname switches)))

(defun eshell/ff (&optional filename wildcards)
  (if (null filename)
    (find-file ".")
    (find-file filename wildcards)))

(defun eshell/vis (program &optional &rest args)
  (apply 'eshell-exec-visual (cons program args)))

;;----------------------------------------------------------------------------
;;-- init.mode-line
;;----------------------------------------------------------------------------
;; From: http://amitp.blogspot.com/2011/08/emacs-custom-mode-line.html

;; Mode line setup
(setq-default
 mode-line-format
 '(; Position, including warning for 80 columns
   (:propertize "%4l:" face mode-line-position-face)
   (:eval (propertize "%3c" 'face
                      (if (>= (current-column) 80)
                          'mode-line-80col-face
                        'mode-line-position-face)))
   ;; emacsclient [default -- keep?]
   mode-line-client
   "  "
   ;; read-only or modified status
   (:eval
    (cond (buffer-read-only
           (propertize " RO " 'face 'mode-line-read-only-face))
          ((buffer-modified-p)
           (propertize " ** " 'face 'mode-line-modified-face))
          (t "    ")))
   "  "
   ;; directory and buffer/file name
   (:propertize (:eval (shorten-directory default-directory 20))
                face mode-line-folder-face)
   (:propertize "%b"
                face mode-line-filename-face)
   ;; narrow [default -- keep?]
   " %n "
   ;; mode indicators: vc, recursive edit, major mode, minor modes, process, global
   (vc-mode vc-mode)
   "  %["
   (:propertize mode-name
                face mode-line-mode-face)
   "%] "
   (:eval (propertize (format-mode-line minor-mode-alist)
                      'face 'mode-line-minor-mode-face))
   (:propertize mode-line-process
                face mode-line-process-face)
   (global-mode-string global-mode-string)
   "    "))

;; Helper function
(defun shorten-directory (dir max-length)
  "Show up to `max-length' characters of a directory name `dir'."
  (let ((path (reverse (split-string (abbreviate-file-name dir) "/")))
        (output ""))
    (when (and path (equal "" (car path)))
      (setq path (cdr path)))
    (while (and path (< (length output) (- max-length 4)))
      (setq output (concat (car path) "/" output))
      (setq path (cdr path)))
    (when path
      (setq output (concat ".../" output)))
    output))

;; Extra mode line faces
(make-face 'mode-line-read-only-face)
(make-face 'mode-line-modified-face)
(make-face 'mode-line-folder-face)
(make-face 'mode-line-filename-face)
(make-face 'mode-line-position-face)
(make-face 'mode-line-mode-face)
(make-face 'mode-line-minor-mode-face)
(make-face 'mode-line-process-face)
(make-face 'mode-line-80col-face)

(defun set-custom-mode-line-face-attrs ()
  (set-face-attribute 'mode-line nil
                      :foreground (color-theme-color 'base2) :background (color-theme-color 'base02)
                      :inverse-video nil
                      :box `(:line-width 4 :color ,(color-theme-color 'base02) :style nil))
  (set-face-attribute 'mode-line-inactive nil
                      :foreground (color-theme-color 'base2) :background (color-theme-color 'base01)
                      :inverse-video nil
                      :box `(:line-width 4 :color ,(color-theme-color 'base01) :style nil))

  (set-face-attribute 'mode-line-read-only-face nil
                      :inherit 'mode-line-face
                      :foreground (color-theme-color 'blue)
                      :box `(:line-width 2 :color (color-theme-color 'blue)))
  (set-face-attribute 'mode-line-modified-face nil
                      :inherit 'mode-line-face
                      :foreground (color-theme-color 'magenta)
                      :background (color-theme-color 'base2)
                      :box `(:line-width 2 :color ,(color-theme-color 'magenta)))
  (set-face-attribute 'mode-line-folder-face nil
                      :inherit 'mode-line-face
                      :foreground (color-theme-color 'base2))
  (set-face-attribute 'mode-line-filename-face nil
                      :inherit 'mode-line-face
                      :foreground (color-theme-color 'blue)
                      :weight 'bold)
  (set-face-attribute 'mode-line-position-face nil
                      :inherit 'mode-line-face
                      :family "Menlo" :height 100)
  (set-face-attribute 'mode-line-mode-face nil
                      :inherit 'mode-line-face
                      :foreground (color-theme-color 'base2))
  (set-face-attribute 'mode-line-minor-mode-face nil
                      :inherit 'mode-line-mode-face
                      :foreground (color-theme-color 'base0)
                      :height 110)
  (set-face-attribute 'mode-line-process-face nil
                      :inherit 'mode-line-face
                      :foreground (color-theme-color 'blue))
  (set-face-attribute 'mode-line-80col-face nil
                      :inherit 'mode-line-position-face
                      :foreground (color-theme-color 'base2) :background (color-theme-color 'magenta)))

;; set mode line on load
(set-custom-mode-line-face-attrs)

;; set everything again if the theme is toggled between light/dark
(defadvice light (after load-mode-line activate)
  (set-custom-mode-line-face-attrs))
(defadvice dark (after load-mode-line activate)
  (set-custom-mode-line-face-attrs))

;;----------------------------------------------------------------------------
;;-- init.proof-general
;;----------------------------------------------------------------------------

(let ((proof-general-el-file "/usr/local/share/emacs/site-lisp/ProofGeneral/generic/proof-site.el"))
  (when (file-exists-p proof-general-el-file)
    (load-file proof-general-el-file)))

;;----------------------------------------------------------------------------
;;-- init.irc
;;----------------------------------------------------------------------------

(defun freenode-password ()
  "Returns password if it exists, or nil."
  (ignore-errors freenode-password))

(setq circe-network-options
      `(("Freenode"
         :nick "zachallaun"
         :channels ("#clojure" "#julia")
         :nickserv-password ,(freenode-password))))

(setq circe-reduce-lurker-spam t)

;; keep trying to reconnect forever
(setq circe-server-max-reconnect-attempts nil)

(after 'circe
  (require 'circe-color-nicks)
  (enable-circe-color-nicks))

;;; init.el ends here
