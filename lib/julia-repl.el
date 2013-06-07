;;
;; julia-repl.el
;;
;; Support for sending regions of Julia code to a REPL
;;

;; Setup
;;
;; Note: This must be executed after julia-mode.el has loaded.
;;
;; Make sure that julia-repl.el is on your load-path.
;;
;;     (load "julia-repl")
;;     (setq julia-basic-repl-path "/path/to/julia/usr/bin/julia-release-basic")

;; Use
;;
;; Start a Julia REPL in the directory of your current buffer.
;;     M-x julia-repl RET
;;
;; Prefix with C-u to pass arguments to julia-release-basic
;;     C-u M-x julia-repl RET args RET
;;
;; Send highlighted region to be evaluated in the REPL (while in julia-mode)
;;     C-c C-c

;; Current limitations
;;
;; - Only one Julia REPL buffer can be open at once.

(defvar julia-basic-repl-path nil)

(defvar jr-buffer-name "*julia-repl*")

(defun jr-confirm-basic-repl-path ()
  (or julia-basic-repl-path
      (progn
        (message (concat "Cannot start REPL: 'julia-basic-repl-path is unset.\n"
                         "Add (setq julia-basic-repl-path "
                         "\"/path/to/julia/usr/bin/julia-release-basic\") "
                         "to your initialization file."))
        nil)))

(defun jr-switch-if-repl-exists ()
  (if (get-buffer jr-buffer-name)
      (progn (switch-to-buffer jr-buffer-name) nil)
    t))

(defun jr-repl-args-or-prompt (repl-args)
  (cond ((stringp repl-args) repl-args)
        (repl-args (read-from-minibuffer "julia-release-basic "))
        (t "")))

(defun jr-window-wider-than-tall-p ()
  (> (window-total-width)
     (* 2.5 (window-total-height))))

(defun jr-smart-split-window ()
  (if (jr-window-wider-than-tall-p)
      (split-window-horizontally)
    (split-window-vertically))
  (other-window 1))

(defun jr-choose-directory- (directory)
  (interactive "DStart REPL in which directory? ")
  directory)

(defun jr-choose-directory ()
  (let ((default-directory (file-name-directory buffer-file-name)))
    (call-interactively 'jr-choose-directory-)))

(defun jr-process-send-string (s)
  (if (get-buffer jr-buffer-name)
      (process-send-string
       jr-buffer-name
       (if (string= "\n" (substring s -1)) s (concat s "\n")))
    (message "Julia REPL not started. M-x julia-repl")))

(defun jr-start-julia-repl (repl-args)
  (let ((default-directory (jr-choose-directory)))
    (jr-smart-split-window)
    (shell jr-buffer-name)
    (jr-process-send-string (concat julia-basic-repl-path
                                    " " repl-args))))

(defun julia-repl (&optional repl-args)
  (interactive "P")
  (and (jr-confirm-basic-repl-path)
       (jr-switch-if-repl-exists)
       (jr-start-julia-repl (jr-repl-args-or-prompt repl-args))))

(defun eval-region-in-julia-repl ()
  (interactive)
  (let ((region (buffer-substring-no-properties (region-beginning)
                                                (region-end))))
    (jr-process-send-string region)))

(require 'julia-mode)
(add-hook 'julia-mode-hook
          '(lambda () (local-set-key (kbd "C-c C-c") 'eval-region-in-julia-repl)))

(provide 'julia-repl)
