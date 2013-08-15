(require 'clojure-mode)

(define-clojure-indent
  ;; midje
  (fact 'defun)
  (facts 'defun)

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
  )

(defun nrepl-send-dwim ()
  "Send the appropriate forms to the REPL to be evaluated."
  (interactive)
  (let ((expr (nrepl-last-expression)))
    (pop-to-buffer (nrepl-find-or-create-repl-buffer))
    (goto-char (point-max))
    (insert expr)
    (nrepl-return)
    (other-window 1)))

(eval-after-load "nrepl"
  '(define-key nrepl-interaction-mode-map (kbd "C-c C-c") 'nrepl-send-dwim))
