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
