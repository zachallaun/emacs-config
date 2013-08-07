(define-clojure-indent
  ;; midje
  (fact 'defun)

  ;; core.logic
  (run* 'defun)
  (run 'defun)
  (fresh 'defun)
  (defne 'defun)
  (project 'defun)

  ;; core.async
  (go 'defun))
