;;; Custom multi-term-like thing

(defgroup multishell nil
  "Multishell manager")

(defcustom multishell-function '(lambda () (eshell "new"))
  "A quoted function that should generate new shells, returning the new
shell's buffer."
  :group 'multishell)

(defcustom multishell-name "*eshell*"
  "The base name for new buffers created by invoking `multishell-function`."
  :type 'string
  :group 'multishell)

(defvar multishell-buffer-ring ())

(defun new-multishell (&optional &rest args)
  "Apply multishell-function to args, returning the new shell buffer."
  (eval (cons multishell-function args)))

(defun purge-killed-multishell-buffers ()
  "Remove buffers that have been killed from `multishell-buffer-ring`."
  (setq multishell-buffer-ring
        (remove-if-not 'buffer-live-p multishell-buffer-ring)))

(defun switch-to-new-named-multishell (name &optional &rest args)
  "Creates and switches to a new named multishell, returning the buffer."
  (let ((new-buffer (apply 'new-multishell args)))
    (switch-to-buffer new-buffer)
    (rename-buffer name)
    new-buffer))

(defun add-to-multishell-buffer-ring (buffer)
  (setq multishell-buffer-ring
        (append multishell-buffer-ring (list buffer))))

(defun multishell ()
  "Creates a new multishell, adding it to `multishell-buffer-ring`."
  (interactive)
  (let ((new-buffer (switch-to-new-named-multishell
                     (generate-new-buffer-name multishell-name))))
    (add-to-multishell-buffer-ring new-buffer)))

(defvar multishell-last-buffer nil)

(defun switch-to-buffer-or-window (name)
  "Switches to the buffer with name `name`, if possible by switching windows."
  (let ((buffer-window (get-window-with-predicate
                         (lambda (window) (equal (buffer-name (window-buffer window)) name)))))
    (if buffer-window
      (select-window buffer-window)
      (switch-to-buffer name))
    (setq multishell-last-buffer (get-buffer name))))

(defun multishell-current ()
  "Switches to `multishell-last-buffer` if it's set."
  (interactive)
  (if (and multishell-last-buffer (buffer-live-p multishell-last-buffer))
    (switch-to-buffer-or-window (buffer-name multishell-last-buffer))
    (message "No last multishell buffer")))

(defun index-where (pred lst)
  "Returns the index of the first element in `lst` that satisfies `pred`.
If no element satisfies the condition, returns `nil`."
  (flet ((recur (n lst)
           (cond ((null lst) nil)
                 ((funcall pred (car lst)) n)
                 (t (recur (1+ n) (cdr lst))))))
    (recur 0 lst)))

(defun multishell-switch-index (change)
  "Switches to the buffer in `multishell-buffer-ring` found by applying
`change` to the current index. `change` receives the current index and
the length of the ring."
  (let ((current-index (index-where (lambda (b) (equal b (current-buffer)))
                                    multishell-buffer-ring)))
    (when (numberp current-index)
      (let* ((len (length multishell-buffer-ring))
             (next-index (funcall change current-index len)))
        (setq multishell-last-buffer (nth next-index multishell-buffer-ring))
        (multishell-current)))))

(defun multishell-next ()
  "Switches to the next buffer in the multishell buffer ring."
  (interactive)
  (multishell-switch-index
    (lambda (current-index len)
      (if (eq (1+ current-index) len) 0 (1+ current-index)))))

(defun multishell-prev ()
  "Switches to the previous buffer in the multishell buffer ring."
  (interactive)
  (multishell-switch-index
    (lambda (current-index len)
      (if (eq (1- current-index) -1) (1- len) (1- current-index)))))

(defun multishell-switch-to-current-or-create ()
  "Switches to `multishell-last-buffer`, creating a new multishell if it
doesn't already exist."
  (interactive)
  (if (and multishell-last-buffer (buffer-live-p multishell-last-buffer))
    (multishell-current)
    (multishell)))

;; advice kill-buffer to purge killed buffers from the buffer ring
(defadvice kill-buffer (after clean-ring activate)
  (purge-killed-multishell-buffers))

(global-set-key (kbd "C-x m") 'multishell-switch-to-current-or-create)
(global-set-key (kbd "C-x M") 'multishell)
(global-set-key (kbd "s-}") 'multishell-prev)
(global-set-key (kbd "s-{") 'multishell-prev)
