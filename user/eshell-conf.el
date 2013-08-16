
;;; Custom multi-term-like thing

(defgroup multishell nil
  "Multishell manager")

(defcustom multishell-function '(lambda () (eshell "new"))
  "A quoted function that should generate new shells, returning the new
shell's buffer."
  :group 'multishell)

(defcustom multishell-name eshell-buffer-name
  "The base name for new buffers created by invoking `multishell-function`."
  :type 'string
  :group 'multishell)

(defvar multishell-buffer-ring ())

(defvar multishell-current-buffer nil)

(defun new-multishell (&optional &rest args)
  "Apply multishell-function to args, returning the new shell buffer."
  (eval (cons multishell-function args)))

(defun switch-to-new-named-multishell (name &optional &rest args)
  "Creates and switches to a new named multishell, returning the buffer."
  (let ((new-buffer (apply 'new-multishell args)))
    (switch-to-buffer new-buffer)
    (rename-buffer name)
    new-buffer))

(defun purge-killed-multishell-buffers ()
  "Remove buffers that have been killed from `multishell-buffer-ring`.
If `multishell-current-buffer` was killed, set it to be the next buffer
in the ring."
  (flet ((remove-and-set-current (lst set?)
           (when (not (null lst))
             (let ((a (car lst))
                   (d (cdr lst)))
               (cond ((buffer-live-p a)
                      (if set?
                        (progn
                          (setq multishell-current-buffer a)
                          (cons a (remove-and-set-current d nil)))
                        (cons a (remove-and-set-current d nil))))
                     ((equal a multishell-current-buffer)
                      (progn
                        (setq multishell-current-buffer nil)
                        (remove-and-set-current d t)))
                     (t (remove-and-set-current d set?)))))))
    (setq multishell-buffer-ring
          (remove-and-set-current multishell-buffer-ring nil))))

(defun add-to-multishell-buffer-ring (buffer)
  (setq multishell-buffer-ring
        (append multishell-buffer-ring (list buffer))))

(defun multishell ()
  "Creates a new multishell, adding it to `multishell-buffer-ring` and setting
it as the `multishell-current-buffer`."
  (interactive)
  (let ((new-buffer (switch-to-new-named-multishell
                     (generate-new-buffer-name multishell-name))))
    (purge-killed-multishell-buffers)
    (add-to-multishell-buffer-ring new-buffer)
    (setq multishell-current-buffer new-buffer)))

(defun switch-to-buffer-or-window (name)
  "Switches to the buffer with name `name`, if possible by switching windows."
  (let ((buffer-window (get-window-with-predicate
                         (lambda (window) (equal (buffer-name (window-buffer window)) name)))))
    (if buffer-window
      (select-window buffer-window)
      (switch-to-buffer name))))

(defun multishell-current ()
  "Switches to `multishell-current-buffer` if it's set."
  (interactive)
  (purge-killed-multishell-buffers)
  (if (and multishell-current-buffer (buffer-live-p multishell-current-buffer))
    (switch-to-buffer-or-window (buffer-name multishell-current-buffer))
    (message "No live multishell buffer")))

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
  (purge-killed-multishell-buffers)
  (let ((current-index (index-where (lambda (b) (equal b multishell-current-buffer))
                                    multishell-buffer-ring)))
    (when (numberp current-index)
      (let* ((len (length multishell-buffer-ring))
             (next-index (funcall change current-index len)))
        (setq multishell-current-buffer (nth next-index multishell-buffer-ring))
        (multishell-current)))))

(defun multishell-next ()
  "Switches to the buffer after `multishell-current-buffer` in the
`multishell-buffer-ring`, setting a new current buffer."
  (interactive)
  (purge-killed-multishell-buffers)
  (multishell-switch-index
    (lambda (current-index len)
      (if (eq (1+ current-index) len) 0 (1+ current-index)))))

(defun multishell-prev ()
  "Switches to the buffer before `multishell-current-buffer` in the
`multishell-buffer-ring`, setting a new current buffer."
  (interactive)
  (purge-killed-multishell-buffers)
  (multishell-switch-index
    (lambda (current-index len)
      (if (eq (1- current-index) -1) (1- len) (1- current-index)))))

(defun multishell-switch-to-current-or-create ()
  "Switches to `multishell-current-buffer`, creating a new multishell if it
doesn't already exist."
  (interactive)
  (if (and multishell-current-buffer (buffer-live-p multishell-current-buffer))
    (multishell-current)
    (multishell)))

(global-set-key (kbd "C-x m") 'multishell-switch-to-current-or-create)
(global-set-key (kbd "C-x M") 'multishell)
(global-set-key (kbd "s-{") 'multishell-prev)
(global-set-key (kbd "s-}") 'multishell-next)

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

(load "color")

; ~/.emacs.d/user [master] > command
(setq eshell-prompt-function
  (lambda ()
    (concat
     ;; directory
     (propertize (abbreviate-file-name (eshell/pwd))
                 'face `(:foreground ,(solarized-color 'blue)))
     ;; git branch information
     (let ((git-branch (git-dir-branch-string ".")))
       (if git-branch
         (propertize (concat " [" git-branch "]")
                     'face `(:foreground ,(solarized-color 'yellow)))
         ""))
     ;;
     (propertize " >" 'face `(:foreground ,(solarized-color 'orange)
                              :weight bold))
     (propertize " " 'face 'default))))

(setq eshell-prompt-regexp "^[^>]* > ")

;; highlighting the prompt prevents all other prompt styling
(setq eshell-highlight-prompt nil)

;; ignore case during completion
(setq eshell-cmpl-ignore-case t)

(defun eshell/d (&optional dirname switches)
  (if (null dirname)
    (dired "." switches)
    (dired dirname switches)))

(defun eshell/ff (&optional filename wildcards)
  (if (null filename)
    (find-file ".")
    (find-file filename wildcards)))
