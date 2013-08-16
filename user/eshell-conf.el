
;; highlighting the prompt prevents all other prompt styling
(setq eshell-highlight-prompt nil)

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

; ~/.emacs.d/user [master]
; > command
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

(defun eshell/d (&optional dirname switches)
  (if (null dirname)
    (dired "." switches)
    (dired dirname switches)))