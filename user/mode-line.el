;; From: http://amitp.blogspot.com/2011/08/emacs-custom-mode-line.html

;; Mode line setup
(setq-default
 mode-line-format
 '(; Position, including warning for 80 columns
   (:propertize "%4l:" face mode-line-position-face)
   (:eval (propertize "%2c" 'face
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

;; taken from solarized-definitions.el `solarized-color-definitions`
(require 'color-theme)
(require 'color-theme-solarized)
(flet ((color (name)
           (let* ((index (if window-system
                             (if solarized-degrade
                                 3
                               (if solarized-broken-srgb 2 1))
                           (case (display-color-cells)
                             (16 4)
                             (8  5)
                             (otherwise 3)))))
             (nth index (assoc name solarized-colors)))))

  (set-face-attribute 'mode-line nil
                      :foreground (color 'base2) :background (color 'base02)
                      :inverse-video nil
                      :box `(:line-width 4 :color ,(color 'base02) :style nil))
  (set-face-attribute 'mode-line-inactive nil
                      :foreground (color 'base2) :background (color 'base01)
                      :inverse-video nil
                      :box `(:line-width 4 :color ,(color 'base01) :style nil))

  (set-face-attribute 'mode-line-read-only-face nil
                      :inherit 'mode-line-face
                      :foreground (color 'blue)
                      :box `(:line-width 2 :color (color 'blue)))
  (set-face-attribute 'mode-line-modified-face nil
                      :inherit 'mode-line-face
                      :foreground (color 'magenta)
                      :background (color 'base2)
                      :box `(:line-width 2 :color ,(color 'magenta)))
  (set-face-attribute 'mode-line-folder-face nil
                      :inherit 'mode-line-face
                      :foreground (color 'base2))
  (set-face-attribute 'mode-line-filename-face nil
                      :inherit 'mode-line-face
                      :foreground (color 'blue)
                      :weight 'bold)
  (set-face-attribute 'mode-line-position-face nil
                      :inherit 'mode-line-face
                      :family "Menlo" :height 100)
  (set-face-attribute 'mode-line-mode-face nil
                      :inherit 'mode-line-face
                      :foreground (color 'base2))
  (set-face-attribute 'mode-line-minor-mode-face nil
                      :inherit 'mode-line-mode-face
                      :foreground (color 'base0)
                      :height 110)
  (set-face-attribute 'mode-line-process-face nil
                      :inherit 'mode-line-face
                      :foreground (color 'blue))
  (set-face-attribute 'mode-line-80col-face nil
                      :inherit 'mode-line-position-face
                      :foreground (color 'base2) :background (color 'magenta)))
