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

(load "color")

(defun set-custom-mode-line-face-attrs ()
  (set-face-attribute 'mode-line nil
                      :foreground (solarized-color 'base2) :background (solarized-color 'base02)
                      :inverse-video nil
                      :box `(:line-width 4 :color ,(solarized-color 'base02) :style nil))
  (set-face-attribute 'mode-line-inactive nil
                      :foreground (solarized-color 'base2) :background (solarized-color 'base01)
                      :inverse-video nil
                      :box `(:line-width 4 :color ,(solarized-color 'base01) :style nil))

  (set-face-attribute 'mode-line-read-only-face nil
                      :inherit 'mode-line-face
                      :foreground (solarized-color 'blue)
                      :box `(:line-width 2 :color (solarized-color 'blue)))
  (set-face-attribute 'mode-line-modified-face nil
                      :inherit 'mode-line-face
                      :foreground (solarized-color 'magenta)
                      :background (solarized-color 'base2)
                      :box `(:line-width 2 :color ,(solarized-color 'magenta)))
  (set-face-attribute 'mode-line-folder-face nil
                      :inherit 'mode-line-face
                      :foreground (solarized-color 'base2))
  (set-face-attribute 'mode-line-filename-face nil
                      :inherit 'mode-line-face
                      :foreground (solarized-color 'blue)
                      :weight 'bold)
  (set-face-attribute 'mode-line-position-face nil
                      :inherit 'mode-line-face
                      :family "Menlo" :height 100)
  (set-face-attribute 'mode-line-mode-face nil
                      :inherit 'mode-line-face
                      :foreground (solarized-color 'base2))
  (set-face-attribute 'mode-line-minor-mode-face nil
                      :inherit 'mode-line-mode-face
                      :foreground (solarized-color 'base0)
                      :height 110)
  (set-face-attribute 'mode-line-process-face nil
                      :inherit 'mode-line-face
                      :foreground (solarized-color 'blue))
  (set-face-attribute 'mode-line-80col-face nil
                      :inherit 'mode-line-position-face
                      :foreground (solarized-color 'base2) :background (solarized-color 'magenta)))

;; set mode line on load
(set-custom-mode-line-face-attrs)

;; set everything again if the theme is toggled between light/dark
(defadvice color-theme-solarized-light (after load-mode-line activate)
  (set-custom-mode-line-face-attrs))
(defadvice color-theme-solarized-dark (after load-mode-line activate)
  (set-custom-mode-line-face-attrs))
