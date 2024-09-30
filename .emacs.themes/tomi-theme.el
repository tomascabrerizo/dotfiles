(deftheme tomi "Created 2024-09-29.")

;; This comment is a test

(let
    (
     (bg-color "#1b1b1b")
     (font-color "#b3b3b3")
     (font-color-light "#d3d3d3")
     (font-color-dark "#939393")
     (select-color "#3b3b3b")
     (comment-color "#777777")
     (extra-color "#c7c7ff"))
  (custom-theme-set-faces
   'tomi
   `(default ((t (:background ,bg-color  :foreground ,font-color))))
   `(cursor ((t (:foreground "#ffffff" :background "#ffffff"))))
   `(highlight ((t (:background ,select-color))))
   `(region ((t (:extend t :background ,select-color))))
   `(font-lock-builtin-face ((t (:weight bold :foreground ,extra-color))))
   `(font-lock-comment-face ((t (:foreground ,comment-color))))
   `(font-lock-constant-face ((t (:foreground ,font-color-dark))))
   `(font-lock-function-name-face ((t (:foreground ,font-color-light :weight bold))))
   `(font-lock-keyword-face ((t (:foreground ,extra-color :weight bold))))
   `(font-lock-preprocessor-face ((t (:foreground ,font-color-dark))))
   `(font-lock-string-face ((t (:foreground ,extra-color))))
   `(font-lock-type-face ((t (:foreground ,font-color-light :weight bold))))
   `(font-lock-variable-name-face ((t (:foreground ,font-color))))))

(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'tomi)
