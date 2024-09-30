(deftheme tomi "Created 2024-09-29.")

;; This comment is a test

(let
    (
     (bg-color "#101010")
     (font-color "#bbbbbb")
     (font-color-light "#eeeeee")
     (font-color-dark "#777777yy")
     (select-color "#303030")
     (comment-color "#777777")
     (extra-color "#c7c7ff")
     (red "#cb0000")
     (green "#00aa00")
     (blue "#0000aa")
     (yellow "#aaaa00"))
  
  (custom-theme-set-faces
   'tomi
   `(default ((t (:background ,bg-color  :foreground ,font-color))))
   `(cursor ((t (:foreground "#ffffff" :background "#ffffff"))))
   `(highlight ((t (:background ,select-color :underline nil))))
   `(region ((t (:extend t :background ,select-color))))
   `(font-lock-builtin-face ((t (:weight bold :foreground ,extra-color))))
   `(font-lock-comment-face ((t (:foreground ,comment-color))))
   `(font-lock-constant-face ((t (:foreground ,font-color-light))))
   `(font-lock-function-name-face ((t (:foreground ,font-color-light :weight bold))))
   `(font-lock-keyword-face ((t (:foreground ,extra-color :weight bold))))
   `(font-lock-preprocessor-face ((t (:foreground ,font-color-dark))))
   `(font-lock-string-face ((t (:foreground ,extra-color))))
   `(font-lock-type-face ((t (:foreground ,font-color-light :weight bold))))
   `(font-lock-variable-name-face ((t (:foreground ,font-color))))
   `(font-lock-doc-face ((t (:foreground ,extra-color))))
   `(font-lock-doc-string-face ((t (:foreground ,extra-color))))
   `(link ((t (:foreground ,extra-color :underline t))))
   `(link-visited ((t (:foreground ,font-color-dark :underline t))))

   ;; Compilation
   `(compilation-info ((t ,(list :foreground green
                                 :inherit 'unspecified))))
   `(compilation-warning ((t ,(list :foreground yellow
                                    :bold t
                                    :inherit 'unspecified))))
   `(compilation-error ((t (:foreground ,red))))
   `(compilation-mode-line-fail ((t ,(list :foreground red
                                           :weight 'bold
                                           :inherit 'unspecified))))
   `(compilation-mode-line-exit ((t ,(list :foreground green
                                           :weight 'bold
                                           :inherit 'unspecified))))

   `(mode-line ((t ,(list :background "#444444"
                          :foreground "#eeeeee"))))
   `(mode-line-buffer-id ((t ,(list :background "#444444"
                                    :foreground "#eeeeee"))))
   `(mode-line-inactive ((t ,(list :background "#444444"
                                   :foreground "#999999"))))
   ))

(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'tomi)
