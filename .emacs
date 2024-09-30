(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; Custom Theme
(load-file "./.emacs.themes/tomi-theme.el")
(load-theme 'tomi t)

;; Custom functions

(defun tomi-require(package)
  (when (not (package-installed-p package))
    (package-install package)))

(defun tomi-switch-to-next-buffer ()
  (interactive)
  (let (( bread-crumb (buffer-name) ))
    (next-buffer)
    (while
        (and
         (string-match-p "^\*" (buffer-name))
         (not ( equal bread-crumb (buffer-name) )) )
      (next-buffer))))

(defun tomi-switch-to-prev-buffer ()
  (interactive)
  (let (( bread-crumb (buffer-name) ))
    (previous-buffer)
    (while
        (and
         (string-match-p "^\*" (buffer-name))
         (not ( equal bread-crumb (buffer-name) )) )
      (previous-buffer))))

(defun tomi-duplicate-line ()
  (interactive)
  (let ((saved-column (current-column)))
    (move-beginning-of-line nil)
    (set-mark-command nil)
    (move-end-of-line nil)
    (kill-ring-save (mark) (point))
    (newline)
    (yank)
    (move-to-column saved-column)))

(defun tomi-last-line-check()
  (interactive)
  (let ((saved-column (current-column)))
    (move-end-of-line nil)
    (setq result (eobp))
    (move-to-column saved-column)
    result))

(defun tomi-delete-line()
  (interactive)
  (if (tomi-last-line-check)
      (progn
	(move-beginning-of-line nil)
	(move-end-of-line nil)
	(delete-region (mark) (point)))
    (progn
      (let ((saved-column (current-column)))
	(move-beginning-of-line nil)
	(set-mark-command nil)
	(next-line)
	(delete-region (mark) (point))
	(move-to-column saved-column)))))

;; Default configuration

(setq inhibit-startup-screen t)
(setq make-backup-files nil)

(tool-bar-mode 0)
(menu-bar-mode 0)
(global-display-line-numbers-mode 1)
(scroll-bar-mode 0)
(column-number-mode 1)
(show-paren-mode 1)
(set-frame-font "Liberation Mono-14" nil t)
(delete-selection-mode 1)
(fido-mode 1)

;; Custom packages configuration

(tomi-require 'zenburn-theme)
(tomi-require 'tao-theme)
(tomi-require 'nord-theme)
(tomi-require 'typescript-mode)
(tomi-require 'json-mode)
(tomi-require 'undo-fu)
(tomi-require 'company)
(tomi-require 'lsp-mode)

(add-hook 'typescript-mode-hook
          (lambda ()
            (lsp)
            (setq tab-width 4)
            (setq indent-tabs-mode nil)))

(add-hook 'html-mode-hook
	  (lambda ()
	    (company-mode)
	    (setq tab-width 2)))

(add-hook 'css-mode-hook
	  (lambda ()
	    (company-mode)
	    (setq tab-width 2)))

(setq lsp-log-io nil)
(setq lsp-modeline-code-actions-enable nil)
(setq lsp-headerline-breadcrumb-enable nil)
(setq lsp-clients-typescript-prefer-use-project-ts-server t)

(setq lsp-typescript-format-enable nil)

(setq company-minimum-prefix-length 1
     company-idle-delay 0.0)

(with-eval-after-load 'company
  (define-key company-active-map (kbd "TAB") #'company-complete-selection)
  (define-key company-active-map (kbd "<tab>") #'company-complete-selection))

;; Custom keybinding

(global-set-key (kbd "M-,") 'tomi-duplicate-line)
(global-set-key (kbd "C-d") 'tomi-delete-line)
(global-set-key (kbd "M-<home>") 'back-to-indentation)
(global-set-key (kbd "M-<end>") 'move-end-of-line)

(global-set-key (kbd "C-x C-<right>") 'tomi-switch-to-next-buffer)
(global-set-key (kbd "C-x C-<left>") 'tomi-switch-to-prev-buffer)

(global-set-key (kbd "C-u") 'undo-fu-only-undo)
(global-set-key (kbd "C-r") 'undo-fu-only-redo)

(global-set-key (kbd "M-n") 'next-error)
(global-set-key (kbd "M-p") 'previous-error)
(global-set-key (kbd "M-N") 'flymake-goto-next-error)
(global-set-key (kbd "M-P") 'flymake-goto-prev-error)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("98b4ef49c451350c28a8c20c35c4d2def5d0b8e5abbc962da498c423598a1cdd" "09b833239444ac3230f591e35e3c28a4d78f1556b107bafe0eb32b5977204d93" default))
 '(package-selected-packages
   '(nord-theme tao-theme lsp-mode company undo-fu json-mode typescript-mode zenburn-theme)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
