;;; custom.el --- user customization file    -*- no-byte-compile: t -*-
;;; Commentary:
;;;       Copy custom-template.el to custom.el and change the configurations, then restart Emacs.
;;;       Put your own configurations in custom-post.el to override default configurations.
;;; Code:

(setq centaur-logo nil)                             ; Logo file or nil (official logo)
(setq centaur-full-name "Guo Dong")                 ; User full name
(setq centaur-mail-address "guodongsoft@163.com")   ; Email address
;; (setq centaur-proxy "127.0.0.1:1080")          ; Network proxy
;; (setq centaur-package-archives 'emacs-china)   ; Package repo: melpa, melpa-mirror, emacs-china netease or tuna
;; (setq centaur-theme 'classic)                  ; Color theme: default, classic, doom, dark, light or daylight
;; (setq centaur-dashboard nil)                   ; Use dashboard at startup or not: t or nil
;; (setq centaur-lsp nil)                         ; Set LSP client: lsp-mode, eglot or nil
;; (setq centaur-ivy-icon nil)                    ; Display icons in ivy or not: t or nil
;; (setq centaur-benchmark t)                     ; Enable initialization benchmark or not: t or nil

;; For Emacs devel
;; (setq package-user-dir (locate-user-emacs-file (format "elpa-%s" emacs-major-version)))
;; (setq desktop-base-file-name (format ".emacs-%s.desktop" emacs-major-version))
;; (setq desktop-base-lock-name (format ".emacs-%s.desktop.lock" emacs-major-version))

;; Fonts
(when (display-graphic-p)
  ;; Set default fonts
  (cond
   ((member "Source Code Pro" (font-family-list))
    (set-face-attribute 'default nil :font "Source Code Pro"))
   ((member "Menlo" (font-family-list))
    (set-face-attribute 'default nil :font "Menlo"))
   ((member "Monaco" (font-family-list))
    (set-face-attribute 'default nil :font "Monaco"))
   ((member "DejaVu Sans Mono" (font-family-list))
    (set-face-attribute 'default nil :font "DejaVu Sans Mono"))
   ((member "Consolas" (font-family-list))
    (set-face-attribute 'default nil :font "Consolas")))

  (cond
   (sys/mac-x-p
    (set-face-attribute 'default nil :height 130))
   (sys/win32p
    (set-face-attribute 'default nil :height 110)))

  ;; Specify fonts for all unicode characters
  (cond
   ((member "Apple Color Emoji" (font-family-list))
    (set-fontset-font t 'unicode "Apple Color Emoji" nil 'prepend))
   ((member "Symbola" (font-family-list))
    (set-fontset-font t 'unicode "Symbola" nil 'prepend)))

  ;; Specify fonts for Chinese characters
  (cond
   ((member "WenQuanYi Micro Hei" (font-family-list))
    (set-fontset-font t '(#x4e00 . #x9fff) "WenQuanYi Micro Hei"))
   ((member "Microsoft Yahei" (font-family-list))
    (set-fontset-font t '(#x4e00 . #x9fff) "Microsoft Yahei")))
  )

;; Misc.
;; (setq confirm-kill-emacs 'y-or-n-p)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; 多行注释
(defun my-comment-or-uncomment-region (beg end &optional arg)
  "Comment or uncomment region (BEG END ARG)."
  (interactive (if (use-region-p)
                   (list (region-beginning) (region-end) nil)
                 (list (line-beginning-position)
                       (line-beginning-position 2))))
  (comment-or-uncomment-region beg end arg))
(global-set-key [remap comment-or-uncomment-region] 'my-comment-or-uncomment-region)
(global-set-key (kbd "C-c ;") 'comment-or-uncomment-region)

;; （Y or N）
(fset 'yes-or-no-p 'y-or-n-p)

(defvar load-path-list)
(setq load-path-list
      '(
        "~/.emacs.d.customize/customs"
        "~/.emacs.d.customize/language"
        "~/.emacs.d.customize/download"
        "~/.emacs.d.customize/download/common"
        "~/.emacs.d.customize/download/awesome"
        ))
(setq load-path (append load-path load-path-list))

(add-hook 'prog-mode-hook
          (lambda()
            (display-line-numbers-mode -1)
            (require 'custom-pair)))

;;; custom.el ends here
