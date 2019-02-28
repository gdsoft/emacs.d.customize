;;; custom-post.el --- user customization file    -*- no-byte-compile: t -*-
;;; Commentary:
;;;       Copy custom-template.el to custom.el and change the configurations, then restart Emacs.
;;;       Put your own configurations in custom-post.el to override default configurations.
;;; Code:

;; 使用f7键打开内容搜索
(global-set-key [f7] 'projectile-ag)

;; 显示行号
(dolist (hook '(prog-mode-hook text-mode-hook treemacs-mode-hook))
  (add-hook hook #'display-line-numbers-mode))

;; gofmt whitespace => tab
(add-hook 'go-mode-hook
          (lambda ()
            (setq whitespace-style '(face empty trailing lines-tail))
            (setq tab-width 4)
            (setq indent-tabs-mode t)))

(require 'sly-autoloads)
(global-set-key [f10] 'sly)

(let ((require-list '(
                      install-elisp
                      custom-evil
                      custom-encoding
                      custom-tab
                      custom-shell
                                        ; custom-rust
                      custom-pair
                                        ; custom-edit
                      custom-rails
                      )))
  (dolist (req require-list) (require req)))

;;; custom-post.el ends here
