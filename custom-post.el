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

(add-hook 'prog-mode-hook
          (lambda()
            ;; Sly
            (require 'sly-autoloads)))

(let ((require-list '(
                      install-elisp
                      custom-evil
                      custom-encoding
                      custom-tab
                      custom-shell
                                        ;       custom-rust
                      custom-pair
                      custom-rails
                      )))
  (dolist (req require-list) (require req)))

;;; custom-post.el ends here
