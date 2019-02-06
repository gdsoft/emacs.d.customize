;;; custom-post.el --- user customization file    -*- no-byte-compile: t -*-
;;; Commentary:
;;;       Copy custom-template.el to custom.el and change the configurations, then restart Emacs.
;;;       Put your own configurations in custom-post.el to override default configurations.
;;; Code:

;; 使用f7键打开内容搜索
(global-set-key [f7] 'projectile-ag)

;; 显示行号
(add-to-list 'auto-mode-alist '("\\.\\(md\\|org\\)\\'" . display-line-numbers-mode))

;; Treemacs
(with-eval-after-load 'treemacs
  (treemacs-reset-icons) ; Treemacs custom icons
  (evil-define-key 'normal treemacs-mode-map (kbd "o") 'treemacs-RET-action)
  (evil-define-key 'normal treemacs-mode-map (kbd "F") 'treemacs-create-file)
  (evil-define-key 'normal treemacs-mode-map (kbd "+") 'treemacs-create-dir))

;; Evil
(use-package evil
  :ensure t
  :init
  (setq evil-want-integration t) ;; This is optional since it's already set to t by default.
  (setq evil-want-keybinding nil))
(evil-mode t)

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (with-eval-after-load 'ibuffer (evil-collection-init 'ibuffer))
  (evil-collection-init 'dired))

(evil-set-initial-state 'edebug-mode 'normal)
(add-hook 'edebug-mode-hook #'evil-normalize-keymaps)

(use-package evil-magit
  :ensure t
  :defer t
  :after evil magit
  :init
  ;; optional: this is the evil state that evil-magit will use
  (setq evil-magit-state 'normal)
  ;; optional: disable additional bindings for yanking text
  (setq evil-magit-use-y-for-yank t)
  :config
  (evil-magit-revert))

(with-eval-after-load 'magit
  (require 'evil-magit))

(add-hook 'prog-mode-hook
          (lambda()
            ;; Sly
            (require 'sly-autoloads)))

(let ((require-list '(
                      install-elisp
                      custom-encoding
                      custom-tab
                      custom-shell
                                        ;       custom-rust
                      custom-pair
                      custom-rails
                      )))
  (dolist (req require-list) (require req)))

;;; custom-post.el ends here
