;;; custom-post.el --- user customization file    -*- no-byte-compile: t -*-
;;; Commentary:
;;;       Copy custom-template.el to custom.el and change the configurations, then restart Emacs.
;;;       Put your own configurations in custom-post.el to override default configurations.
;;; Code:

;; Treemacs
(with-eval-after-load 'treemacs
  (treemacs-reset-icons) ; Treemacs custom icons
  (evil-define-key 'normal treemacs-mode-map (kbd "o") 'treemacs-RET-action)
  (evil-define-key 'normal treemacs-mode-map (kbd "F") 'treemacs-create-file)
  (evil-define-key 'normal treemacs-mode-map (kbd "+") 'treemacs-create-dir))

;; Evil
(use-package evil :ensure t)
(evil-mode t)

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
  (require'evil-magit))

;;; custom-post.el ends here
