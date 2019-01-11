;;; custom-post.el --- user customization file    -*- no-byte-compile: t -*-
;;; Commentary:
;;;       Copy custom-template.el to custom.el and change the configurations, then restart Emacs.
;;;       Put your own configurations in custom-post.el to override default configurations.
;;; Code:

;; Evil
(use-package evil :ensure t)
(evil-mode t)

;; Treemacs
(with-eval-after-load 'treemacs
 (treemacs-reset-icons) ; Treemacs custom icons
 (evil-define-key 'normal treemacs-mode-map (kbd "o") 'treemacs-RET-action)
 (evil-define-key 'normal treemacs-mode-map (kbd "F") 'treemacs-create-file)
 (evil-define-key 'normal treemacs-mode-map (kbd "+") 'treemacs-create-dir))

;;; custom-post.el ends here
