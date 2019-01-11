;;; custom-post.el --- user customization file    -*- no-byte-compile: t -*-
;;; Commentary:
;;;       Copy custom-template.el to custom.el and change the configurations, then restart Emacs.
;;;       Put your own configurations in custom-post.el to override default configurations.
;;; Code:

;; Evil
(use-package evil :ensure t)
(evil-mode t)

;; Treemacs custom icons
(with-eval-after-load 'treemacs
 (treemacs-reset-icons))

;;; custom-post.el ends here
