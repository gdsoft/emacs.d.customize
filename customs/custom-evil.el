;;; custom-evil.el --- user customization file    -*- no-byte-compile: t -*-
;;; Commentary:

;;; Code:

;; Evil
(use-package evil
  :ensure t
  :init
  (setq evil-want-integration t) ;; This is optional since it's already set to t by default.
  (setq evil-want-keybinding nil))
(evil-mode t)

;; Magit
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

;; Treemacs
(with-eval-after-load 'treemacs
  (treemacs-reset-icons) ; Treemacs custom icons
  (evil-define-key 'normal treemacs-mode-map (kbd "o") 'treemacs-RET-action)
  (evil-define-key 'normal treemacs-mode-map (kbd "F") 'treemacs-create-file)
  (evil-define-key 'normal treemacs-mode-map (kbd "+") 'treemacs-create-dir))

;; Dired
(with-eval-after-load 'dired
  (evil-define-key 'normal dired-mode-map (kbd "RET") 'dired-find-file)
  (evil-define-key 'normal dired-mode-map (kbd "m") 'dired-mark)
  (evil-define-key 'normal dired-mode-map (kbd "u") 'dired-unmark)
  (evil-define-key 'normal dired-mode-map (kbd "d") 'dired-do-delete)
  (evil-define-key 'normal dired-mode-map (kbd "R") 'dired-do-rename))

;; Edebug
(evil-set-initial-state 'edebug-mode 'normal)
(add-hook 'edebug-mode-hook #'evil-normalize-keymaps)

;; Evil-collection
(use-package evil-collection
  :after evil
  :ensure t
  :config
  (with-eval-after-load 'ibuffer (evil-collection-init 'ibuffer)))

(provide 'custom-evil)
;;; custom-evil.el ends here
