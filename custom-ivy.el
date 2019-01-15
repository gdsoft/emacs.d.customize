;;; custom-ivy.el --- Ivy mode
;;; Commentary:

;; Ivy
;;; Code:
(use-package ivy
  :ensure t
  :diminish (ivy-mode . "")
  :config
  (ivy-mode 1)
  (setq ivy-use-virutal-buffers t)
  (setq enable-recursive-minibuffers t)
  (setq ivy-height 10)
  (setq ivy-initial-inputs-alist nil)
  (setq ivy-count-format "%d/%d ")
  (setq ivy-re-builders-alist
        `((t . ivy--regex-ignore-order))))

;; Counsel
(use-package counsel
  :ensure t
  :bind (("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file)))

;; Swiper
(use-package swiper
  :ensure t
  :bind (("C-s" . swiper)))

(provide 'custom-ivy)
;;; custom-ivy.el ends here
