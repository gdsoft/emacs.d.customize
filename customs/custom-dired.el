;;; custom-dired.el --- Dired's initialization file
;;; Commentary:

;;; Dired Jump
;; C-x C-j
(require 'dired-x)

;;; Code:
;; 设置默认递归删除
(setq dired-recursive-deletes 'always)
(setq dired-recursive-copies 'always)

;; 重用唯一的一个缓冲区作为 Dired Mode 显示专用缓冲区
(put 'dired-find-alternate-file 'disabled nil)

;; 主动加载 Dired Mode
;; (require 'dired)
;; (defined-key dired-mode-map (kbd "RET") 'dired-find-alternate-file)

;; 延迟加载
(with-eval-after-load 'dired
    (define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file)
    (define-key dired-mode-map (kbd "G") 'evil-goto-line)
    (define-key dired-mode-map (kbd "\\") 'dired-do-chgrp))

(provide 'custom-dired)
;;; custom-dired.el ends here
