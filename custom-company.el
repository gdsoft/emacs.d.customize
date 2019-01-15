;;; custom-company.el --- company
;;; Commentary:

;; Company
;; 是 complete anything 的意思
;; 是一種 auto-complete 自動完成
;;; Code:
(use-package company
  :ensure t
  :config
  (global-company-mode t)
  (setq company-idle-delay 0 ; 菜单延迟
      company-minimum-prefix-length 3 ; 开始补全字数0
      company-require-match nil
      company-dabbrev-ignore-case nil
      company-dabbrev-downcase nil
      company-show-numbers t ; 显示序号
      company-transformers '(company-sort-by-backend-importance)
      company-continue-commands '(not helm-dabbrev)
      company-backends '((company-files
                          company-yasnippet
                          company-keywords
                          company-capf)
                         (company-abbrev company-dabbrev))))

(add-hook 'emacs-lisp-mode-hook
            (lambda ()
              (add-to-list
               (make-local-variable 'company-backends)
               '(company-elisp))))

;; 上下选择改为C-p/C-n
(with-eval-after-load 'company
  (define-key company-active-map (kbd "M-n") nil)
  (define-key company-active-map (kbd "M-p") nil)
  (define-key company-active-map (kbd "C-n") #'company-select-next)
  (define-key company-active-map (kbd "C-p") #'company-select-previous))

(advice-add 'company-complete-common
            :before (lambda () (setq my-company-point (point))))
(advice-add 'company-complete-common
            :after (lambda () (when (equal my-company-point (point))
                                (yas-expand))))

(provide 'custom-company)
;;; custom-company.el ends here
