;;; my-golang.el --- Golang develop environment configure
;;; Commentary:

;;; Code:
(use-package go-projectile :ensure t)
(use-package go-errcheck :ensure t)
(use-package lsp-go :ensure t)

(use-package go-mode
  :ensure t
  :mode (("\\.go\\'" . go-mode))
  :hook ((before-save . gofmt-before-save))
  :config
  (setq gofmt-command "goimports")
  (use-package company-go
    :ensure t
    :config
    (add-hook 'go-mode-hook
              (lambda()
                (add-to-list (make-local-variable 'company-backends)
                             '(company-go company-files company-yasnippet company-capf company-dabbrev))
                (local-set-key (kbd "C-c c") 'go-errcheck)
                (setq ac-sources '(ac-source-go ac-source-abbrev ac-source-dictionary))
                #'flycheck-mode
                #'lsp-go-enable
                #'company-mode-on
                (company-mode))))
  (use-package go-eldoc
    :ensure t
    :hook
    (go-mode . go-eldoc-setup)
    )
  (use-package go-guru
    :ensure t
    :config
    (add-hook 'go-mode-hook #'go-guru-hl-identifier-mode)
    )
  )

(set-variable 'gofmt-command "goimports")

;; 如果是Mac OS X系统，则需要借助exec-path-from-shell将GOPATH环境变量拷贝到emacs中
;; 具体问题可以参考https://github.com/purcell/exec-path-from-shell
;; 该插件在common目录下已经安装，此处可以直接使用
(when (memq window-system '(mac ns))
  (exec-path-from-shell-copy-env "GOPATH"))

;;;;; auto-complete ;;;;;;
;; 如果要使用company-go，则需要先安装gocode，请参考
;; https://github.com/nsf/gocode
(use-package go-autocomplete :ensure t)
(ac-config-default)

(provide 'my-golang)
;;; my-golang.el ends here
