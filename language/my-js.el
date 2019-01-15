;;; my-js.el --- Javascript 
;;; Commentary:

(use-package js-comint)

;; --- js ---
(use-package js2-mode
  :ensure t
  :bind (("C-c ]" . tide-jump-to-definition)
         ("C-c t" . tide-jump-back))
  :mode (("\\.js\\'" . js2-mode)
         ("\\.jsx\\'" . js2-jsx-mode)
         ("\\.json\\'" . javascript-mode))
  :init
  (setq js-indent-level 2)
  (setq-default js2-basic-offset 2)
  (setq-default js2-global-externs '("module" "require" "assert" "setTimeout" "clearTimeout" "setInterval" "clearInterval" "location" "__d\
irname" "console" "JSON")))

;; --- typescript ---
(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode t)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode t)
  (tide-hl-identifier-mode t)
  ;; company is an optional dependency. You have to
  ;; install it separately via package-install
  ;; `M-x package-install [ret] company`
  (add-to-list (make-local-variable 'company-backends)
               'company-files)
  (company-mode t))

(add-hook 'js2-mode-hook #'setup-tide-mode)
(add-hook 'css-mode-hook  'company-mode)

(use-package tide
  :ensure t
  :after
  (typescript-mode company flycheck)
  :hook
  ((typescript-mode . tide-setup)
   (typescript-mode . tide-hl-identifier-mode)
   (before-save . tide-format-before-save))
  :config
  (setq tide-completion-enable-autoimport-suggestions t))

;; --- prettier ---
(use-package prettier-js
  :ensure t
  :hook
  ((js2-mode . prettier-js-mode))
  :config
  (setq prettier-js-args
        '(
          "--trailing-comma" "none"
          "--bracket-spacing" "false"
          )))

;; --- eslint ---
;; デフォルトではJSHintの優先度が高くなっているようなので、JSHintを無効にする
(eval-after-load 'flycheck
  '(custom-set-variables
    '(flycheck-disabled-checkers '(javascript-jshint javascript-jscs))
    ))
;; 上記記事にあったjs2-modeで競合してしまう機能についてもオフにしておく
(setq js2-include-browser-externs nil)
(setq js2-mode-show-parse-errors nil)
(setq js2-mode-show-strict-warnings nil)
(setq js2-highlight-external-variables nil)
(setq js2-include-jslint-globals nil)

(defun eslint-fix-file ()
  (interactive)
  (message "eslint --fixing the file" (buffer-file-name))
  (shell-command (concat "eslint --fix " (buffer-file-name))))

(defun eslint-fix-file-and-revert ()
  (interactive)
  (eslint-fix-file)
  (revert-buffer t t))

(add-hook 'js2-mode-hook
          (lambda ()
            (add-hook 'after-save-hook #'eslint-fix-file-and-revert)))

(add-hook 'js-mode-hook 'js2-minor-mode)
(add-hook 'web-mode-hook
          (lambda ()
            (when (equal web-mode-content-type "jsx")
              (flycheck-add-mode 'javascript-eslint 'web-mode)
              (flycheck-mode))))

(provide 'my-js)
;;; my-js.el ends here
