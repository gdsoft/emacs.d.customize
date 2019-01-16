;;; custom-language.el --- user customization file    -*- no-byte-compile: t -*-
;;; Commentary:

;;; Code:

;; Language Server Protocol
(use-package lsp-mode :ensure t)
(use-package company-lsp :ensure t)

;; rainbow-delimiters を使うための設定
(use-package rainbow-delimiters :ensure t)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;; 括弧の色を強調する設定
(use-package cl-lib :ensure t)
(use-package color :ensure t)

(defun rainbow-delimiters-using-stronger-colors ()
  "Rainbow delimiters using stronger colors."
  (interactive)
  (cl-loop
   for index from 1 to rainbow-delimiters-max-face-count
   do
   (let ((face (intern (format "rainbow-delimiters-depth-%d-face" index))))
    (cl-callf color-saturate-name (face-foreground face) 30))))
(add-hook 'emacs-startup-hook 'rainbow-delimiters-using-stronger-colors)

;; --- Lisp ---
;; Emacs Lisp
(add-hook 'elisp-mode-hook 'company-mode)

;; Common Lisp
(require 'cl)

;; TODO ----
;; SBCL
;(setq inferior-lisp-program "sbcl")
;(load (expand-file-name "~/quicklisp/slime-helper.el"))
;; TODO ----

;; --- C C++ ---
(add-hook 'c-mode-hook      'company-mode)
(add-hook 'c++-mode-hook    'company-mode)
;; 缩进4个空格
(setq c-basic-offset 4)
;; 没有这个 { } 就会瞎搞
(setq c-default-style "linux")

;; --- Go ---
(require 'my-golang)

;; --- Javascript ---
(require 'my-js)

;; --- Rust ---
(require 'my-rust)

;; --- Python ---
;(add-hook 'python-mode-hook 'anaconda-mode)

;; 补全
;(elpy-enable)
;(setq elpy-rpc-backend "jedi")

;; --- Ruby ---
;(require 'my-rails)

(provide 'custom-language)
;;; custom-language.el ends here
