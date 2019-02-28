;;; custom-edit.el --- user customization file    -*- no-byte-compile: t -*-
;;; Commentary:

;;; Code:
(require 'gd-emedit)

(dolist (hook (list
               'c-mode-common-hook
               'c-mode-hook
               'c++-mode-hook
               'java-mode-hook
               'haskell-mode-hook
               'emacs-lisp-mode-hook
               'lisp-interaction-mode-hook
               'lisp-mode-hook
               'maxima-mode-hook
               'ielm-mode-hook
               'sh-mode-hook
               'makefile-gmake-mode-hook
               'php-mode-hook
               'python-mode-hook
               'js-mode-hook
               'go-mode-hook
               'qml-mode-hook
               'jade-mode-hook
               'css-mode-hook
               'ruby-mode-hook
               'coffee-mode-hook
               'rust-mode-hook
               'qmake-mode-hook
               'lua-mode-hook
               'swift-mode-hook
               'minibuffer-inactive-mode-hook
               ))
  (add-hook hook '(lambda () (gd-emedit-mode 1))))

(define-key gd-emedit-mode-map (kbd "(") 'gd-emedit-open-round)
(define-key gd-emedit-mode-map (kbd "[") 'gd-emedit-open-bracket)
(define-key gd-emedit-mode-map (kbd "{") 'gd-emedit-open-curly)
(define-key gd-emedit-mode-map (kbd ")") 'gd-emedit-close-round)
(define-key gd-emedit-mode-map (kbd "]") 'gd-emedit-close-bracket)
(define-key gd-emedit-mode-map (kbd "}") 'gd-emedit-close-curly)
(define-key gd-emedit-mode-map (kbd "%") 'gd-emedit-match-paren)
(define-key gd-emedit-mode-map (kbd "\"")  'gd-emedit-double-quote)
(evil-define-key 'motion gd-emedit-mode-map (kbd "\"") 'gd-emedit-double-quote)
(define-key gd-emedit-mode-map (kbd "M-o") 'gd-emedit-backward-delete)
(define-key gd-emedit-mode-map (kbd "C-k") 'gd-emedit-kill)
(define-key gd-emedit-mode-map (kbd "M-\"") 'gd-emedit-wrap-double-quote)
(define-key gd-emedit-mode-map (kbd "M-[") 'gd-emedit-wrap-bracket)
(define-key gd-emedit-mode-map (kbd "M-{") 'gd-emedit-wrap-curly)
(define-key gd-emedit-mode-map (kbd "M-(") 'gd-emedit-wrap-round)
(define-key gd-emedit-mode-map (kbd "M-)") 'gd-emedit-unwrap)
(define-key gd-emedit-mode-map (kbd "M-p") 'gd-emedit-jump-right)
(define-key gd-emedit-mode-map (kbd "M-n") 'gd-emedit-jump-left)
(define-key gd-emedit-mode-map (kbd "M-:") 'gd-emedit-jump-out-pair-and-newline)

(provide 'custom-edit)
;;; custom-edit.el ends here
