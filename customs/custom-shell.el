;;; custom-shell.el --- user customization file    -*- no-byte-compile: t -*-
;;; Commentary:

;;; Code:
(use-package shell-pop
  :init
  (let ((shell-type
         (if sys/win32p
             '("eshell" "*eshell*" (lambda () (eshell)))
           '("multi-term" "*terminal*" (lambda () (multi-term))))))

    (setq shell-pop-shell-type shell-type))

  (require 'shell-pop)
  (global-set-key [f9] '(lambda ()
                          "Shell popup."
                          (interactive)
                          (if (cl-search "terminal" (buffer-name))
                              (shell-pop-out)
                            (shell-pop-up 1))))

  ;; C-x t で multi-term を起動する
  (global-set-key (kbd "C-x t o") '(lambda ()
                                     "Get shell buffer."
                                     (interactive)
                                     (multi-term)
                                     )))

;; shell の存在を確認
(defun skt:shell ()
  "Find shell."
  (or (executable-find "fish")
      (executable-find "zsh")
      (executable-find "bash")
      ;; (executable-find "f_zsh") ;; Emacs + Cygwin を利用する人は Zsh の代りにこれにしてください
      ;; (executable-find "f_bash") ;; Emacs + Cygwin を利用する人は Bash の代りにこれにしてください
      (executable-find "cmdproxy")
      (error "Can't find 'shell' command in PATH!!")))

;; Shell 名の設定
(custom-set-variables
 '(shell-file-name (skt:shell))
 '(explicit-shell-file-name shell-file-name)

 ;; Emacs が保持する terminfo を利用する
 '(system-uses-terminfo nil)

 '(multi-term-program shell-file-name)

 ;; t: skip dedicated window when using `other-window'.
 '(multi-term-dedicated-skip-other-window-p t)

 ;; focus dedicated window after open
 '(multi-term-dedicated-select-after-open-p t)

 '(multi-term-dedicated-close-back-to-open-buffer-p t))

(setenv "SHELL" shell-file-name)

;; for lsp-go
;; replace backspace to ":"
;;(let ((gopath (getenv "GOPATH")))
;;  (setenv "GOPATH" (replace-regexp-in-string "[^a-zA-Z/_]+" ":" gopath)))

(with-eval-after-load 'multi-term
  ;; "C-z"、"C-x"、"C-c"、"C-h"、"C-y"、"<ESC>" のキーが奪われなくなりますので、ほとんどの操作は Emacs 的にできるはずです。
  ;; 他のキーも奪われたくなければ以下のようにキーを追加します。
  ;; emacs に認識させたいキーがある場合は、term-unbind-key-list に追加する
  (add-to-list 'term-unbind-key-list "C-\\") ; IME の切り替えを有効とする
  (add-to-list 'term-unbind-key-list "M-x")
  (add-to-list 'term-unbind-key-list "C-q")
  ;; (add-to-list 'term-unbind-key-list "C-o")  ; IME の切り替えに C-o を設定している場合

  ;; terminal に直接通したいキーがある場合は、以下をアンコメントする
  (delete "<ESC>" term-unbind-key-list)
                                        ;(delete "C-h" term-unbind-key-list)
                                        ;(delete "C-z" term-unbind-key-list)
                                        ;(delete "C-x" term-unbind-key-list)
                                        ;(delete "C-c" term-unbind-key-list)
                                        ;(delete "C-y" term-unbind-key-list)
  )

(defun term-send-tab ()
  "Term send tab."
  (interactive)
  (term-send-raw-string "\C-i"))

;; Interrupt
(defadvice term-interrupt-subjob
    (around ad-term-interrupt-subjob activate)
  "Term send raw string."
  (term-send-raw-string "\C-c"))

;; Paste via helm interface
(defadvice insert-for-yank
    (around insert-for-yank-on-term (str) activate)
  "Check term mode."
  (if (eq major-mode 'term-mode)
      (term-send-raw-string (ad-get-arg 0))
    ad-do-it))

(defun my-clear ()
  "Clear term."
  (interactive)
  (let ((comint-buffer-maximum-size 0))
    (comint-truncate-buffer)))

(add-hook 'term-mode-hook
          (lambda ()
            ;; https://github.com/fish-shell/fish-shell/issues/1411
            ;; fish in emacs ansi-term prints an extra "⏎" #1411
            ;; fixed
            (toggle-truncate-lines)
            (setq term-prompt-regexp "^.*❯❯❯ ")
            (make-local-variable 'mouse-yank-at-point)
            (setq mouse-yank-at-point t)
            (make-local-variable 'transient-mark-mode)
            (setq transient-mark-mode nil)
            (setq yas-dont-activate t)

            ;;  mkdir -p ~/.terminfo
            ;;  tic -o ~/.terminfo /app/emacs/emacs/etc/e/eterm-color.ti
            ;; ~/.terminfo/65 -> ~/.terminfo/e
            ;; infocmp eterm-color
            (setenv "TERMINFO" "~/.terminfo")

            ;; keybind
            ;; C-h を term 内文字削除にする
                                        ;(define-key term-raw-map (kbd "C-h") 'term-send-backspace)
            ;; C-y を term 内ペーストにする
                                        ;(define-key term-raw-map (kbd "C-y") 'term-paste)

            (define-key term-raw-map (kbd "M-d") 'term-send-forward-kill-word)
            (define-key term-raw-map (kbd "M-<backspace>") 'term-send-backward-kill-word)
            (define-key term-raw-map (kbd "M-w") 'term-send-backward-kill-word)
            (define-key term-raw-map (kbd "M-DEL") 'term-send-backward-kill-word)
            (define-key term-raw-map (kbd "TAB") 'term-send-tab)

            (define-key term-raw-map (kbd "C-l") 'my-clear)

            (evil-define-key 'insert term-raw-map (kbd "C-a") (lambda () (interactive) (term-send-raw-string "\C-a")))
            (evil-define-key 'insert term-raw-map (kbd "C-k") (lambda () (interactive) (term-send-raw-string "\C-k")))

            (define-key term-raw-map (kbd "M-d") (lambda () (interactive) (term-send-raw-string "\ed")))
            (define-key term-raw-map (kbd "M-p") (lambda () (interactive) (term-send-raw-string "\ep")))

            (define-key term-raw-map (kbd "<M-left>")  (lambda () (interactive) (term-send-raw-string "\e[1;3D")))
            (define-key term-raw-map (kbd "<M-right>") (lambda () (interactive) (term-send-raw-string "\e[1;3C")))
            (define-key term-raw-map (kbd "<M-up>")    (lambda () (interactive) (term-send-raw-string "\e[1;3A")))
            (define-key term-raw-map (kbd "<M-down>")  (lambda () (interactive) (term-send-raw-string "\e[1;3B")))

            (define-key term-raw-map (kbd "M-j") 'multi-term-prev)
            (define-key term-raw-map (kbd "M-k") 'multi-term-next)

            (define-key term-raw-map (kbd "C-SPC") nil)
            (define-key term-raw-map (kbd "C-@") nil) ; for putty
            (define-key term-raw-map (kbd "C-v") nil)

            (setq system-uses-terminfo t)
            (setq term-default-bg-color nil)
            (setq term-default-fg-color nil)
            (setq multi-term-scroll-show-maximum-output t)

            (setq show-trailing-whitespace nil)))

(provide 'custom-shell)
;;; custom-shell.el ends here
