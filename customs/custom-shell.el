;;; custom-shell.el --- user customization file    -*- no-byte-compile: t -*- -*- lexical-binding: t -*-
;;; Commentary:

;; Set the `:foreground` and `:background` attributes of the following faces to a
;; color you like:

;; - vterm-color-default-fg
;; - vterm-color-default-bg
;; - vterm-color-black-fg
;; - vterm-color-black-bg
;; - vterm-color-red-fg
;; - vterm-color-green-bg
;; - vterm-color-green-fg
;; - vterm-color-yellow-bg
;; - vterm-color-blue-fg
;; - vterm-color-blue-bg
;; - vterm-color-magenta-fg
;; - vterm-color-magenta-bg
;; - vterm-color-cyan-fg
;; - vterm-color-cyan-bg
;; - vterm-color-white-fg
;; - vterm-color-white-bg

;;; Code:

;; Shell Pop
(use-package shell-pop
  :bind ([f9] . shell-pop)
  :init (setq shell-pop-window-size 30
              shell-pop-shell-type
              (cond ((fboundp 'vterm) '("vterm" "*vterm*" #'vterm))
                    (sys/win32p '("eshell" "*eshell*" #'eshell))
                    (t '("terminal" "*terminal*"
                         (lambda () (term shell-pop-term-shell)))))))

(add-hook 'vterm-mode-hook
          (lambda ()
            ;; mkdir -p ~/.terminfo
            ;; tic -o ~/.terminfo /app/emacs/emacs/etc/e/eterm-color.ti
            ;; ~/.terminfo/65 -> ~/.terminfo/e
            ;; infocmp eterm-color
            (setenv "TERMINFO" "~/.terminfo")

            ;; keybind
            (define-key vterm-mode-map (kbd "M-c") #'vterm-send-ctrl-c)

            (dolist (state '(insert motion))
              (evil-define-key state vterm-mode-map (kbd "C-o") (lambda () (interactive) (vterm))))

            ;; (dolist (c '("a" "e" "k" "u"))
            ;;   (let ((key (kbd (concat "C-" c))))
            ;;     (evil-define-key 'insert vterm-mode-map key `(lambda () (interactive) (vterm-send-string ,key)))))

            (mapc (lambda (x)
                    (--> x
                         (concat "C-" it)
                         (kbd it)
                         (evil-define-key 'insert vterm-mode-map it `(lambda () (interactive) (vterm-send-string ,it)))))
                  '("a" "e" "k" "u"))

            (dolist (key '("M-j" "M-k"))
              (define-key vterm-mode-map (kbd key) nil))

            (dotimes (n 12)
              (let ((key (intern (concat "f" (int-to-string (1+ n))))))
                (define-key vterm-mode-map `[,key] nil)))
            ))

(provide 'custom-shell)
;;; custom-shell.el ends here
