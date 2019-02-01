;;; custom-shell.el --- user customization file    -*- no-byte-compile: t -*-
;;; Commentary:

;;; Code:
(use-package shell-pop
  :init
  (let ((shell-type
         (if sys/win32p
             '("eshell" "*eshell*" (lambda () (eshell)))
           '("vterm" "vterm" (lambda () (vterm))))))

    (setq shell-pop-shell-type shell-type))

  (require 'shell-pop)
  (global-set-key [f9] '(lambda ()
                          "Shell popup."
                          (interactive)
                          (if (cl-search "vterm" (buffer-name))
                              (shell-pop-out)
                            (shell-pop-up 1)))))

(setq gd-utils-path (file-name-directory custom-file))
(use-package gd-utils
  :load-path gd-utils-path
  :init
  (require 'gd-utils)
  (setenv "SHELL" (gd-util-search-shell)))

(add-hook 'vterm-mode-hook
          (lambda ()
            ;;  mkdir -p ~/.terminfo
            ;;  tic -o ~/.terminfo /app/emacs/emacs/etc/e/eterm-color.ti
            ;; ~/.terminfo/65 -> ~/.terminfo/e
            ;; infocmp eterm-color
            (setenv "TERMINFO" "~/.terminfo")

            ;; keybind
            (define-key vterm-mode-map (kbd "M-c") #'vterm-send-ctrl-c)
            (evil-define-key 'motion vterm-mode-map (kbd "C-o") (lambda () (interactive) (vterm)))

            ;; (dolist (key-pair '(("C-a" . "\C-a") ("C-e" . "\C-e") ("C-k" . "\C-k") ("C-u" . "\C-u")))
            ;;   (evil-define-key 'insert vterm-mode-map (kbd (car key-pair))
            ;;     `(lambda () (interactive) (vterm-send-string (cdr ',key-pair)))))

            ;; (let ((key-pair '('C-a '\C-a)))
            ;;   (evil-define-key 'insert vterm-mode-map (kbd (car key-pair))
            ;;     `(lambda () (interactive) (vterm-send-string (cadr ',key-pair)))))

            (dolist (key '("a" "e" "k" "u"))
              (let ((key-pair (kbd (concat "C-" key))))
                (evil-define-key 'insert vterm-mode-map key-pair
                  `(lambda () (interactive) (vterm-send-string ,key-pair)))))

            (dolist (key '("M-j" "M-k"))
              (define-key vterm-mode-map (kbd key) nil))

            (dolist (key '([f1] [f2] [f3] [f4] [f5] [f6] [f7] [f8] [f9] [f10] [f11] [f12]))
              (define-key vterm-mode-map `,key nil))

            ;; (dotimes (n 12)
            ;;   (let ((key (kbd (concat "f" (int-to-string (1+ n))))))
            ;;     (define-key vterm-mode-map `,key nil)))
            ))

(concat "[f" (int-to-string (1+ 1))  "]")
(provide 'custom-shell)
;;; custom-shell.el ends here
