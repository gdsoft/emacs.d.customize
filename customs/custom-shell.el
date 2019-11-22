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
(let* ((libvterm-dir "~/.config/emacs.me/emacs-libvterm")
       (vterm-el (expand-file-name "vterm.el" libvterm-dir)))
  (when (file-exists-p vterm-el)
    (push libvterm-dir load-path)
    (let (vterm-install)
      (require 'vterm))

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
    ))

(setq gd-utils-path (file-name-directory custom-file))
(use-package gd-utils
  :load-path gd-utils-path
  :init
  (require 'gd-utils)
  (setenv "SHELL" (gd-util-search-shell)))

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
