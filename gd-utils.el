;;; gd-utils.el --- gd utils    -*- no-byte-compile: t -*-
;;; Commentary:

;;; Code:
(defun gd-util-ismac ()
  "システムがMacかどうかを判定する."
  (eq system-type 'darwin))

(defun gd-util-islinux ()
  "システムがLinuxかどうかを判定する."
  (eq system-type 'gnu/linux))

(defun gd-util-search-shell ()
  "Shellを探す."
  (let ((shell-name
         ;; Mac または Linux
         ;;(if (or (eq system-type 'darwin) (eq system-type 'gnu/linux))
         (if (or (gd-util-ismac) (gd-util-islinux))
             (or (executable-find "zsh") (executable-find "bash"))
           ;; その他
           (or (executable-find "bash")
               (executable-find "sh")
               ;; (executable-find "fish")
               ;; (executable-find "f_zsh") ;; Emacs + Cygwin を利用する人は Zsh の代りにこれにしてください
               ;; (executable-find "f_bash") ;; Emacs + Cygwin を利用する人は Bash の代りにこれにしてください
               (executable-find "cmdproxy")))))

    (if shell-name shell-name
      (error "Can't find 'shell' command in PATH!!!"))))

(provide 'gd-utils)
;;; gd-utils.el ends here
