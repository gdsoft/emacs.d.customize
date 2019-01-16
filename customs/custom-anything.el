;;; custom-anything.el --- user customization file    -*- no-byte-compile: t -*-
;;; Commentary:

;;; Code:
(require 'anything-complete)

;; Automatically collect symbols by 150 secs
(anything-lisp-complete-symbol-set-timer 150)
(define-key emacs-lisp-mode-map "\C-\M-i" 'anything-lisp-complete-symbol-partial-match)
(define-key lisp-interaction-mode-map "\C-\M-i" 'anything-lisp-complete-symbol-partial-match)
;; replace completion commands with `anything'
(anything-read-string-mode 1)
;; Bind C-o to complete shell history
(anything-complete-shell-history-setup-key "\C-o")

(provide 'custom-anything)
;;; custom-anything.el ends here
