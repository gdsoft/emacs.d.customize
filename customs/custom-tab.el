;;; custom-tab.el --- user customization file    -*- no-byte-compile: t -*-
;;; Commentary:

;;; Code:
(require 'awesome-tab)
(awesome-tab-mode t)

(global-set-key [(meta j)] 'awesome-tab-backward)
(global-set-key [(meta k)] 'awesome-tab-forward)

(defun awesome-tab-hide-tab-function (x)
  "Awesome tab hide tab function (X)."
  (let ((name (format "%s" x)))
    (and
     (not (string-prefix-p "*epc" name))
     (not (string-prefix-p "*helm" name))
     (not (string-prefix-p "*Compile-Log*" name))
     (not (string-prefix-p "*lsp" name))
     (not (and (string-prefix-p "magit" name)
               (not (file-name-extension name))))
     )))

(setq awesome-tab-buffer-groups-function #'custom-tab-buffer-groups)

(defun custom-tab-buffer-groups ()
  "`custom-tab-buffer-groups' control buffers' group rules.

Group custom-tab with mode if buffer is derived from `vterm-mode'.
Other buffer group by `awesome-tab-buffer-groups' rules."
  (if (derived-mode-p 'vterm-mode) '("VTerm")
    (awesome-tab-buffer-groups)))

(provide 'custom-tab)
;;; custom-tab.el ends here
