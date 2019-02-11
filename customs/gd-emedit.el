;;; gd-emedit.el --- Minor mode for editing parentheses

;; Filename: gd-emedit.el
;; Description: Minor mode for editing parentheses
;; Author: Guo Dong <guodongsoft@gmail.com>
;; Maintainer: Guo Dong <guodongsoft@gmail.com>
;; Copyright (C) 2019, Guo Dong, all rights reserved.
;; Created: 2019-02-12 09:27:58
;; Version: 0.1
;; Last-Updated: 2019-02-09 02:27:40
;;           By: Guo Dong
;; URL:
;; Keywords:
;; Compatibility: GNU Emacs 27.0.50
;;

;;; License
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; Minor mode for editing parentheses
;;
;; The gd-emedit was largely inspired by paredit and awesome-pair.
;; Thanks Taylor R. Campbell and Andy Stewart
;;

;;; Installation:
;;
;; Put gd-emedit.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'gd-emedit)
;;
;; No need more.

;;; Customize:
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET gd-emedit RET
;;

;;; Change log:
;;
;;
;; 2019/02/12
;;      * First released.
;;

;;; Require
(require 'subr-x)

;;; Code:

(defvar gd-emedit-mode-map (make-sparse-keymap)
  "Keymap for the gd-emedit minor mode.")

(define-minor-mode gd-emedit-mode
  "Minor mode for auto parenthesis pairing with syntax table.
\\<gd-emedit-mode-map>"
  )

(defun region-round (start-ins end-ins)
  (if (region-active-p)
      (let ((start (region-beginning))
            (end (region-end)))
        (setq mark-active nil)
        (goto-char start)
        (insert start-ins)
        (goto-char (+ end (length end-ins)))
        (insert end-ins)
        (goto-char (+ start (length start-ins)))
        t)))

(defun direct-insert (ins &rest args)
  (defvar conds '(gd-emedit-in-string-p gd-emedit-in-comment-p))
  (nconc conds args)

  (defvar result nil)
  (catch 'break
    (dolist (cond conds)
      (when (funcall cond)
        (insert ins)
        (setq result t)
        (throw 'break result))))
  result)

(defun pair-insert (start-ins end-ins)
  (insert start-ins)
  (insert end-ins)
  (backward-char (length end-ins))
  t)

(defun gd-emedit-open (start-ins end-ins &optional special-open)
  (or (region-round start-ins end-ins)
      (direct-insert start-ins)
      (or (if special-open (funcall special-open))
          (pair-insert start-ins end-ins))))

;;;;;;;;;;;;;;;;; Interactive functions ;;;;;;;;;;;;;;;;;;;;;;

(defun gd-emedit-open-round ()
  (interactive)
  (gd-emedit-open "(" ")"))

(defun gd-emedit-open-curly ()
  (interactive)
  (gd-emedit-open "{" "}"
                  (lambda ()
                    (if (derived-mode-p 'ruby-mode)
                        (pair-insert "{ " " }")))))

(defun gd-emedit-open-bracket ()
  (interactive)
  (gd-emedit-open "[" "]"))

(defun gd-emedit-close-round ()
  (interactive)


  (cond ((or (gd-emedit-in-string-p)
             (gd-emedit-in-comment-p))
         (insert ")"))
        ;; Insert ) directly in sh-mode for case ... in syntax.
        ((derived-mode-p 'sh-mode)
         (insert ")"))
        (t
         (let ((close (gd-emedit-missing-close)))
           (if close
               (if (eq ?\) (matching-paren close))
                   (insert ")"))
             (up-list))
           ))))

(defun gd-emedit-close-curly ()
  (interactive)
  (cond ((or (gd-emedit-in-string-p)
             (gd-emedit-in-comment-p))
         (insert "}"))
        (t
         (let ((close (gd-emedit-missing-close)))
           (if close
               (if (eq ?\} (matching-paren close))
                   (insert "}"))
             (up-list))
           ))))

(defun gd-emedit-close-bracket ()
  (interactive)
  (cond ((or (gd-emedit-in-string-p)
             (gd-emedit-in-comment-p))
         (insert "]"))
        (t
         (let ((close (gd-emedit-missing-close)))
           (if close
               (if (eq ?\] (matching-paren close))
                   (insert "]"))
             (up-list))
           ))))

(defun gd-emedit-double-quote ()
  (interactive)
  (cond ((gd-emedit-in-string-p)
         (insert "\\\""))
        ((gd-emedit-in-comment-p)
         (insert "\""))
        (t
         (insert "\"\"")
         (backward-char))
        ))

(defun gd-emedit-match-paren (arg)
  "Go to the matching parenthesis if on parenthesis, otherwise insert %."
  (interactive "p")
  (cond ((or (gd-emedit-in-comment-p)
             (gd-emedit-in-string-p))
         (self-insert-command (or arg 1)))
        ((looking-at "\\s\(\\|\\s\{\\|\\s\[")
         (forward-list))
        ((looking-back "\\s\)\\|\\s\}\\|\\s\\]")
         (backward-list))
        (t
         (cond
          ;; Enhancement the automatic jump of web-mode.
          ((derived-mode-p 'web-mode)
           (gd-emedit-web-mode-match-paren))
          (t
           (self-insert-command (or arg 1))))
         )))

(defun gd-emedit-web-mode-match-paren ()
  (cond ((looking-at "<")
         (sgml-skip-tag-forward 1))
        ((looking-back ">")
         (sgml-skip-tag-backward 1))
        (t (self-insert-command (or arg 1)))))

(defun gd-emedit-backward-delete ()
  (interactive)
  (cond ((gd-emedit-in-string-p)
         (gd-emedit-backward-delete-in-string))
        ((gd-emedit-in-comment-p)
         (backward-delete-char 1))
        ((gd-emedit-after-close-pair-p)
         (gd-emedit-backward-movein-or-delete-close-pair))
        ((gd-emedit-in-empty-pair-p)
         (gd-emedit-backward-delete-in-pair))
        ((not (gd-emedit-after-open-pair-p))
         (backward-delete-char 1))))

(defun gd-emedit-forward-delete ()
  (interactive)
  (cond ((gd-emedit-in-string-p)
         (gd-emedit-forward-delete-in-string))
        ((gd-emedit-in-comment-p)
         (delete-char 1))
        ((gd-emedit-before-open-pair-p)
         (gd-emedit-forward-movein-or-delete-open-pair))
        ((gd-emedit-in-empty-pair-p)
         (gd-emedit-backward-delete-in-pair))
        ((not (gd-emedit-before-close-pair-p))
         (delete-char 1)
         )))

(defun gd-emedit-kill ()
  "It's annoying that we need re-indent line after we delete blank line with `gd-emedit-kill'.
`paredt-kill+' fixed this problem.

If current mode is `web-mode', use `gd-emedit-web-mode-kill' instead `gd-emedit-kill' for smarter kill operation."
  (interactive)
  (cond ((derived-mode-p 'web-mode)
         (gd-emedit-web-mode-kill))
        ((derived-mode-p 'ruby-mode)
         (gd-emedit-ruby-mode-kill))
        (t
         (gd-emedit-common-mode-kill))))

(defun gd-emedit-wrap-round ()
  (interactive)
  (cond
   ((region-active-p)
    (let ((start (region-beginning))
          (end (region-end)))
      (setq mark-active nil)
      (goto-char start)
      (insert "(")
      (goto-char (+ end 1))
      (insert ")")
      (goto-char start)))
   ((gd-emedit-in-string-p)
    (let ((string-bound (gd-emedit-string-start+end-points)))
      (save-excursion
        (goto-char (car string-bound))
        (insert "(")
        (goto-char (+ (cdr string-bound) 2))
        (insert ")"))))
   ((gd-emedit-in-comment-p)
    (save-excursion
      (let ((start (beginning-of-thing 'symbol))
            (end (end-of-thing 'symbol)))
        (goto-char start)
        (insert "(")
        (goto-char (+ end 1))
        (insert ")"))))
   (t
    (save-excursion
      (let ((start (beginning-of-thing 'sexp))
            (end (end-of-thing 'sexp)))
        (goto-char start)
        (insert "(")
        (goto-char (+ end 1))
        (insert ")"))
      )))
  ;; Forward to jump in parenthesis.
  (forward-char)
  ;; Indent wrap area.
  (gd-emedit-indent-parenthesis-area))

(defun gd-emedit-wrap-bracket ()
  (interactive)
  (cond
   ((region-active-p)
    (let ((start (region-beginning))
          (end (region-end)))
      (setq mark-active nil)
      (goto-char start)
      (insert "[")
      (goto-char (+ end 1))
      (insert "]")
      (goto-char start)))
   ((gd-emedit-in-string-p)
    (let ((string-bound (gd-emedit-string-start+end-points)))
      (save-excursion
        (goto-char (car string-bound))
        (insert "[")
        (goto-char (+ (cdr string-bound) 2))
        (insert "]"))))
   ((gd-emedit-in-comment-p)
    (save-excursion
      (let ((start (beginning-of-thing 'symbol))
            (end (end-of-thing 'symbol)))
        (goto-char start)
        (insert "[")
        (goto-char (+ end 1))
        (insert "]"))))
   (t
    (save-excursion
      (let ((start (beginning-of-thing 'sexp))
            (end (end-of-thing 'sexp)))
        (goto-char start)
        (insert "[")
        (goto-char (+ end 1))
        (insert "]"))
      )))
  ;; Forward to jump in parenthesis.
  (forward-char)
  ;; Indent wrap area.
  (gd-emedit-indent-parenthesis-area))

(defun gd-emedit-wrap-curly ()
  (interactive)
  (cond
   ((region-active-p)
    (let ((start (region-beginning))
          (end (region-end)))
      (setq mark-active nil)
      (goto-char start)
      (insert "{")
      (goto-char (+ end 1))
      (insert "}")
      (goto-char start)))
   ((gd-emedit-in-string-p)
    (let ((string-bound (gd-emedit-string-start+end-points)))
      (save-excursion
        (goto-char (car string-bound))
        (insert "{")
        (goto-char (+ (cdr string-bound) 2))
        (insert "}"))))
   ((gd-emedit-in-comment-p)
    (save-excursion
      (let ((start (beginning-of-thing 'symbol))
            (end (end-of-thing 'symbol)))
        (goto-char start)
        (insert "{")
        (goto-char (+ end 1))
        (insert "}"))))
   (t
    (save-excursion
      (let ((start (beginning-of-thing 'sexp))
            (end (end-of-thing 'sexp)))
        (goto-char start)
        (insert "{")
        (goto-char (+ end 1))
        (insert "}"))
      )))
  ;; Forward to jump in parenthesis.
  (forward-char)
  ;; Indent wrap area.
  (gd-emedit-indent-parenthesis-area))

(defun gd-emedit-wrap-double-quote ()
  (interactive)
  (cond
   ((region-active-p)
    (let ((start (region-beginning))
          (end (region-end)))
      (setq mark-active nil)
      (goto-char start)
      (insert "\"")
      (goto-char (+ end 1))
      (insert "\"")
      (goto-char start)))
   ((gd-emedit-in-string-p)
    (goto-char (1+ (cdr (gd-emedit-string-start+end-points)))))
   ((gd-emedit-in-comment-p)
    (save-excursion
      (let ((start (beginning-of-thing 'symbol))
            (end (end-of-thing 'symbol)))
        (goto-char start)
        (insert "\"")
        (goto-char (+ end 1))
        (insert "\""))))
   (t
    (save-excursion
      (let ((start (beginning-of-thing 'sexp))
            (end (end-of-thing 'sexp)))
        (goto-char start)
        (insert "\"")
        (goto-char (+ end 1))
        (insert "\"")))))
  ;; Forward to jump in parenthesis.
  (forward-char)
  ;; Indent wrap area.
  (gd-emedit-indent-parenthesis-area))

(defun gd-emedit-unwrap (&optional argument)
  (interactive "P")
  (if (gd-emedit-in-string-p)
      (gd-emedit-splice-string argument)
    (save-excursion
      (gd-emedit-kill-surrounding-sexps-for-splice argument)
      (backward-up-list)
      (save-excursion
        (forward-sexp)
        (backward-delete-char 1))
      (delete-char 1)
      ;; Try to indent parent expression after unwrap pair.
      ;; This feature just enable in lisp-like language.
      (when (or
             (derived-mode-p 'lisp-mode)
             (derived-mode-p 'emacs-lisp-mode))
        (ignore-errors
          (backward-up-list)
          (indent-sexp))))))

(defun gd-emedit-jump-out-pair-and-newline ()
  (interactive)
  (cond ((gd-emedit-in-string-p)
         (goto-char (1+ (cdr (gd-emedit-string-start+end-points))))
         (newline-and-indent))
        (t
         ;; Just do when have `up-list' in next step.
         (if (gd-emedit-ignore-errors (save-excursion (up-list)))
             (let (up-list-point)
               (if (gd-emedit-is-blank-line-p)
                   ;; Clean current line first if current line is blank line.
                   (gd-emedit-kill-current-line)
                 ;; Move out of current parentheses and newline.
                 (up-list)
                 (setq up-list-point (point))
                 (newline-and-indent)
                 ;; Try to clean unnecessary whitespace before close parenthesis.
                 ;; This feature just enable in lisp-like language.
                 (when (or
                        (derived-mode-p 'lisp-mode)
                        (derived-mode-p 'emacs-lisp-mode))
                   (save-excursion
                     (goto-char up-list-point)
                     (backward-char)
                     (when (gd-emedit-only-whitespaces-before-cursor-p)
                       (gd-emedit-delete-whitespace-around-cursor))))))
           ;; Try to clean blank line if no pair can jump out.
           (if (gd-emedit-is-blank-line-p)
               (gd-emedit-kill-current-line))))))

(defun gd-emedit-jump-left ()
  "To left of previous match parentheses."
  (interactive)
  (cond
   ;; Jump out of string if cursor in string area.
   ((gd-emedit-in-string-p)
    (goto-char (car (gd-emedit-string-start+end-points))))
   ;; Jump to previous pair.
   (t
    (backward-char 1)
    (while (not (looking-at "\\(['\"<({]\\|[[]\\)")) (backward-char 1)))))

(defun gd-emedit-jump-right ()
  "To right of next match parentheses."
  (interactive)
  (cond
   ;; Jump out of string if cursor in string area.
   ((gd-emedit-in-string-p)
    (goto-char (+ (cdr (gd-emedit-string-start+end-points)) 1)))
   ;; Jump to next pair.
   (t
    (while (not (looking-at "\\(['\">)}]\\|]\\)")) (forward-char 1))
    (forward-char 1))))

(defun gd-emedit-delete-whitespace-before-cursor ()
  (kill-region (save-excursion
                 (search-backward-regexp "[^ \t\n]" nil t)
                 (forward-char)
                 (point))
               (point)))

(defun gd-emedit-delete-whitespace-around-cursor ()
  (kill-region (save-excursion
                 (search-backward-regexp "[^ \t\n]" nil t)
                 (forward-char)
                 (point))
               (save-excursion
                 (search-forward-regexp "[^ \t\n]" nil t)
                 (backward-char)
                 (point))))

(defun gd-emedit-kill-current-line ()
  (kill-region (beginning-of-thing 'line) (end-of-thing 'line))
  (back-to-indentation))

(defun gd-emedit-missing-close ()
  (let (open)
    (ignore-errors
      (save-excursion
        (backward-up-list)
        (setq open (char-after))
        (if (gd-emedit-ignore-errors (forward-sexp))
            nil
          open)))))

(defun gd-emedit-backward-delete-in-pair ()
  (backward-delete-char 1)
  (delete-char 1))

(defun gd-emedit-backward-movein-or-delete-close-pair ()
  (if (gd-emedit-ignore-errors (save-excursion (backward-sexp)))
      (backward-char)
    (backward-delete-char 1)))

(defun gd-emedit-forward-movein-or-delete-open-pair ()
  (if (gd-emedit-ignore-errors (save-excursion (forward-sexp)))
      (forward-char)
    (delete-char 1)))

(defun gd-emedit-backward-delete-in-string ()
  (let ((start+end (gd-emedit-string-start+end-points)))
    (cond
     ;; Some language, such as Python, `gd-emedit-string-start+end-points' will return nil cause by `beginning-of-defun' retun nil.
     ;; This logical branch is handle this.
     ((not start+end)
      ;; First determine if it is in the string area?
      (when (gd-emedit-in-string-p)
        (let ((syn-before (char-syntax (char-before)))
              (syn-after  (char-syntax (char-after))))
          (cond
           ;; Remove double quotes when the string is empty
           ((and (eq syn-before ?\" )
                 (eq syn-after  ?\" ))
            (backward-delete-char 1)
            (delete-char 1))
           ;; If there is still content in the string and the double quotation marks are in front of the cursor,
           ;; no delete operation is performed.
           ((eq syn-before ?\" ))
           ;; If the cursor is not double quotes before and after, delete the previous character.
           (t
            (backward-delete-char 1))))))
     ((not (eq (1- (point)) (car start+end)))
      (if (gd-emedit-in-string-escape-p)
          (delete-char 1))
      (backward-delete-char 1)
      (if (gd-emedit-in-string-escape-p)
          (backward-delete-char 1)))
     ((eq (point) (cdr start+end))
      (backward-delete-char 1)
      (delete-char 1)))))

(defun gd-emedit-forward-delete-in-string ()
  (let ((start+end (gd-emedit-string-start+end-points)))
    (cond
     ;; Some language, such as Python, `gd-emedit-string-start+end-points' will return nil cause by `beginning-of-defun' retun nil.
     ;; This logical branch is handle this.
     ((not start+end)
      ;; First determine if it is in the string area?
      (when (gd-emedit-in-string-p)
        (let ((syn-before (char-syntax (char-before)))
              (syn-after  (char-syntax (char-after))))
          (cond
           ;; Remove double quotes when the string is empty
           ((and (eq syn-before ?\" )
                 (eq syn-after  ?\" ))
            (backward-delete-char 1)
            (delete-char 1))
           ;; If there is still content in the string and the double quotation marks are after of the cursor,
           ;; no delete operation is performed.
           ((eq syn-after ?\" ))
           ;; If the cursor is not double quotes before and after, delete the previous character.
           (t
            (delete-char 1))))))
     ((not (eq (point) (cdr start+end)))
      (cond ((gd-emedit-in-string-escape-p)
             (delete-char -1))
            ((eq (char-after) ?\\ )
             (delete-char +1)))
      (delete-char +1))
     ((eq (1- (point)) (car start+end))
      (delete-char -1)
      (delete-char +1)))))

(defun gd-emedit-splice-string (argument)
  (let ((original-point (point))
        (start+end (gd-emedit-string-start+end-points)))
    (let ((start (car start+end))
          (end (cdr start+end)))
      (let* ((escaped-string
              (cond ((not (consp argument))
                     (buffer-substring (1+ start) end))
                    ((= 4 (car argument))
                     (buffer-substring original-point end))
                    (t
                     (buffer-substring (1+ start) original-point))))
             (unescaped-string
              (gd-emedit-unescape-string escaped-string)))
        (if (not unescaped-string)
            (error "Unspliceable string.")
          (save-excursion
            (goto-char start)
            (delete-region start (1+ end))
            (insert unescaped-string))
          (if (not (and (consp argument)
                        (= 4 (car argument))))
              (goto-char (- original-point 1))))))))

(defun gd-emedit-point-at-sexp-start ()
  (save-excursion
    (forward-sexp)
    (backward-sexp)
    (point)))

(defun gd-emedit-point-at-sexp-end ()
  (save-excursion
    (backward-sexp)
    (forward-sexp)
    (point)))

(defun gd-emedit-point-at-sexp-boundary (n)
  (cond ((< n 0) (gd-emedit-point-at-sexp-start))
        ((= n 0) (point))
        ((> n 0) (gd-emedit-point-at-sexp-end))))

(defun gd-emedit-kill-surrounding-sexps-for-splice (argument)
  (cond ((or (gd-emedit-in-string-p)
             (gd-emedit-in-comment-p))
         (error "Invalid context for splicing S-expressions."))
        ((or (not argument) (eq argument 0)) nil)
        ((or (numberp argument) (eq argument '-))
         (let* ((argument (if (eq argument '-) -1 argument))
                (saved (gd-emedit-point-at-sexp-boundary (- argument))))
           (goto-char saved)
           (ignore-errors (backward-sexp argument))
           (gd-emedit-hack-kill-region saved (point))))
        ((consp argument)
         (let ((v (car argument)))
           (if (= v 4)
               (let ((end (point)))
                 (ignore-errors
                   (while (not (bobp))
                     (backward-sexp)))
                 (gd-emedit-hack-kill-region (point) end))
             (let ((beginning (point)))
               (ignore-errors
                 (while (not (eobp))
                   (forward-sexp)))
               (gd-emedit-hack-kill-region beginning (point))))))
        (t (error "Bizarre prefix argument `%s'." argument))))

(defun gd-emedit-unescape-string (string)
  (with-temp-buffer
    (insert string)
    (goto-char (point-min))
    (while (and (not (eobp))
                (search-forward "\\" nil t))
      (delete-char -1)
      (forward-char))
    (condition-case condition
        (progn (check-parens) (buffer-string))
      (error nil))))

(defun gd-emedit-hack-kill-region (start end)
  (let ((this-command nil)
        (last-command nil))
    (kill-region start end)))

(defun gd-emedit-kill-internal ()
  (cond (current-prefix-arg
         (kill-line (if (integerp current-prefix-arg)
                        current-prefix-arg
                      1)))
        ((gd-emedit-in-string-p)
         (gd-emedit-kill-line-in-string))
        ((gd-emedit-in-single-quote-string-p)
         (gd-emedit-kill-line-in-single-quote-string))
        ((or (gd-emedit-in-comment-p)
             (save-excursion
               (gd-emedit-skip-whitespace t (point-at-eol))
               (or (eq (char-after) ?\; )
                   (eolp))))
         (kill-line))
        (t (gd-emedit-kill-sexps-on-line))))

(defun gd-emedit-kill-line-in-single-quote-string ()
  (let ((sexp-end (save-excursion
                    (forward-sexp)
                    (backward-char)
                    (point))))
    (kill-region (point) sexp-end)))

(defun gd-emedit-kill-line-in-string ()
  (cond ((save-excursion
           (gd-emedit-skip-whitespace t (point-at-eol))
           (eolp))
         (kill-line))
        (t
         (save-excursion
           (if (gd-emedit-in-string-escape-p)
               (backward-char))
           (let ((beginning (point)))
             (while (save-excursion
                      (forward-char)
                      (gd-emedit-in-string-p))
               (forward-char))
             (kill-region beginning (point)))
           ))))

(defun gd-emedit-skip-whitespace (trailing-p &optional limit)
  (funcall (if trailing-p 'skip-chars-forward 'skip-chars-backward)
           " \t\n"
           limit))

(defun gd-emedit-kill-sexps-on-line ()
  (if (gd-emedit-in-char-p)
      (backward-char 2))
  (let ((beginning (point))
        (eol (point-at-eol)))
    (let ((end-of-list-p (gd-emedit-forward-sexps-to-kill beginning eol)))
      (if end-of-list-p (progn (up-list) (backward-char)))
      (if kill-whole-line
          (gd-emedit-kill-sexps-on-whole-line beginning)
        (kill-region beginning
                     (if (and (not end-of-list-p)
                              (eq (point-at-eol) eol))
                         eol
                       (point)))))))

(defun gd-emedit-forward-sexps-to-kill (beginning eol)
  (let ((end-of-list-p nil)
        (firstp t))
    (catch 'return
      (while t
        (if (and kill-whole-line (eobp)) (throw 'return nil))
        (save-excursion
          (unless (gd-emedit-ignore-errors (forward-sexp))
            (if (gd-emedit-ignore-errors (up-list))
                (progn
                  (setq end-of-list-p (eq (point-at-eol) eol))
                  (throw 'return nil))))
          (if (or (and (not firstp)
                       (not kill-whole-line)
                       (eobp))
                  (not (gd-emedit-ignore-errors (backward-sexp)))
                  (not (eq (point-at-eol) eol)))
              (throw 'return nil)))
        (forward-sexp)
        (if (and firstp
                 (not kill-whole-line)
                 (eobp))
            (throw 'return nil))
        (setq firstp nil)))
    end-of-list-p))

(defun gd-emedit-kill-sexps-on-whole-line (beginning)
  (kill-region beginning
               (or (save-excursion
                     (gd-emedit-skip-whitespace t)
                     (and (not (eq (char-after) ?\; ))
                          (point)))
                   (point-at-eol)))
  (cond ((save-excursion (gd-emedit-skip-whitespace nil (point-at-bol))
                         (bolp))
         (lisp-indent-line))
        ((eobp) nil)
        ((let ((syn-before (char-syntax (char-before)))
               (syn-after  (char-syntax (char-after))))
           (or (and (eq syn-before ?\) )
                    (eq syn-after  ?\( ))
               (and (eq syn-before ?\" )
                    (eq syn-after  ?\" ))
               (and (memq syn-before '(?_ ?w))
                    (memq syn-after  '(?_ ?w)))))
         (insert " "))))

(defun gd-emedit-common-mode-kill ()
  (if (gd-emedit-is-blank-line-p)
      (gd-emedit-kill-blank-line-and-reindent)
    (gd-emedit-kill-internal)))

(defun gd-emedit-web-mode-kill ()
  "It's a smarter kill function for `web-mode'."
  (if (gd-emedit-is-blank-line-p)
      (gd-emedit-kill-blank-line-and-reindent)
    (cond
     ;; Kill all content wrap by <% ... %> when right is <%
     ((and (looking-at "<%")
           (save-excursion (search-forward-regexp "%>" nil t)))
      (kill-region (point) (search-forward-regexp "%>" nil t)))
     ;; Kill content in <% ... %> if left is <% or <%=
     ((and (looking-back "<%=?\\s-?")
           (save-excursion (search-forward-regexp "%>" nil t)))
      (let ((start (point))
            (end (progn
                   (search-forward-regexp "%>" nil t)
                   (backward-char 2)
                   (point)
                   )))
        (kill-region start end)))
     ;; Kill string if current pointer in string area.
     ((gd-emedit-in-string-p)
      (gd-emedit-kill-internal))
     ;; Kill string in single quote.
     ((gd-emedit-in-single-quote-string-p)
      (gd-emedit-kill-line-in-single-quote-string))
     ;; Kill element if no attributes in tag.
     ((and
       (looking-at "\\s-?+</")
       (looking-back "<[a-z]+\\s-?>\\s-?+"))
      (web-mode-element-kill 1))
     ;; Kill whitespace in tag.
     ((looking-at "\\s-+>")
      (search-forward-regexp ">" nil t)
      (backward-char)
      (gd-emedit-delete-whitespace-before-cursor))
     ;; Jump in content if point in start tag.
     ((and (looking-at ">")
           (looking-back "<[a-z]+"))
      (forward-char 1))
     ;; Kill tag if in end tag.
     ((and (looking-at ">")
           (looking-back "</[a-z]+"))
      (beginning-of-thing 'sexp)
      (web-mode-element-kill 1))
     ;; Kill attributes if point in attributes area.
     ((and
       (web-mode-attribute-beginning-position)
       (web-mode-attribute-end-position)
       (>= (point) (web-mode-attribute-beginning-position))
       (<= (point) (web-mode-attribute-end-position)))
      (web-mode-attribute-kill))
     ;; Kill attributes if only space between point and attributes start.
     ((and
       (looking-at "\\s-+")
       (save-excursion
         (search-forward-regexp "\\s-+" nil t)
         (equal (point) (web-mode-attribute-beginning-position))))
      (search-forward-regexp "\\s-+")
      (web-mode-attribute-kill))
     ;; Kill line if rest chars is whitespace.
     ((looking-at "\\s-?+\n")
      (kill-line))
     (t
      (unless (gd-emedit-ignore-errors
               ;; Kill all sexps in current line.
               (gd-emedit-kill-sexps-on-line))
        ;; Kill block if sexp parse failed.
        (web-mode-block-kill))))))

(defun gd-emedit-ruby-mode-kill ()
  "It's a smarter kill function for `ruby-mode'.

If current line is blank line, re-indent line after kill whole line.

If current line is not blank, do `gd-emedit-kill' first, re-indent line if rest line start with ruby keywords.
"
  (if (gd-emedit-is-blank-line-p)
      (gd-emedit-kill-blank-line-and-reindent)
    ;; Do `gd-emedit-kill' first.
    (gd-emedit-kill-internal)

    ;; Re-indent current line if line start with ruby keywords.
    (when (let (in-beginning-block-p
                in-end-block-p
                current-symbol)
            (save-excursion
              (back-to-indentation)
              (ignore-errors (setq current-symbol (buffer-substring-no-properties (beginning-of-thing 'symbol) (end-of-thing 'symbol))))
              (setq in-beginning-block-p (member current-symbol '("class" "module" "else" "def" "if" "unless" "case" "while" "until" "for" "begin" "do")))
              (setq in-end-block-p (member current-symbol '("end")))

              (or in-beginning-block-p in-end-block-p)))
      (indent-for-tab-command))))

(defun gd-emedit-kill-blank-line-and-reindent ()
  (kill-region (beginning-of-thing 'line) (end-of-thing 'line))
  (back-to-indentation))

(defun gd-emedit-indent-parenthesis-area ()
  (let ((bound-start (save-excursion
                       (backward-up-list)
                       (point)))
        (bound-end (save-excursion
                     (up-list)
                     (point)
                     )))
    (save-excursion
      (indent-region bound-start bound-end))))

;;;;;;;;;;;;;;;;; Utils functions ;;;;;;;;;;;;;;;;;;;;;;
(defun gd-emedit-current-parse-state ()
  (let ((point (point)))
    (beginning-of-defun)
    (parse-partial-sexp (point) point)))

(defun gd-emedit-string-start+end-points (&optional state)
  (ignore-errors
    (save-excursion
      (let ((start (nth 8 (or state (gd-emedit-current-parse-state)))))
        (goto-char start)
        (forward-sexp 1)
        (cons start (1- (point)))))))

(defun gd-emedit-after-open-pair-p ()
  (save-excursion
    (let ((syn (char-syntax (char-before))))
      (or (eq syn ?\()
          (and (eq syn ?_)
               (eq (char-before) ?\{)))
      )))

(defun gd-emedit-after-close-pair-p ()
  (save-excursion
    (let ((syn (char-syntax (char-before))))
      (or (eq syn ?\) )
          (eq syn ?\" )
          (and (eq syn ?_ )
               (eq (char-before) ?\}))
          ))))

(defun gd-emedit-before-open-pair-p ()
  (save-excursion
    (let ((syn (char-syntax (char-after))))
      (or (eq syn ?\( )
          (eq syn ?\" )
          (and (eq syn ?_)
               (eq (char-after) ?\{))))))

(defun gd-emedit-before-close-pair-p ()
  (save-excursion
    (let ((syn (char-syntax (char-after))))
      (or (eq syn ?\) )
          (and (eq syn ?_)
               (eq (char-after) ?\}))))))

(defun gd-emedit-in-empty-pair-p ()
  (ignore-errors
    (save-excursion
      (or (and (eq (char-syntax (char-before)) ?\()
               (eq (char-after) (matching-paren (char-before))))
          (and (eq (char-syntax (char-before)) ?_)
               (eq (char-before) ?\{)
               (eq (char-syntax (char-after)) ?_)
               (eq (char-after) ?\})
               )))))

(defun gd-emedit-in-single-quote-string-p ()
  (save-excursion
    (when (gd-emedit-ignore-errors
           (progn
             (save-excursion (backward-sexp))
             (save-excursion (forward-sexp))))
      (let* ((current-sexp (buffer-substring-no-properties
                            (save-excursion
                              (backward-sexp)
                              (point))
                            (save-excursion
                              (forward-sexp)
                              (point))
                            ))
             (first-char (substring current-sexp 0 1))
             (last-char (substring current-sexp -1 nil)))
        (and (string-equal first-char "'")
             (string-equal last-char "'"))))))

(defun gd-emedit-in-string-p (&optional state)
  (save-excursion
    (or (nth 3 (or state (gd-emedit-current-parse-state)))
        (and
         (eq (get-text-property (point) 'face) 'font-lock-string-face)
         (eq (get-text-property (- (point) 1) 'face) 'font-lock-string-face))
        (and
         (eq (get-text-property (point) 'face) 'font-lock-doc-face)
         (eq (get-text-property (- (point) 1) 'face) 'font-lock-doc-face))
        )))

(defun gd-emedit-in-comment-p (&optional state)
  (save-excursion
    (or (nth 4 (or state (gd-emedit-current-parse-state)))
        (eq (get-text-property (point) 'face) 'font-lock-comment-face))))

(defun gd-emedit-in-string-escape-p ()
  (let ((oddp nil))
    (save-excursion
      (while (eq (char-before) ?\\ )
        (setq oddp (not oddp))
        (backward-char)))
    oddp))

(defun gd-emedit-in-char-p (&optional argument)
  (let ((argument (or argument (point))))
    (and (eq (char-before argument) ?\\ )
         (not (eq (char-before (1- argument)) ?\\ )))))

(defun gd-emedit-is-blank-line-p ()
  (save-excursion
    (beginning-of-line)
    (looking-at "[[:space:]]*$")))

(defun gd-emedit-only-whitespaces-before-cursor-p ()
  (let ((string-before-cursor
         (buffer-substring
          (save-excursion
            (beginning-of-line)
            (point))
          (point))))
    (equal (length (string-trim string-before-cursor)) 0)))

(defmacro gd-emedit-ignore-errors (body)
  `(ignore-errors
     ,body
     t))

(provide 'gd-emedit)

;;; gd-emedit.el ends here
