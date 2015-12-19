;;; limble.el --- Lisp navigation

;; Author: Vegard Øye <vegard_oye at hotmail.com>
;; Created: 8 May 2010
;; Keywords: modal Lisp editing
;;
;; This file is not part of GNU Emacs.

;;; Commentary:

;; limble: a combination of limber and nimble that denotes someone
;; who is both quick and light in motion and possess a supple and
;; resilient quality. (Urban Dictionary)
;;
;; This library provides a vi-like interface to paredit.el.

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of the
;; License, or any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
;; 02111-1307, USA.

;;; Code:

(require 'paredit)
(require 'vimpulse)

(defconst limble-version 0.1)

(defun limble-version ()
  (interactive)
  (message "limble version is %s" limble-version))

(define-minor-mode limble-mode
  "limble-mode provides modal bindings for editing Lisp code.")

(defun turn-on-limble-mode ()
  "Enable limble-mode."
  (interactive)
  (limble-mode 1))

;; vi (command) state bindings.
(vimpulse-define-key 'limble-mode 'vi-state "w" 'limble-forward-sexp)
(vimpulse-define-key 'limble-mode 'vi-state "W" 'limble-forward-Sexp)
(vimpulse-define-key 'limble-mode 'vi-state "e" 'limble-end-of-sexp)
(vimpulse-define-key 'limble-mode 'vi-state "E" 'limble-end-of-Sexp)
(vimpulse-define-key 'limble-mode 'vi-state "b" 'limble-backward-sexp)
(vimpulse-define-key 'limble-mode 'vi-state "B" 'limble-backward-Sexp)
(vimpulse-define-key 'limble-mode 'vi-state "J" 'limble-join)
(vimpulse-define-key 'limble-mode 'vi-state "Q" 'limble-indent)
(vimpulse-define-key 'limble-mode 'vi-state "gh" 'limble-backward-down-list)
(vimpulse-define-key 'limble-mode 'vi-state "gj" 'limble-down-list)
(vimpulse-define-key 'limble-mode 'vi-state "gk" 'limble-backward-up-list)
(vimpulse-define-key 'limble-mode 'vi-state "gl" 'limble-up-list)
(vimpulse-define-key 'limble-mode 'vi-state "(" 'limble-backward-up-list)
(vimpulse-define-key 'limble-mode 'vi-state ")" 'limble-up-list)
(vimpulse-define-key 'limble-mode 'vi-state "gi" 'limble-insert-before)
(vimpulse-define-key 'limble-mode 'vi-state "ga" 'limble-insert-after)
(vimpulse-define-key 'limble-mode 'vi-state "gO" 'limble-insert-above)
(vimpulse-define-key 'limble-mode 'vi-state "go" 'limble-insert-below)
(vimpulse-define-key 'limble-mode 'vi-state "gw" 'limble-wrap)

;; Operator-Pending state bindings.
(vimpulse-define-key 'limble-mode 'operator-state "w" 'limble-end-of-Sexp)
(vimpulse-define-key 'limble-mode 'operator-state "e" 'limble-end-of-Sexp)
(vimpulse-define-key 'limble-mode 'operator-state "b" 'limble-backward-Sexp)
(vimpulse-define-key 'limble-mode 'operator-state "aw" 'limble-select-sexp)
(vimpulse-define-key 'limble-mode 'operator-state "iw" 'limble-select-sexp)
(vimpulse-define-key 'limble-mode 'operator-state "s" 'limble-splice)
(vimpulse-define-key 'limble-mode 'operator-state "x" 'limble-transpose)
(vimpulse-define-key 'limble-mode 'operator-state "X" 'limble-transpose-backwards)

;; Insert state bindings.
(vimpulse-define-key 'limble-mode 'insert-state (kbd "RET") 'limble-ret)

(defun limble-forward-sexp (&optional arg)
  "Move to beginning of next S-expression, irrespective of nesting."
  (interactive "p")
  (setq arg (or arg 1))
  (dotimes (var arg)
    (cond
     ((or (paredit-in-string-p) (paredit-in-comment-p))
      (forward-word 2)
      (backward-word))
     ((and (char-after) (eq (char-syntax (char-after)) ?<))
      (forward-word)
      (backward-word))
     (t
      (unless (> (skip-syntax-forward " )>") 0)
        (if (and (char-after) (memq (char-syntax (char-after)) '(?\( ?')))
            (forward-char)
          (forward-sexp))
        (skip-syntax-forward " )>"))))))

(defun limble-forward-Sexp (&optional arg)
  "Move to beginning of next S-expression, respective of nesting."
  (interactive "p")
  (setq arg (or arg 1))
  (cond
   ((or (paredit-in-string-p) (paredit-in-comment-p)
        (and (char-after) (eq (char-syntax (char-after)) ?<)))
    (dotimes (var arg)
      (forward-word 2)
      (backward-word)))
   (t
    (skip-syntax-forward " )>")
    (unless (looking-at "[[:space:]]")
      (condition-case nil
          (forward-sexp)
        (error nil)))
    (condition-case nil
        (forward-sexp arg)
      (error nil))
    (backward-sexp))))

(defun limble-backward-sexp (&optional arg)
  "Move to beginning of previous S-expression, irrespective of nesting."
  (interactive "p")
  (setq arg (or arg 1))
  (dotimes (var arg)
    (cond
     ((or (paredit-in-string-p) (paredit-in-comment-p)
          (and (char-after) (eq (char-syntax (char-after)) ?<)))
      (backward-word))
     (t
      (skip-syntax-backward " )>")
      (if (and (char-before) (eq (char-syntax (char-before)) ?\())
          (backward-char)
        (backward-sexp))))))

(defun limble-backward-Sexp (&optional arg)
  "Move to beginning of previous S-expression, respective of nesting."
  (interactive "p")
  (setq arg (or arg 1))
  (cond
   ((or (paredit-in-string-p) (paredit-in-comment-p)
        (and (char-after) (eq (char-syntax (char-after)) ?<)))
    (backward-word arg))
   (t
    (unless (or (eobp) (eq (char-syntax (char-after)) ?\())
      (forward-char))
    (backward-sexp arg))))

(defun limble-end-of-sexp (&optional arg)
  "Move to end of next S-expression, irrespective of nesting."
  (interactive "p")
  (setq arg (or arg 1))
  (dotimes (var arg)
    (cond
     ((or (paredit-in-string-p) (paredit-in-comment-p)
          (and (char-after) (eq (char-syntax (char-after)) ?<)))
      (forward-word))
     (t
      (limble-with-next-char
        (skip-syntax-forward " (<")
        (if (and (char-after) (eq (char-syntax (char-after)) ?\)))
            (forward-char)
          (forward-sexp)))))))

(defun limble-end-of-Sexp (&optional arg)
  "Move to end of next S-expression, respective of nesting."
  (interactive "p")
  (setq arg (or arg 1))
  (cond
   ((or (paredit-in-string-p) (paredit-in-comment-p)
        (and (char-after) (eq (char-syntax (char-after)) ?<)))
    (forward-word arg))
   (t
    ;; TODO: Make a separate Operator-Pending command.
    (unless (or (eq viper-current-state 'operator-state)
                (and (char-after) (eq (char-syntax (char-after)) ?\()))
      (forward-char))
    (skip-syntax-forward " )>")
    (forward-sexp arg)
    (backward-char))))
(put 'limble-end-of-Sexp 'motion-type 'inclusive)

(vimpulse-define-text-object limble-select-sexp (arg)
  "Select a S-expression."
  (vimpulse-inner-object-range
   arg
   'backward-sexp
   'forward-sexp))

(vimpulse-define-text-object limble-select-upper-sexp (arg)
  "Select containing S-expression."
  (limble-backward-up-list)
  (vimpulse-inner-object-range
   arg
   'backward-sexp
   'forward-sexp))

(defun limble-up-list (&optional arg)
  "Like `up-list', but breaks out of strings."
  (interactive "p")
  (let ((opoint (point)))
    (setq arg (or arg 1))
    (unless (region-active-p)
      (push-mark nil t nil))
    (cond
     ((and (char-after)
           (eq (char-syntax (char-after) )  ?\())
      (forward-sexp)
      (backward-char))
     (t
      (up-list arg)
      (backward-char)
      (when (eq opoint (point))
        (up-list)
        (skip-syntax-forward " "))))))

(defun limble-backward-up-list (&optional arg)
  "Like `backward-up-list', but breaks out of strings."
  (interactive "p")
  (let ((opoint (point)))
    (setq arg (or arg 1))
    (unless (region-active-p)
      (push-mark nil t nil))
    (while (progn
             (condition-case nil
                 (backward-up-list arg)
               (error nil))
             (when (eq opoint (point))
               (backward-char)
               (setq opoint (point)) t)))))

(defun limble-down-list (&optional arg)
  "Move forward down one level of parentheses."
  (interactive "p")
  (setq arg (or arg 1))
  (down-list arg))

(defun limble-backward-down-list (&optional arg)
  "Move backward down one level of parentheses."
  (interactive "p")
  (setq arg (or arg 1))
  (down-list (- arg)))

(defun limble-indent (&optional arg)
  "Indent line and S-expression, if any."
  (interactive "p")
  (setq arg (or arg 1))
  (cond
   ((region-active-p)
    (indent-region (region-beginning) (region-end)))
   (t
    (indent-according-to-mode)
    (condition-case nil
        (indent-sexp)
      (error nil)))))

(defun limble-ret (&optional arg)
  "Insert newline, push closing parens onto next line and indent."
  (interactive "p")
  (setq arg (or arg 1))
  (when (and (char-after)
             (eq (char-syntax (char-after)) ?\)))
    (save-excursion
      (newline-and-indent)))
  (newline arg)
  (limble-indent))

(defun limble-insert-above (&optional arg)
  "Insert above S-expression."
  (interactive "p")
  (setq arg (or arg 1))
  (when (and (char-after)
             (eq (char-syntax (char-after)) ?\)))
    (forward-char))
  (split-line arg)
  (viper-insert nil))

(defun limble-insert-before (&optional arg)
  "Insert before S-expression."
  (interactive "p")
  (setq arg (or arg 1))
  (if (looking-back "^[[:space:]]*")
      (limble-indent)
    (unless (or (looking-back "[[:space:]]+")
                (and (char-before)
                     (eq (char-syntax (char-before)) ?\()))
      (insert " ")))
  (unless (looking-at "[[:space:]]+")
    (save-excursion
      (insert " ")))
  (viper-insert nil))

(defun limble-insert-after (&optional arg)
  "Insert after S-expression."
  (interactive "p")
  (setq arg (or arg 1))
  (if (and (char-after) (eq (char-syntax (char-after)) ?\)))
      (forward-char)
    (forward-sexp arg))
  (insert " ")
  (viper-insert nil))

(defun limble-insert-below (&optional arg)
  "Insert below S-expression."
  (interactive "p")
  (setq arg (or arg 1))
  (skip-syntax-forward " ")
  (if (and (char-after)
           (eq (char-syntax (char-after)) ?\)))
      (forward-char)
    (condition-case nil
        (forward-sexp)
      (error nil)))
  (viper-insert nil)
  (newline arg)
  (limble-indent)
  (unless (looking-at "[[:space:]]*\$")
    (split-line)))

(defun limble-join (beg end)
  "Join lines."
  (interactive (vimpulse-range nil nil t nil 'vimpulse-line))
  (let ((num (count-lines beg end)))
    (dotimes (var num)
      (delete-indentation t))))

(defun limble-wrap (&optional arg)
  "Wrap the following S-expression in parentheses.
Enter Insert mode."
  (interactive "P")
  (paredit-wrap-round arg)
  (viper-insert nil))

(defun limble-splice (&optional arg)
  "Remove containing delimiters."
  (interactive "p")
  (paredit-splice-sexp arg)
  (viper-move-marker-locally 'viper-com-point (point)))

(defun limble-transpose (&optional arg)
  "Interchange S-expressions around point."
  (interactive "p")
  (setq vimpulse-inhibit-operator t)
  (transpose-sexps arg))

(defun limble-transpose-backwards (&optional arg)
  "Interchange S-expressions around point, to the left."
  (interactive "p")
  (setq vimpulse-inhibit-operator t)
  (transpose-sexps (- arg)))

;; Utilities.
(defmacro limble-with-next-char (&rest body)
  "Execute BODY after current character.
Move back a character afterwards."
  (declare (indent defun))
  `(unwind-protect
       (progn
         (unless (eobp)
           (forward-char))
         ,@body)
     (unless (bobp)
       (backward-char))))

(when (fboundp 'font-lock-add-keywords)
  (font-lock-add-keywords
   'emacs-lisp-mode
   '(("(\\(limble-with-next-char\\)\\>" 1 font-lock-keyword-face))))

(defun limble-delete-blank (&optional following)
  "Delete blank lines.
This is just a customized version of `delete-blank-lines'."
  (interactive "*")
  (let (thisblank singleblank)
    (save-excursion
      (beginning-of-line)
      (setq thisblank (looking-at "[ \t]*$"))
      ;; Set singleblank if there is just one blank line here.
      (setq singleblank
            (and thisblank
                 (not (looking-at "[ \t]*\n[ \t]*$"))
                 (or (bobp)
                     (progn (forward-line -1)
                            (not (looking-at "[ \t]*$")))))))
    ;; Delete preceding blank lines, and this one too if it's the only one.
    (if thisblank
        (progn
          (beginning-of-line)
          (if singleblank (forward-line 1))
          (delete-region (point)
                         (if (re-search-backward "[^ \t\n]" nil t)
                             (progn (forward-line 1) (point))
                           (point-min)))))
    ;; Delete following blank lines unless there is only one.
    (if (or following (and thisblank (not singleblank)))
        (save-excursion
          (end-of-line)
          (forward-line 1)
          (delete-region (point)
                         (if (re-search-forward "[^ \t\n]" nil t)
                             (progn (beginning-of-line) (point))
                           (point-max)))))
    ;; Handle the special case where point is followed by newline and eob.
    ;; Delete the line, leaving point at eob.
    (if (looking-at "^[ \t]*\n\\'")
        (delete-region (point) (point-max)))))

;; Index movement commands.
(dolist (cmd '(limble-forward-sexp
               limble-forward-Sexp
               limble-backward-sexp
               limble-backward-Sexp
               limble-end-of-sexp
               limble-end-of-Sexp
               limble-up-list
               limble-backward-up-list
               limble-down-list
               limble-backward-down-list))
  (add-to-list 'vimpulse-movement-cmds cmd))

(eval-after-load 'eldoc
  '(apply 'eldoc-add-command
          '(limble-forward-sexp
            limble-forward-Sexp
            limble-backward-sexp
            limble-backward-Sexp
            limble-end-of-sexp
            limble-end-of-Sexp
            limble-up-list
            limble-backward-up-list
            limble-down-list
            limble-backward-down-list
            limble-indent
            limble-ret
            limble-insert-above
            limble-insert-before
            limble-insert-after
            limble-insert-below
            limble-join
            limble-wrap
            limble-splice
            limble-transpose
            limble-transpose-backwards)))

(provide 'vimpulse-limble)

;;; vimpulse-limble.el ends here
