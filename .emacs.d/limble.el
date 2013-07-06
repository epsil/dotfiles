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
(require 'evil)

(defconst limble-version 0.1)

(defun limble-version ()
  (interactive)
  (message "limble version is %s" limble-version))

(define-minor-mode limble-mode
  "limble-mode provides modal bindings for editing Lisp code."
  :keymap (make-sparse-keymap))

(defun turn-on-limble-mode ()
  "Enable limble-mode."
  (interactive)
  (limble-mode 1))

;; Normal state bindings
(evil-define-key 'normal limble-mode-map "Q" 'limble-indent)
(evil-define-key 'normal limble-mode-map "gi" 'limble-insert-before)
(evil-define-key 'normal limble-mode-map "ga" 'limble-insert-after)
(evil-define-key 'normal limble-mode-map "gO" 'limble-insert-above)
(evil-define-key 'normal limble-mode-map "go" 'limble-insert-below)
(evil-define-key 'normal limble-mode-map "gw" 'limble-wrap)

;; Motion state bindings
(evil-define-key 'motion limble-mode-map "w" 'limble-forward-sexp)
(evil-define-key 'motion limble-mode-map "W" 'limble-forward-Sexp)
(evil-define-key 'motion limble-mode-map "e" 'limble-end-of-sexp)
(evil-define-key 'motion limble-mode-map "E" 'limble-end-of-Sexp)
(evil-define-key 'motion limble-mode-map "b" 'limble-backward-sexp)
(evil-define-key 'motion limble-mode-map "B" 'limble-backward-Sexp)
(evil-define-key 'motion limble-mode-map "gh" 'limble-backward-down-list)
(evil-define-key 'motion limble-mode-map "gj" 'limble-down-list)
(evil-define-key 'motion limble-mode-map "gk" 'limble-backward-up-list)
(evil-define-key 'motion limble-mode-map "gl" 'limble-up-list)
(evil-define-key 'motion limble-mode-map "(" 'limble-backward-up-list)
(evil-define-key 'motion limble-mode-map ")" 'limble-up-list)
(evil-define-key 'motion limble-mode-map "aw" 'limble-select-sexp)
(evil-define-key 'motion limble-mode-map "iw" 'limble-select-sexp)

;; Operator-Pending state bindings
(evil-define-key 'operator limble-mode-map "w" 'limble-end-of-Sexp)
(evil-define-key 'operator limble-mode-map "e" 'limble-end-of-Sexp)
(evil-define-key 'operator limble-mode-map "b" 'limble-backward-Sexp)
(evil-define-key 'operator limble-mode-map "s" 'limble-splice)
(evil-define-key 'operator limble-mode-map "x" 'limble-transpose)
(evil-define-key 'operator limble-mode-map "X" 'limble-transpose-backwards)

;; Insert state bindings
(evil-define-key 'insert limble-mode-map (kbd "RET") 'limble-ret)

(evil-define-motion limble-forward-sexp (arg)
  "Move to beginning of next S-expression, irrespective of nesting."
  (setq arg (or arg 1))
  (dotimes (var arg)
    (cond
     ((or (paredit-in-string-p) (paredit-in-comment-p))
      (evil-forward-word-begin arg))
     ((and (char-after) (eq (char-syntax (char-after)) ?<))
      (evil-forward-word-begin arg))
     (t
      (unless (> (skip-syntax-forward " )>") 0)
        (if (and (char-after) (memq (char-syntax (char-after)) '(?\( ?')))
            (forward-char)
          (forward-sexp))
        (skip-syntax-forward " )>"))))))

(evil-define-motion limble-forward-Sexp (arg)
  "Move to beginning of next S-expression, respective of nesting."
  (setq arg (or arg 1))
  (cond
   ((or (paredit-in-string-p) (paredit-in-comment-p)
        (and (char-after) (eq (char-syntax (char-after)) ?<)))
    (evil-forward-word-begin arg))
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

(evil-define-motion limble-backward-sexp (arg)
  "Move to beginning of previous S-expression, irrespective of nesting."
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

(evil-define-motion limble-backward-Sexp (arg)
  "Move to beginning of previous S-expression, respective of nesting."
  (setq arg (or arg 1))
  (cond
   ((or (paredit-in-string-p) (paredit-in-comment-p)
        (and (char-after) (eq (char-syntax (char-after)) ?<)))
    (backward-word arg))
   (t
    (unless (or (eobp) (eq (char-syntax (char-after)) ?\())
      (forward-char))
    (backward-sexp arg))))

(evil-define-motion limble-end-of-sexp (arg)
  "Move to end of next S-expression, irrespective of nesting."
  (setq arg (or arg 1))
  (dotimes (var arg)
    (cond
     ((or (paredit-in-string-p) (paredit-in-comment-p)
          (and (char-after) (eq (char-syntax (char-after)) ?<)))
      (evil-forward-word-end arg))
     (t
      (limble-with-next-char
        (skip-syntax-forward " (<")
        (if (and (char-after) (eq (char-syntax (char-after)) ?\)))
            (forward-char)
          (forward-sexp)))))))

(evil-define-motion limble-end-of-Sexp (arg)
  "Move to end of next S-expression, respective of nesting."
  :type inclusive
  (setq arg (or arg 1))
  (cond
   ((or (paredit-in-string-p) (paredit-in-comment-p)
        (and (char-after) (eq (char-syntax (char-after)) ?<)))
    (evil-forward-word-begin arg)
    (setq evil-this-type 'exclusive))
   (t
    (unless (or (evil-operator-state-p)
                (and (char-after) (eq (char-syntax (char-after)) ?\()))
      (forward-char))
    (skip-syntax-forward " )>")
    (forward-sexp arg)
    (backward-char))))

(evil-define-text-object limble-select-sexp (arg)
  "Select a S-expression."
  (evil-inner-object-range
   arg
   'forward-sexp
   'backward-sexp))

(evil-define-text-object limble-select-upper-sexp (arg)
  "Select containing S-expression."
  (limble-backward-up-list)
  (evil-inner-object-range
   arg
   'forward-sexp
   'backward-sexp))

(evil-define-motion limble-up-list (arg)
  "Like `up-list', but breaks out of strings."
  :jump t
  (let ((opoint (point)))
    (setq arg (or arg 1))
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

(evil-define-motion limble-backward-up-list (arg)
  "Like `backward-up-list', but breaks out of strings."
  :jump t
  (let ((opoint (point)))
    (setq arg (or arg 1))
    (while (progn
             (condition-case nil
                 (backward-up-list arg)
               (error nil))
             (when (eq opoint (point))
               (backward-char)
               (setq opoint (point)) t)))))

(evil-define-motion limble-down-list (arg)
  "Move forward down one level of parentheses."
  (setq arg (or arg 1))
  (down-list arg))

(evil-define-motion limble-backward-down-list (arg)
  "Move backward down one level of parentheses."
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
    (unless (or (paredit-in-string-p) (paredit-in-comment-p))
      (condition-case nil
          (indent-sexp)
        (error nil))))))

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
  (evil-insert nil))

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
  (evil-insert nil))

(defun limble-insert-after (&optional arg)
  "Insert after S-expression."
  (interactive "p")
  (setq arg (or arg 1))
  (if (and (char-after) (eq (char-syntax (char-after)) ?\)))
      (forward-char)
    (forward-sexp arg))
  (insert " ")
  (evil-insert nil))

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
  (evil-insert nil)
  (newline arg)
  (limble-indent)
  (unless (looking-at "[[:space:]]*\$")
    (split-line)))

(defun limble-wrap (&optional arg)
  "Wrap the following S-expression in parentheses.
Enter Insert mode."
  (interactive "P")
  (paredit-wrap-round arg)
  (evil-insert nil))

(defun limble-splice (&optional arg)
  "Remove containing delimiters."
  (interactive "p")
  (paredit-splice-sexp arg)
  ;; (set-marker evil-motion-marker (point))
  (set-mark (point)))

(defun limble-transpose (&optional arg)
  "Interchange S-expressions around point."
  (interactive "p")
  (setq evil-inhibit-operator t)
  (transpose-sexps arg))

(defun limble-transpose-backwards (&optional arg)
  "Interchange S-expressions around point, to the left."
  (interactive "p")
  (setq evil-inhibit-operator t)
  (transpose-sexps (- arg)))

;; Utilities
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

(provide 'limble)

;;; limble.el ends here
