;; (require 'rainbow-delimiters)

(defvar columns nil)

(defun rainbow-delimiters-fontify (beg end)
  (save-excursion
    (goto-char beg)
    (while (< (point) end)
      (cond
       ((null (char-after))
        nil)
       ((paredit-in-string-p)
        nil)
       ((paredit-in-comment-p)
        nil)
       ((and (char-before)
             (eq (char-syntax (char-before)) ?\\))
        nil)
       ((eq (char-syntax (char-after)) ?\()
        (push (current-column) columns))
       ((eq (char-syntax (char-after)) ?\))
        (pop columns))
       ((and (eq (char-syntax (char-after)) ?\ )
             (memq (current-column) columns))
        (let ((o (make-overlay (point) (1+ (point)))))
          (overlay-put o 'face 'fringe)
          (overlay-put o 'evaporate t))))
      (forward-char 1))))

(jit-lock-register 'rainbow-delimiters-fontify)

(provide 'rainbow-delimiters)
