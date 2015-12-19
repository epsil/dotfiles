;; (eval `(defun foo () nil))

(defun foo ()
  (foo))


(defun bar ()
  (foo))

(provide 'foo)

;;; foo.el ends here
