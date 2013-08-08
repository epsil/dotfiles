#! /bin/sh
":"; exec emacs -Q --script "$0" "$@" # -*-emacs-lisp-*-

;; #!/usr/bin/emacs --script
;;
;; To suppress loading messages, we use the following hack:
;; http://stackoverflow.com/questions/6238331/emacs-shell-scripts-how-to-put-initial-options-into-the-script

(require 'cl)

(defvar args nil)

(defvar merge-func #'playlists-interleave
  "The default merge function.")

(defun playlists-append (&rest xs)
  "Append two or more playlists."
  (apply #'append xs))

(defun playlists-interleave (&rest xs)
  "Zip two or more playlists."
  (let ((xs (delete nil xs)))
    (cond
     ((null xs)
      xs)
     ((append (mapcar #'car xs)
              (apply #'playlists-interleave (mapcar #'cdr xs)))))))

(defun playlists-merge (xs ys &rest zs)
  "Merge two playlists according to the following rules.
The general case is a left fold, i.e.,
\(merge (merge (merge xs ys) zs) ...).

-- unique x
merge (x:xs) ys =
      x:merge xs ys

-- unique y
merge xs y:ys =
      x:merge xs ys

-- shared element
merge (x:xs) (ys1 ++ [x] ++ ys2) =
      x:merge xs (ys1 ++ ys2)"
  (cond
   (zs
    (reduce #'playlists-merge (append (list xs ys) zs)))
   ((null xs)
    ys)
   ((null ys)
    xs)
   ;; unique xs element
   ((not (member (car xs) ys))
    (cons (car xs)
          (playlists-merge (cdr xs)
                           ys)))
   ;; unique ys element
   ((not (member (car ys) xs))
    (cons (car ys)
          (playlists-merge xs
                           (cdr ys))))
   ;; common element
   (t
    (cons (car xs)
          (playlists-merge (cdr xs)
                           (delete (car xs) ys))))))

(defun playlists-overlay (xs ys &rest zs)
  "Merge two playlists according to the following rules.
The general case is a left fold, i.e.,
\(merge (merge (merge xs ys) zs) ...).

-- unique y
merge xs y:ys =
      x:merge xs ys

-- shared element
merge (x:xs) (x:xs)
      x:merge xs ys"
  (cond
   (zs
    (reduce #'playlists-overlay (append (list xs ys) zs)))
   (t
    (playlists-overlay-1 xs ys xs))))

(defun playlists-overlay-1 (xs ys lst)
  "Internal function for `playlists-overlay'."
  (cond
   ((null xs)
    ys)
   ((null ys)
    xs)
   ;; unique ys element
   ((not (member (car ys) lst))
    (cons (car ys)
          (playlists-overlay-1 xs (cdr ys) lst)))
   ;; common element
   (t
    (cons (car xs)
          (playlists-overlay-1 (cdr xs)
                               (cdr ys)
                               lst)))))

(defun read-lines (file)
  "Return the contents of FILE as a list of lines."
  (with-temp-buffer
    (insert-file-contents file)
    (split-string (buffer-string) "\n" t)))

;; give us some stack
(setq max-lisp-eval-depth 10000)

;; parse command line options
(while argv
  (cond
   ((member (car argv) '("--merge-func" "--method" "-m"))
    (pop argv)
    (setq merge-func (intern (format "playlists-%s" (pop argv)))))
   (t
    (setq args (append args (list (pop argv)))))))

(cond
 ((null args)
  (message "No input arguments"))
 ((= (length args) 1)
  (message "Too few arguments"))
 (t
  (let* ((playlists (mapcar #'read-lines args))
         (playlist (apply merge-func playlists)))
    (message (mapconcat #'identity playlist "\n")))))
