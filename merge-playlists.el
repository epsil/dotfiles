#!/usr/bin/emacs --script

;; ":"; exec emacs -Q --script "$0" "$@" # -*-emacs-lisp-*-
;; http://stackoverflow.com/questions/6238331/emacs-shell-scripts-how-to-put-initial-options-into-the-script

(require 'cl) ; `flet', `reduce'

(defvar args nil)

(defvar output-file nil
  "Output file.")

(defvar merge-func #'playlists-merge-shuffle
  "The default merge function.")

(defvar merge-offset 0
  "Merge offset.")

(defalias #'playlists-insert #'playlists-join)
(defalias #'playlists-append #'playlists-join)
(defalias #'playlists-concat #'playlists-join)
(defalias #'playlists-concatenate #'playlists-join)
(defalias #'playlists-random #'playlists-shuffle)
(defalias #'playlists-randomize #'playlists-shuffle)
(defalias #'playlists-interleave #'playlists-merge)
(defalias #'playlists-shuffle-merge #'playlists-merge-shuffle)
(defalias #'playlists-shuffle-interleave #'playlists-merge-shuffle)
(defalias #'playlists-interleave-shuffle #'playlists-merge-shuffle)
(defalias #'playlists-unique-merge #'playlists-merge-unique)
(defalias #'playlists-unique-interleave #'playlists-merge-unique)
(defalias #'playlists-interleave-unique #'playlists-merge-unique)
(defalias #'playlists-overlay #'playlists-merge-overlay)
(defalias #'playlists-overlay-merge #'playlists-merge-overlay)
(defalias #'playlists-overlay-interleave #'playlists-merge-overlay)
(defalias #'playlists-interleave-overlay #'playlists-merge-overlay)

(defun playlists-join (&rest xs)
  "Chain playlists together."
  (apply #'append xs))

(defun playlists-shuffle (&rest xs)
  "Create a randomized playlist."
  (shuffle-list-fairly (apply #'playlists-join xs)))

(defun playlists-merge (&rest xs)
  "Interleave playlists by alternating between them."
  (let ((xs (delete nil xs)))
    (cond
     ((null xs)
      xs)
     (t
      (append (mapcar #'car xs)
              (apply #'playlists-merge (mapcar #'cdr xs)))))))

(defun playlists-merge-shuffle (&rest xs)
  "Interleave playlists by randomly alternating between them."
  (let ((xs (delete '() xs)))
    (cond
     ((null xs)
      xs)
     (t
      (let* ((n (fair-random (length xs)))
             (x (car (nth n xs))))
        (setcar (nthcdr n xs) (cdr (nth n xs)))
        (cons x (apply #'playlists-merge-shuffle xs)))))))

;; (defun playlists-merge-shuffle (&rest xs)
;;   "Interleave playlists by randomly alternating between them."
;;   (let ((xs (delete '() xs)))
;;     (cond
;;      ((null xs)
;;       xs)
;;      (t
;;       (let ((xs (shuffle-list-fairly xs)))
;;         (cons (car (car xs))
;;               (apply #'playlists-merge-shuffle
;;                      (cons (cdr (car xs)) (cdr xs)))))))))

(defun playlists-merge-unique (xs ys &rest zs)
  "Interleave two playlists in preference of unique elements.
Elements unique to XS are picked over elements unique to YS,
which are picked over common elements. Common elements are
picked from XS and removed from YS. The general case is a left fold,
i.e., (merge (merge (merge xs ys) zs) ...).

Pseudo-code:

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
    (reduce #'playlists-merge-unique `(,xs ,ys ,@zs)))
   ((null xs)
    ys)
   ((null ys)
    xs)
   ;; unique xs element
   ((not (member (car xs) ys)) ; TODO: lookup original XS?
    (cons (car xs)
          (playlists-merge-unique (cdr xs) ys)))
   ;; unique ys element
   ((not (member (car ys) xs))
    (cons (car ys)
          (playlists-merge-unique xs (cdr ys))))
   ;; common element
   (t
    (cons (car xs)
          (playlists-merge-unique (cdr xs)
                                  (delete (car xs) ys))))))

;; (defun playlists-merge-unique (xs ys &rest zs)
;;   "Interleave two playlists in preference of unique elements.
;; Elements unique to XS are picked over elements unique to YS,
;; which are picked over common elements. Common elements are
;; picked from XS and removed from YS. The general case is a left fold,
;; i.e., (merge (merge (merge xs ys) zs) ...).
;;
;; Pseudo-code:
;;
;; -- unique x
;; merge (x:xs) ys =
;; x:merge xs ys
;;
;; -- unique y
;; merge xs y:ys =
;; x:merge xs ys
;;
;; -- shared element
;; merge (x:xs) (ys1 ++ [x] ++ ys2) =
;; x:merge xs (ys1 ++ ys2)"
;;   (flet ((overlay (xs ys xs-orig ys-orig)
;;             (cond
;;              ((null xs)
;;               ys)
;;              ((null ys)
;;               xs)
;;              ;; unique xs element
;;              ((not (member (car xs) ys-orig))
;;               (cons (car xs)
;;                     (merge (cdr xs) ys xs-orig ys-orig)))
;;              ;; unique ys element
;;              ((not (member (car ys) xs))
;;               (cons (car ys)
;;                     (merge xs (cdr ys) xs-orig ys-orig)))
;;              ;; common element
;;              (t
;;               (cons (car xs)
;;                     (merge (cdr xs)
;;                            (delete (car xs) ys)
;;                            xs-orig ys-orig))))))
;;     (cond
;;      (zs
;;       (reduce #'playlists-merge-overlay `(,xs ,ys ,@zs)))
;;      (t
;;       (merge xs ys xs)))))

(defun playlists-merge-overlay (xs ys &rest zs)
  "Interleave two playlists by overlaying unique elements.
Elements from YS are only picked if they are unique.
Non-unique YS elements are ignored, and an element
from XS is picked instead. Thus, the unique elements of YS
are \"overlaid\" onto XS. The general case is a left fold,
i.e., (merge (merge (merge xs ys) zs) ...).

Pseudo-code:

-- unique y
merge xs y:ys =
x:merge xs ys

-- shared element
merge (x:xs) (x:xs)
x:merge xs ys"
  (flet ((overlay (xs ys lst)
                  ;; internal function for keeping track of
                  ;; the original XS list (stored in LST)
                  (cond
                   ((null xs)
                    ys)
                   ((null ys)
                    xs)
                   ;; unique ys element
                   ((not (member (car ys) lst))
                    (cons (car ys)
                          (overlay xs (cdr ys) lst)))
                   ;; common element
                   (t
                    (cons (car xs)
                          (overlay (cdr xs)
                                   (cdr ys)
                                   lst))))))
    (cond
     (zs
      (reduce #'playlists-merge-overlay `(,xs ,ys ,@zs)))
     (t
      (overlay xs ys xs)))))

;; Utility functions

(defvar fair-random-value -1)
(defun fair-random (&optional limit)
  "Fair random value."
  (let (val)
    (if (or (not (numberp limit)) (< limit 3))
        (setq fair-random-value (random limit))
      ;; avoid repetition
      (while (progn
               (setq val (random limit))
               (= val fair-random-value)))
      (setq fair-random-value val))))

(defun shuffle-list (list)
  "Randomly permute the elements of LIST."
  (let ((vector (vconcat list)))
    (setq vector (shuffle-vector vector))
    (append vector nil)))

(defun shuffle-list-fairly (list)
  "Randomly permute the elements of LIST.
Ensure that a different list is returned."
  (if (< (length list) 3)
      (shuffle-list list)
    (let (newlist)
      (while (progn
               (setq newlist (shuffle-list list))
               (equal newlist list)))
      newlist)))

(defun read-lines (file &optional newline)
  "Read FILE as a list of lines, splitting on NEWLINE."
  (let ((newline (or newline "\n")))
    (with-temp-buffer
      (insert-file-contents file)
      (split-string (buffer-string) newline t))))

(defun write-string-to-file (string file)
  "Write STRING to FILE."
  (let (message-log-max)
    (with-temp-buffer
      (insert string)
      (when (file-writable-p file)
        (write-region (point-min) (point-max) file)))))

;; give us some stack
(setq max-lisp-eval-depth 10000
      max-specpdl-size 10000)

;; seed the random-number generator
(random t)

;; parse command line options
(while argv
  (cond
   ((member (car argv) '("--merge-func" "--method" "-m"))
    (pop argv)
    (setq merge-func
          (intern (format "playlists-%s" (downcase (pop argv))))))
   ((member (car argv) '("--offset"))
    (pop argv)
    (setq merge-offset (string-to-number (pop argv))))
   ((member (car argv) '("--output" "-o"))
    (pop argv)
    (setq output-file (pop argv)))
   (t
    (setq args (append args (list (pop argv)))))))

(cond
 ((null args)
  (message "No input arguments"))
 ((= (length args) 1)
  (message "Too few arguments"))
 (t
  (let* ((playlists (mapcar #'read-lines args))
         ;; protect the first elements of the first playlist
         ;; according to `merge-offset'
         (head (subseq (car playlists) 0 merge-offset))
         (tail (nthcdr merge-offset (car playlists)))
         (playlists (cons tail (cdr playlists)))
         ;; apply merge function and merge with offset elements
         (playlist (append head (apply merge-func playlists)))
         (string (mapconcat #'identity playlist "\n")))
    (if output-file
        (write-string-to-file string output-file)
      (message string)))))
