#! /usr/bin/racket
#lang racket

;; Merge two or more M3U playlists together. Concatenate, interleave
;; or shuffle the tracks.
;;
;; Usage:
;;
;;     ./merge-playlists.rkt in1.m3u in2.m3u > out.m3u
;;
;; Or with the -o option:
;;
;;     ./merge-playlists.rkt -o out.m3u in1.m3u in2.m3u
;;
;; The -m option is used to select the merging algorithm.
;; The default is MERGE-SHUFFLE.
;;
;;     ./merge-playlists.rkt -m JOIN in1.m3u in2.m3u > out.m3u
;;     ./merge-playlists.rkt -m SHUFFLE in1.m3u in2.m3u > out.m3u
;;     ./merge-playlists.rkt -m MERGE in1.m3u in2.m3u > out.m3u
;;     ./merge-playlists.rkt -m MERGE-UNIQUE in1.m3u in2.m3u > out.m3u
;;     ./merge-playlists.rkt -m OVERLAY in1.m3u in2.m3u > out.m3u
;;
;; To install: chmod +x, symlink to /usr/local/bin/merge-playlists
;; and invoke without the .rkt suffix.

;; TODO: remove duplicates

(require srfi/13)
(require racket/dict)
(require racket/mpair)

;; Chain playlists together
(define (playlists-join . xs)
  (apply append xs))

;; Create a randomized playlist
(define (playlists-shuffle . xs)
  (shuffle-fairly (apply playlists-join xs)))

(define (playlists-shuffle-fair . xs)
  (apply playlists-merge-shuffle-fair (map playlists-shuffle xs)))

;; Interleave playlists by alternating between them
(define (playlists-merge . xs)
  (let ((xs (remove '() xs)))
    (cond
     ((null? xs)
      xs)
     (else
      (append (map car xs)
              (apply playlists-merge (map cdr xs)))))))

;; Interleave playlists by randomly alternating between them
(define (playlists-merge-shuffle . xs)
  (let ((xs (shuffle-fairly (remove '() xs))))
    (cond
     ((null? xs)
      xs)
     (else
      (cons (car (car xs))
            (apply playlists-merge-shuffle
                   (cons (cdr (car xs)) (cdr xs))))))))

;; Interleave playlists by randomly alternating between them fairly
(define (playlists-merge-shuffle-fair . xs)
  (let ((xs (shuffle-fairly (remove '() xs))))
    (cond
     ((null? xs)
      xs)
     (else
      (append (map car xs) ; fair
              (apply playlists-merge-shuffle (map cdr xs)))))))

;; Split a playlist into several playlists (artists, albums, etc.)
(define (playlists-split . xs)
  (let* ((xs (apply playlists-join xs))
         (prefix (string-prefix xs)))
    (define (find-key str)
      (let ((regexp (format "^~a([^/]+)" (regexp-quote prefix))))
        (match (regexp-match regexp str)
          [(list prefix key matches ...)
           key]
          [_ ""])))
    ;; (define (insert lst key val)
    ;;   (let ((pair (massoc key lst)))
    ;;     (match pair
    ;;       [(mcons key v)
    ;;        (set-mcdr! pair (mappend v (mlist val)))
    ;;        lst]
    ;;       [_ (mappend lst (mlist (mcons key (mlist val))))])))
    (define (insert lst key val)
      (if (assoc key lst)
          (map (lambda (pair)
                 (if (equal? (car pair) key)
                     (cons key (append (cdr pair) (list val)))
                     pair))
               lst)
          (append lst (list (cons key (list val))))))
    (define (split xs acc)
      (if (null? xs)
          (map cdr acc)
          (let* ((x (car xs))
                 (xs (cdr xs))
                 (key (find-key x)))
            (split xs (insert acc key x)))))
    (split xs '())))

;; Interleave m playlists at once
(define (playlists-merge-window m . xs)
  (define (merge window queue)
    (cond
     ((member '() window)
      (merge (remove '() window) queue))
     ((and (not (null? queue))
           (or (< (length window) m) (<= m 0)))
      (merge (append window (list (car queue)))
             (cdr queue)))
     ((null? window)
      '())
     (else
      (append (map car window)
              (merge (map cdr window) queue)))))
  (merge '() xs))

;; Randomly interleave m playlists at once
(define (playlists-merge-window-shuffle m . xs)
  (define (merge window queue)
    (cond
     ((member '() window)
      (merge (remove '() window) queue))
     ((and (not (null? queue))
           (or (< (length window) m) (<= m 0)))
      (merge (append window (list (car queue)))
             (cdr queue)))
     ((null? window)
      '())
     (else
      (let ((window (shuffle-fairly window)))
        (append (map car window)
                (merge (map cdr window) queue))))))
  (merge '() xs))

;; Trim playlists to n elements at a time
(define (playlists-trim n . xs)
  (define (trim xs)
    (cond
     ((null? xs)
      '())
     ((member '() xs)
      (trim (remove '() xs)))
     (else
      (append (map (lambda (lst)
                     (take lst (min n (length lst))))
                   xs)
              (trim (map (lambda (lst)
                           (drop lst (min n (length lst))))
                         xs))))))
  (trim xs))

;; Create an increasing buffer of playlists
(define (playlists-increasing-gradient m . xs)
  (define (gradient n xs)
    (cond
     ((null? xs)
      (values '() '()))
     ((> n m)
      (values '() xs))
     (else
      (let*-values (((x) (car xs))
                    ((y z) (split-at x (min n (length x))))
                    ((ys zs) (gradient (+ n 1) (cdr xs))))
        (values (cons y ys) (cons z zs))))))
  (let*-values (((y z) (gradient 1 xs)))
    (append y z)))

;; Create an decreasing buffer of playlists
(define (playlists-decreasing-gradient m . xs)
  (define (gradient n xs)
    (cond
     ((null? xs)
      (values '() '()))
     ((>= n m)
      (values '() xs))
     (else
      (let*-values (((x) (car xs))
                    ((y z) (split-at x (min (- m n) (length x))))
                    ((ys zs) (gradient (+ n 1) (cdr xs))))
        (values (cons y ys) (cons z zs))))))
  (let*-values (((y z) (gradient 0 xs)))
    (append y z)))

;; Interleave n tracks from m playlists at once
(define (playlists-merge-window-trimmed m n . xs)
  (apply playlists-merge-window m
         (apply playlists-trim n
                (apply playlists-decreasing-gradient m xs))))

;; Randomly interleave n tracks from m playlists at once
(define (playlists-merge-window-trimmed-shuffle m n . xs)
  (apply playlists-merge-window-shuffle m
         (apply playlists-trim n
                ;; (apply playlists-decreasing-gradient m xs)
                xs)))

;; "Normalize" a mixed playlist by merging five artists at a time
(define (playlists-normalize . xs)
  (apply playlists-merge-window-trimmed-shuffle 5 5
         (apply playlists-split xs)))

;; Interleave two playlists in preference of unique elements.
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
;; -- common element
;; merge (x:xs) (ys1 ++ [x] ++ ys2) =
;; x:merge xs (ys1 ++ ys2)
(define (playlists-merge-unique . xs)
  (define (merge ys-orig xs-orig)
    (define (merge2 xs ys)
      (cond
       ((null? xs)
        ys)
       ((null? ys)
        xs)
       ;; unique xs element
       ((not (member (car xs) ys-orig))
        (cons (car xs)
              (merge2 (cdr xs) ys)))
       ;; unique ys element
       ((not (member (car ys) xs))
        (cons (car ys)
              (merge2 xs (cdr ys))))
       ;; common element
       (else
        (cons (car xs)
              (merge2 (cdr xs)
                      (remove (car xs) ys))))))
    (merge2 xs-orig ys-orig))
  (foldl merge '() xs))

;; Interleave two playlists by overlaying unique elements.
;; Elements from YS are only picked if they are unique.
;; Non-unique YS elements are ignored, and an element
;; from XS is picked instead. Thus, the unique elements of YS
;; are "overlaid" onto XS. The general case is a left fold,
;; i.e., (merge (merge (merge xs ys) zs) ...).
;;
;; Pseudo-code:
;;
;; -- unique y
;; merge xs y:ys =
;; x:merge xs ys
;;
;; -- common element
;; merge (x:xs) (x:xs)
;; x:merge xs ys
(define (playlists-merge-overlay . xs)
  (define (overlay ys-orig xs-orig)
    (define (overlay2 xs ys)
      (cond
       ((null? xs)
        ys)
       ((null? ys)
        xs)
       ;; unique ys element
       ((not (member (car ys) xs-orig))
        (cons (car ys) (overlay2 xs (cdr ys))))
       ;; common element
       (else
        (cons (car xs) (overlay2 (cdr xs) (cdr ys))))))
    (overlay2 xs-orig ys-orig))
  (foldl overlay '() xs))

;; (playlists-decreasing-gradient 3
;;                 '("en" "to" "tre" "fire")
;;                 '("one" "two" "three" "four")
;;                 '("foo" "bar" "baz" "quux")
;;                 '("la" "dee" "da")
;;                 '("ein" "zwei" "drei")
;;                 '("jau" "jo" "jepp"))

;; (playlists-merge-window-shuffle 2
;;                 '("en" "to" "tre" "fire")
;;                 '("one" "two" "three" "four")
;;                 '("foo" "bar" "baz" "quux")
;;                 '("la" "dee" "da")
;;                 '("ein" "zwei" "drei")
;;                 '("jau" "jo" "jepp"))

;; (playlists-merge-window-trimmed-shuffle 3 3
;;                 '("en" "to" "tre" "fire")
;;                 '("one" "two" "three" "four")
;;                 '("foo" "bar" "baz" "quux")
;;                 '("la" "dee" "da")
;;                 '("ein" "zwei" "drei")
;;                 '("jau" "jo" "jepp"))

;; Utility functions

;; Randomly permute the elements of LST.
;; Ensure that a different list is returned.
(define (shuffle-fairly lst)
  (define (fair-shuffle lst)
    (let ((newlst (shuffle lst)))
      (if (equal? newlst lst)
          (fair-shuffle lst)
          newlst)))
  ;; ignore two-element lists since the only possible output is
  ;; (a b) => (b a), i.e., fair but not random
  (if (<= (length lst) 2)
      (shuffle lst)
      (fair-shuffle lst)))

;; Find the common string prefix of a list of strings.
(define (string-prefix lst)
  (cond
   ((null? lst)
    "")
   ((eq? (length lst) 1)
    (car lst))
   (else
    (foldl (lambda (str1 str2)
             (cond
              ((equal? str1 "")
               str2)
              ((equal? str2 "")
               str1)
              (else
               (let ((len (string-prefix-length str1 str2)))
                 (if (> len 0)
                     (substring str1 0 len)
                     "")))))
           "" lst))))

;; Command line parsing

(define (merge-func)
  (match (string-downcase (merge-arg))
    [(or "insert" "append" "concat" "concatenate" "join")
     playlists-join]
    [(or "random" "randomize" "shuffle")
     playlists-shuffle]
    [(or "random-fair" "randomize-fair" "shuffle-fair")
     playlists-shuffle-fair]
    [(or "interleave" "merge")
     playlists-merge]
    [(or "shuffle-merge" "shuffle-interleave" "interleave-shuffle" "merge-shuffle")
     playlists-merge-shuffle]
    [(or "shuffle-merge-fair" "shuffle-interleave-fair" "interleave-shuffle-fair" "merge-shuffle-fair")
     playlists-merge-shuffle-fair]
    [(or "unique-merge" "unique-interleave" "interleave-unique" "merge-unique")
     playlists-merge-unique]
    [(or "overlay" "overlay-merge" "overlay-interleave" "interleave-overlay" "merge-overlay")
     playlists-merge-overlay]
    [(or "normalize")
     playlists-normalize]
    [else
     playlists-merge-shuffle-fair]))

(define merge-arg (make-parameter ""))

(define output-file (make-parameter null))

(define input-files
  (command-line
   #:once-each
   [("-m" "--method" "--merge-func") mf
    "Merge function"
    (merge-arg mf)]
   [("-o" "--output" "--output-file") of
    "Output file"
    (output-file of)]
   #:args filename
   filename))

;; Seed the random number generator
(random-seed (current-milliseconds))

(define input-lists (map file->lines input-files))

(define output-list
  (apply (merge-func) input-lists))

(define output
  (string-append (string-join output-list "\n") "\n"))

;; Write to file or standard output
(cond
 ((not (null? (output-file)))
  (display-to-file output (output-file) #:exists 'replace))
 (else
  (display output)))
