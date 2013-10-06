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

(define (playlists-merge-shuffle-fair . xs)
  (let ((xs (shuffle-fairly (remove '() xs))))
    (cond
     ((null? xs)
      xs)
     (else
      (append (map car xs) ; fair
              (apply playlists-merge-shuffle (map cdr xs)))))))

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
