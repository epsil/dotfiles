(require 'ert)
(require 'evil)

(defvar evil-ex2-current-cmd nil)
(defvar evil-ex2-current-buffer nil)
(defvar evil-ex2-current-range nil)
(defvar evil-ex2-grammar nil
  "Grammar for Ex.
Entries have the form (SYM DEF), where DEF is one or more production
rules. The first entry is the start symbol. See also `evil-parser'.")

;; cmdline.txt: If more line specifiers are given than required for
;; the command, the first one(s) will be ignored.
;;
;; Line numbers may be specified with:          *:range* *E14* *{address}*
;;      {number}        an absolute line number
;;      .               the current line                          *:.*
;;      $               the last line in the file                 *:$*
;;      %               equal to 1,$ (the entire file)            *:%*
;;      't              position of mark t (lowercase)            *:'*
;;      'T              position of mark T (uppercase); when the mark is in
;;                      another file it cannot be used in a range
;;      /{pattern}[/]   the next line where {pattern} matches     *:/*
;;      ?{pattern}[?]   the previous line where {pattern} matches *:?*
;;      \/              the next line where the previously used search
;;                      pattern matches
;;      \?              the previous line where the previously used search
;;                      pattern matches
;;      \&              the next line where the previously used substitute
;;                      pattern matches
;;
;; Each may be followed (several times) by '+' or '-' and an optional number.
;; This number is added or subtracted from the preceding line number.  If the
;; number is omitted, 1 is used.
(setq evil-ex2-grammar
      '((expression
         ((\? count) command (\? bang) (\? arguments)
          #'evil-ex2-call-command)
         (range command (\? bang) (\? arguments)
                #'evil-ex2-call-command-on-range)
         (line #'evil-goto-line))
        (count
         number)
        (command
         "[a-zA-Z_-]+" #'intern)
        (bang
         ((! space) "!" #'$1))
        (arguments
         string-arguments
         lisp-arguments)
        (string-arguments
         ((+ ("/" (! (\? space) "$") "\\(?:[\\].\\|[^/]\\)*" #'$2))
          (\? "/" (& (\? space) "$")) #'$1))
        (lisp-arguments
         #'evil-ex2-parse-arguments)
        (range
         (address (\? "[,;]" address #'$2) #'evil-ex2-range)
         ("%" #'(evil-ex2-full-range)))
        (address
         (line (\? offset) #'evil-ex2-address))
        (line
         number
         marker
         search
         ("\\^" #'(evil-ex2-first-line))
         ("\\$" #'(evil-ex2-last-line))
         ("\\." #'(evil-ex2-current-line)))
        (offset
         (+ signed-number #'+))
        (marker
         ("'" "[a-zA-Z_-<>']" #'(evil-ex2-marker $2)))
        (search
         forward
         backward
         next
         prev
         subst)
        (forward
         ("/" "\\(?:[\\].\\|[^/,; ]\\)+" (! "/")
          #'(evil-ex2-re-fwd $2))
         ("/" "\\(?:[\\].\\|[^/]\\)+" "/"
          #'(evil-ex2-re-fwd $2)))
        (backward
         ("\\?" "\\(?:[\\].\\|[^?,; ]\\)+" (! "\\?")
          #'(evil-ex2-re-bwd $2))
         ("\\?" "\\(?:[\\].\\|[^?]\\)+" "\\?"
          #'(evil-ex2-re-bwd $2)))
        (next
         "/" #'(evil-ex2-prev-search))
        (prev
         "\\?" #'(evil-ex2-prev-search))
        (subst
         "&" #'(evil-ex2-prev-search))
        (signed-number
         (sign (\? number) #'evil-ex2-signed-number))
        (sign
         "\\+\\|-" #'intern)
        (number
         "[0-9]+" #'string-to-number)
        (space
         "[ ]+")))

;; expression <- count? command bang? arguments?
;;             / range command bang? arguments?
;;             / line
;; count <- number
;; command <- [a-zA-Z_-]
;; bang <- "!"
;; arguments <- string-arguments
;;            / lisp-arguments
;; ...
;; range <- address ([,;] adress)?
;; address <- line offset?
;; line <- number
;;       / marker
;;       / search
;;       / "^"
;;       / "$"
;;       / "."
;; offset <- signed-number+
;; marker <- "'" "[a-zA-Z]"
(defun evil-ex2-parse (string &optional syntax)
  (let* ((start-symbol (car-safe (car-safe evil-ex2-grammar)))
         (match (evil-parser
                 string start-symbol evil-ex2-grammar t syntax)))
    (when match
      (car match))))

;; this is rather non-functional, but efficient
(defun evil-flatten-tree (tree)
  (let* ((result nil)
         (traverse
          (lambda (tree path)
            (if (stringp tree)
                (dotimes (char (length tree))
                  (push path result))
              (let ((path (cons (car tree) path)))
                (dolist (subtree (cdr tree))
                  (funcall traverse subtree path)))))))
    (funcall traverse tree nil)
    (nreverse result)))

(defun evil-ex2-completed-binding (&rest args))
(defun evil-ex2-get-current-range (&rest args))

(defun evil-ex2-call-current-command ()
  "Execute the given command COMMAND."
  (if (not evil-ex2-current-cmd)
      (error "Invalid ex-command.")
    (let ((binding (evil-ex2-completed-binding evil-ex2-current-cmd)))
      (if binding
          (with-current-buffer evil-ex2-current-buffer
            (save-excursion
              (let ((range (evil-ex2-get-current-range))
                    prefix-arg)
                (when (and (not range)
                           evil-ex2-current-range
                           (car evil-ex2-current-range)
                           (numberp (caar evil-ex2-current-range)))
                  (setq prefix-arg (caar evil-ex2-current-range)))
                (call-interactively binding))))
        (error "Unknown command %s" evil-ex2-current-cmd)))))

(defun evil-ex2-call-command (count command bang arguments)
  "Execute the given command COMMAND."
  (let ((current-prefix-arg count)
        (evil-ex2-this-args arguments)
        (evil-ex2-this-bang bang))
    (if command
        (call-interactively command)
      (error "Invalid command"))))

(defun evil-ex2-call-command-on-range (range command bang arguments)
  "Execute the given command COMMAND."
  ;; TODO: handle type
  (setq arguments (append range arguments))
  (evil-ex2-call-command nil command arguments bang))

(defun evil-ex2-range (beg-line &optional end-line)
  "Returns the first and last position of the current range."
  (evil-range
   (save-excursion
     (evil-goto-line beg-line)
     (point))
   (save-excursion
     (evil-goto-line (or end-line beg-line))
     (point))
   'line))

(defun evil-ex2-full-range ()
  (evil-range (point-min) (point-max) 'line))

(defun evil-ex2-address (base &optional offset)
  (+ (or offset 0)
     (cond
      ((integerp base)
       base)
      ((null base)
       (line-number-at-pos))
      ((eq (car-safe base) 'abs)
       (cdr base))
      ;; TODO: (1- ...) may be wrong if the match is the empty string
      ((eq base 're-fwd)
       (save-excursion
         (beginning-of-line 2)
         (and (re-search-forward (cdr base))
              (line-number-at-pos (1- (match-end 0))))))
      ((eq base 're-bwd)
       (save-excursion
         (beginning-of-line 0)
         (and (re-search-backward (cdr base))
              (line-number-at-pos (match-beginning 0)))))
      ;; $, %, ., etc.
      ((eq base 'current-line)
       (line-number-at-pos (point)))
      ((eq base 'first-line)
       (line-number-at-pos (point-min)))
      ((eq base 'last-line)
       (line-number-at-pos (point-max)))
      ((eq (car-safe base) 'mark)
       (setq base (cadr base))
       (let* ((base (cadr base))
              (mark (evil-get-marker base)))
         (cond
          ((null mark)
           (error "Marker <%c> not defined" base))
          ((consp mark)
           (error "Ex-mode ranges do not support markers in other files"))
          (t
           (line-number-at-pos mark)))))
      ((eq base 'next-of-prev-search)
       (error "Next-of-prev-search not yet implemented"))
      ((eq base 'prev-of-prev-search)
       (error "Prev-of-prev-search not yet implemented"))
      ((eq base 'next-of-prev-subst)
       (error "Next-of-prev-subst not yet implemented"))
      (t
       (error "Invalid address: %s" base)))))

(defun evil-ex2-first-line ()
  "Return the line number of the first line."
  1)

(defun evil-ex2-current-line ()
  "Return the line number of the current line."
  (line-number-at-pos (point)))

(defun evil-ex2-last-line ()
  "Return the line number of the last line."
  (line-number-at-pos (point-max)))

(defun evil-ex2-signed-number (sign &optional number)
  "Return a signed number like -3 and +1.
NUMBER defaults to 1."
  (funcall sign (or number 1)))

(defun evil-ex2-marker (marker)
  "Return MARKER's line number in the current buffer.
Signal an error if MARKER is in a different buffer."
  (setq marker (evil-get-marker marker))
  (if (numberp marker)
      (line-number-at-pos marker)
    (error "Ex does not support markers in other files")))

(defun evil-ex2-re-fwd (pattern)
  (save-excursion
    (beginning-of-line 2)
    (and (re-search-forward pattern)
         (line-number-at-pos (1- (match-end 0))))))

(defun evil-ex2-re-bwd (pattern)
  (save-excursion
    (beginning-of-line 0)
    (and (re-search-backward pattern)
         (line-number-at-pos (match-beginning 0)))))

(defun evil-ex2-parse-arguments (string)
  (let ((pos 0) args expr start)
    (condition-case nil
        (while (not (eq start pos))
          (setq start pos
                expr (read-from-string string start))
          (push (car expr) args)
          (setq pos (cdr expr)))
      (error nil))
    (when args
      (setq args (cons 'list (nreverse args))))
    (cons args nil)))

(defun evil-ex2-prev-search ()
  (error "Previous search not yet implemented"))

(defun evil-parser (string symbol grammar &optional greedy syntax)
  "Parse STRING as a SYMBOL in GRAMMAR.
If GREEDY is non-nil, the whole of STRING must match.
The return value is a cons cell (RESULT . TAIL), where
RESULT is a syntax tree and TAIL is the remainder of STRING.

GRAMMAR is an association list of syntactic definitions.
Each entry in the list has the form (SYM DEF), where DEF is
either a #'-quoted function or a list of production rules.
If DEF is a function, then the function must accept a string
as input and return a parse result as output. Otherwise,
DEF is one or more of the following rules:

    nil matches the empty string.
    A regular expression matches a substring.
    A symbol matches a production for that symbol.
    (X Y) matches X followed by Y.
    (\\? X) matches zero or one of X.
    (* X) matches zero or more of X.
    (+ X) matches one or more of X.
    (& X) matches X, but does not consume.
    (! X) matches anything but X, but does not consume.

When multiple rules are specified, the rules are tried in the
order given. When a rule matches, any remaining rules are
ignored. Therefore, a grammar is always unambiguous, since
at most one parsing is found.

A rule may specify a semantic action with a #'-quoted function at
the end of the rule. If no function is given, then the default
action is to construct a list of values; if a function is given,
then its input are the values returned by other semantic actions.
The value of a regexp rule is the string matched, while the value
of a nil rule is also nil.

The following symbols have reserved meanings within a grammar:
`\\?', `*', `+', `&', `!', `function', `alt', `seq' and nil."
  (let ((string (or string ""))
        func pair result rules tail)
    (cond
     ;; epsilon
     ((member symbol '("" nil))
      (setq pair (cons nil string)))
     ;; token
     ((stringp symbol)
      (save-match-data
        (when (or (eq (string-match symbol string) 0)
                  ;; ignore leading whitespace
                  (and (string-match "^[ \f\t\n\r\v]+" string)
                       (eq (match-end 0)
                           (string-match
                            symbol string (match-end 0)))))
          (setq result (match-string 0 string)
                tail (substring string (match-end 0))
                pair (cons result tail))
          (when (and syntax pair)
            (setq result (substring string 0
                                    (- (length string)
                                       (length tail))))
            (setcar pair result)))))
     ;; term
     ((symbolp symbol)
      (setq rules (cdr-safe (assq symbol grammar)))
      (setq pair (evil-parser string `(alt ,@rules)
                              grammar greedy syntax))
      (when (and syntax pair)
        (setq result (car pair))
        (if (and (listp result) (sequencep (car result)))
            (setq result `(,symbol ,@result))
          (setq result `(,symbol ,result)))
        (setcar pair result)))
     ;; function
     ((eq (car-safe symbol) 'function)
      (setq symbol (cadr symbol)
            pair (funcall symbol string))
      (when (and syntax pair)
        (setq tail (or (cdr pair) "")
              result (substring string 0
                                (- (length string)
                                   (length tail))))
        (setcar pair result)))
     ;; list
     ((listp symbol)
      (setq rules symbol
            symbol (car-safe rules))
      (if (memq symbol '(& ! \? * + alt seq))
          (setq rules (cdr rules))
        (setq symbol 'seq))
      (when (and (memq symbol '(+ alt seq))
                 (> (length rules) 1))
        (setq func (car (last rules)))
        (if (eq (car-safe func) 'function)
            (setq rules (delq func (copy-sequence rules))
                  func (cadr func))
          (setq func nil)))
      (cond
       ;; positive lookahead
       ((eq symbol '&)
        (when (evil-parser string rules grammar greedy syntax)
          (setq pair (evil-parser string nil grammar nil syntax))))
       ;; negative lookahead
       ((eq symbol '!)
        (unless (evil-parser string rules grammar greedy syntax)
          (setq pair (evil-parser string nil grammar nil syntax))))
       ;; zero or one
       ((eq symbol '\?)
        (setq rules (if (> (length rules) 1)
                        `(alt ,rules nil)
                      `(alt ,@rules nil))
              pair (evil-parser string rules grammar greedy syntax)))
       ;; zero or more
       ((eq symbol '*)
        (setq rules `(alt (+ ,@rules) nil)
              pair (evil-parser string rules grammar greedy syntax)))
       ;; one or more
       ((eq symbol '+)
        (let (current results)
          (catch 'done
            (while (setq current (evil-parser
                                  string rules grammar nil syntax))
              (setq result (car-safe current)
                    tail (or (cdr-safe current) "")
                    results (append results (if syntax result
                                              (cdr-safe result))))
              ;; stop if stuck
              (if (equal string tail)
                  (throw 'done nil)
                (setq string tail))))
          (when results
            (setq func (or func 'list)
                  pair (cons results tail)))))
       ;; alternatives
       ((eq symbol 'alt)
        (catch 'done
          (dolist (rule rules)
            (when (setq pair (evil-parser
                              string rule grammar greedy syntax))
              (throw 'done pair)))))
       ;; sequence
       (t
        (setq func (or func 'list))
        (let ((last (car-safe (last rules)))
              current results rule)
          (catch 'done
            (while rules
              (setq rule (pop rules)
                    current (evil-parser string rule grammar
                                         (when greedy
                                           (null rules))
                                         syntax))
              (cond
               ((null current)
                (setq results nil)
                (throw 'done nil))
               (t
                (setq result (car-safe current)
                      tail (cdr-safe current))
                (unless (memq (car-safe rule) '(& !))
                  (if (and syntax
                           (or (null result)
                               (and (listp rule)
                                    (not (and (eq (car-safe rule) '\?)
                                              (eq (length rule) 2))))))
                      (setq results (append results result))
                    (setq results (append results (list result)))))
                (setq string (or tail ""))))))
          (when results
            (setq pair (cons results tail))))))
      ;; semantic action
      (when (and pair func (not syntax))
        (setq result (car pair))
        (let* ((dexp
                (lambda (obj)
                  (when (symbolp obj)
                    (let ((str (symbol-name obj)))
                      (when (string-match "\\$\\([0-9]+\\)" str)
                        (string-to-number (match-string 1 str)))))))
               ;; traverse a tree for dollar expressions
               (dval
                (lambda (obj)
                  (if (listp obj)
                      (mapcar dval obj)
                    (let ((num (funcall dexp obj)))
                      (if num
                          (if (not (listp result))
                              result
                            (if (eq num 0)
                                `(list ,@result)
                              (nth (1- num) result)))
                        obj))))))
          (cond
           ((null func)
            (setq result nil))
           ((eq (car-safe func) 'lambda)
            (if (memq symbol '(+ seq))
                (setq result `(funcall ,func ,@result))
              (setq result `(funcall ,func ,result))))
           ((funcall dexp func)
            (setq result (funcall dval func)))
           ((listp func)
            (setq result (funcall dval func)))
           (t
            (if (memq symbol '(+ seq))
                (setq result `(,func ,@result))
              (setq result `(,func ,result))))))
        (setcar pair result))))
    ;; weed out incomplete matches
    (when pair
      (if (not greedy) pair
        (if (null (cdr pair)) pair
          ;; ignore trailing whitespace
          (when (string-match "^[ \f\t\n\r\v]*$" (cdr pair))
            (unless syntax (setcdr pair nil))
            pair))))))

(string (aref "'a,'bcenter-line/hm/" 4))
;; => "b"
(nth 4 (evil-flatten-tree (evil-ex2-parse "'a,'bcenter-line/hm/" t)))
;; => (marker line address range expression)

'(evil-flatten-tree
  '(expression
    (range
     (address
      (line
       (marker "'" "a")))
     ","
     (address
      (line
       (marker " '" "b"))))
    (command "center-line")
    (arguments
     (string-arguments "/" "hm" "/"))))

'((marker line address range expression)
  (marker line address range expression)
  (range expression)
  (marker line address range expression)
  (marker line address range expression)
  (marker line address range expression)
  (command expression)
  (command expression)
  (command expression)
  (command expression)
  (command expression)
  (command expression)
  (command expression)
  (command expression)
  (command expression)
  (command expression)
  (command expression)
  (string-arguments arguments expression)
  (string-arguments arguments expression)
  (string-arguments arguments expression)
  (string-arguments arguments expression))

(provide 'evil-ex2)

;; evil-ex2.el ends here