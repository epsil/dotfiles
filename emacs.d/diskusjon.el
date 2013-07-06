;;; diskusjon.el --- major mode for foruminnlegg -*- coding: utf-8 -*-

;; Author: Vegard Øye <vegard underline oye at hotmail dot com>
;; Created: 10 Oct 2010
;; Keywords: bbcode, forum, diskusjon.no
;; Compatibility: GNU Emacs 22, 23
;;
;; This file is not part of GNU Emacs.

;;; Commentary:

;; Plasser denne filen et sted i `load-path'. Åpne deretter .emacs og
;; legg til følgende linje (uten kommentarprefiks):
;;
;; (require 'diskusjon)
;;
;; Filer med endelsen .forum vil automatisk åpnes som foruminnlegg.
;; Dermed kan Emacs brukes sammen med Firefox-utvidelsen
;; «It's All Text!», som kan hentes fra:
;;
;; https://addons.mozilla.org/en-US/firefox/addon/4125/
;;
;; For å konfigurere Firefox, åpne «It's All Text! Preferences».
;; Sett «Editor» til /usr/bin/emacs og legg til endelsen .forum
;; først i «File Extensions». Nå kan Emacs startes ved å bevege
;; musen over et tekstfelt og klikke på den blå «edit»-knappen
;; nederst til høyre.

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or any later version.

(require 'iso-transl)
(require 'skeleton)

;; Filtype for foruminnlegg
(add-to-list 'auto-mode-alist '("\\.forum$" . diskusjon-mode))
(modify-coding-system-alist 'file "\\.forum$" 'utf-8)

(define-derived-mode diskusjon-mode text-mode "Diskusjon"
  "Major mode for å skrive foruminnlegg.

Dette er en major mode for å skrive foruminnlegg med BB-kode.
Tagger som [i] ... [/i] kan settes inn med enkle tastetrykk, og
tegnsetting som « og » håndteres automatisk ut fra konteksten.
Et overblikk over kommandoer:\\<diskusjon-mode-map>

    C-c e      `diskusjon-italic'        (kursiv tekst)
    C-c b      `diskusjon-bold'          (fet tekst)
    C-c C-u    `diskusjon-underline'     (understreket tekst)
    C-c C-s    `diskusjon-strikethrough' (gjennomstreket tekst)
    C-c t      `diskusjon-tt'            (fastbreddeskrift)
    C-c +      `diskusjon-sup'           (hevet tekst)
    C-c -      `diskusjon-sub'           (senket tekst)
    C-c u      `diskusjon-url'           (hyperlenke)
    C-c l      `diskusjon-list'          (liste)
    C-c j      `diskusjon-point'         (listepunkt)
    C-c i      `diskusjon-img'           (bilde)
    C-c C-i    `diskusjon-indent'        (blokkinnrykk)
    C-c s      `diskusjon-spoiler'       (skjult tekst)
    C-c y      `diskusjon-youtube'       (YouTube-film)
    C-c n      `diskusjon-note'          (kildehenvisning)
    C-c q      `diskusjon-quotation'     (sitat)

Dette er avledet fra AUCTeX, og gamle AUCTeX-brukere med
\«C-c C-f»-sekvenser i fingrene kan bruke disse i stedet.
Som i AUCTeX fungerer kommandoene også på markert tekst.

\\[diskusjon-double-quote]-tasten implementerer «smart quotes», \
dvs. setter inn « og »
basert på konteksten. (Den nøyaktige oppførselen avhenger av
variable `diskusjon-lang' og `diskusjon-quotes'.) I tillegg blir
enkelte sekvenser automatisk erstattet à la AutoKorrektur i Word:
for eksempel vil -- byttes ut med ekte tankestrek. (Minner om
Emacs' egen abbrev-mode, men er kraftigere da det bygger på
regulære uttrykk: se variabelen `diskusjon-regexps'.)
Erstattingen kan overstyres med «C-q»: «C-q - C-q -» vil sette
inn to bindestreker uten å erstatte dem.

Kommandoen `M-x ispell-buffer' kjører stavekontroll med språket i
variable `diskusjon-lang', standardverdi «norsk». Språkvalget kan
endres interaktivt med `M-x diskusjon-lang'."
  (add-hook 'post-command-hook 'diskusjon-post-hook nil t)
  (make-variable-buffer-local 'diskusjon-lang)
  (setq skeleton-end-newline nil)
  (ispell-change-dictionary diskusjon-lang)
  (if (version< emacs-version "23")
      (longlines-mode 1)
    (visual-line-mode 1))
  (when diskusjon-ascii
    (setq diskusjon-quotes diskusjon-ascii-quotes
          diskusjon-regexps diskusjon-ascii-regexps))
  (setq font-lock-multiline t)
  (setq font-lock-defaults
        `((("\\(\\[[^]]*\\]\\)"
            (1 diskusjon-tag-face append))
           (,(format
              "\\([%s]\\(?:.\\|\n\\)*?[%s]\\)"
              (apply 'concat
                     (mapcar (lambda (e)
                               (regexp-quote (string (nth 1 e))))
                             diskusjon-quotes))
              (apply 'concat
                     (mapcar (lambda (e)
                               (regexp-quote (string (nth 2 e))))
                             diskusjon-quotes)))
            (1 diskusjon-string-face append))
           ("\\[url=\"?[^]\"]*\"?\\]\
\\(\\(?:.\\|\n\\)*?\\)\\[/url\\]"
            (1 diskusjon-link-face append))
           ("\\[i\\]\\(\\(?:.\\|\n\\)*?\\)\\[/i\\]"
            (1 diskusjon-italic-face append))
           ("\\[b\\]\\(\\(?:.\\|\n\\)*?\\)\\[/b\\]"
            (1 diskusjon-bold-face append))
           ("\\[u\\]\\(\\(?:.\\|\n\\)*?\\)\\[/u\\]"
            (1 diskusjon-underline-face append))
           ("\\(\\[sup\\]\\(?:.\\|\n\\)*?\\[/sup\\]\\)"
            (1 diskusjon-sup-face append))
           ("\\(\\[sub\\]\\(?:.\\|\n\\)*?\\[/sub\\]\\)"
            (1 diskusjon-sub-face append))))))

(defface diskusjon-tag-face '((t (:inherit minibuffer-prompt)))
  "BB-kode.")
(defvar diskusjon-tag-face 'diskusjon-tag-face)

(defface diskusjon-string-face '((t (:inherit font-lock-string-face)))
  "Sitater.

Hvis du bruker en tidligere Emacs-versjon enn 23.2, kan denne
være litt lys av farge. Dette ordnes med følgende linje:

    (set-face-foreground 'font-lock-string-face \"VioletRed4\")\n")
(defvar diskusjon-string-face '(face diskusjon-string-face font-lock-multiline t))

(defface diskusjon-italic-face '((t (:inherit italic)))
  "Kursiv skrift.")
(defvar diskusjon-italic-face '(face diskusjon-italic-face font-lock-multiline t))

(defface diskusjon-bold-face '((t (:inherit bold)))
  "Fet skrift.")
(defvar diskusjon-bold-face '(face diskusjon-bold-face font-lock-multiline t))

(defface diskusjon-underline-face '((t (:inherit underline)))
  "Understreket skrift.")
(defvar diskusjon-underline-face '(face diskusjon-underline-face font-lock-multiline t))

(defface diskusjon-link-face '((t (:inherit link)))
  "Lenketekst.")
(defvar diskusjon-link-face '(face diskusjon-link-face font-lock-multiline t))

(defface diskusjon-sup-face '((t (:height 0.8)))
  "Hevet skrift.")
(defvar diskusjon-sup-face
  '(face diskusjon-sup-face font-lock-multiline t display (raise 0.3)))

(defface diskusjon-sub-face '((t (:height 0.8)))
  "Senket skrift.")
(defvar diskusjon-sub-face
  '(face diskusjon-sub-face font-lock-multiline t display (raise -0.3)))

(defvar diskusjon-lang "norsk"
  "*Språkvalg for anførselstegn.
Verdien, som er en streng, må være listet i `diskusjon-quotes'
og bør kunne gjenkjennes av ispell.")

(defvar diskusjon-ascii nil
  "*Hvorvidt tegnene skal begrenses til ASCII.")

(defun diskusjon-lang (lang)
  "Endre språk.
Det gjeldende språkvalget er lagret i variable `diskusjon-lang'."
  (interactive
   (list (completing-read
          (if (equal diskusjon-lang "norsk")
              "Språk: " "Language: ")
          (and (fboundp 'ispell-valid-dictionary-list)
               (mapcar 'list (ispell-valid-dictionary-list)))
          nil nil diskusjon-lang)))
  (ispell-change-dictionary lang)
  (setq diskusjon-lang lang))

(defvar diskusjon-quotes
  '((english ?“ ?” ?‘ ?’)
    ;; http://www.typografi.org/sitat/sitatart.html
    (norsk ?« ?» ?‘ ?’                  ; ?“ ?”
           ))
  "Doble og enkle anførselstegn for ulike språk.
En oppføring har formen

    (SPRÅK DOBBEL-VENSTRE DOBBEL-HØYRE
           ENKEL-VENSTRE ENKEL-HØYRE ...).

Det er rom for et vilkårlig antall nivåer. Det første nivået blir
brukt for fargekoding med face `diskusjon-string-face'.")

(defvar diskusjon-ascii-quotes
  '((english ?\" ?\" ?' ?')
    (norsk ?« ?» ?' ?')))

(defvar diskusjon-regexps
  '(("--" . "–")
    ("–-" . "—")
    ("—-" . "−")
    ("\\.\\.\\." . "…")
    ("\\(?:[[:blank:] ]\\|^\\)\\(\\^\\.\\)" . "·")
    ("\\(?:[[:blank:] ]\\|^\\)\\(\\^x\\)" . "×")
    ;; ("  " . " ")
    ("~~" . " ")
    ("\\\\," . " ")
    ("\\( \\)[–…%×·@]" . " ")
    ("\\( –[[:blank:]]\\)[^.?!]+ –" . " – ")
    ("[×·@]\\( \\)" . " ")
    ("\\(?:[^ [:blank:]]\\|^\\)[–…]\\(\\ \\)" . " ")
    ("[0-9]+\\( \\)[0-9]+" . " ")
    ("\\( \\)[–…] " . " ")
    ("\\^-" . "−")
    ("\\+/-" . "±")
    ("-/\\+" . "∓")
    ("->" . "→")
    ("<-" . "←")
    ("←>" . "↔")
    ("=>" . "⇒")
    ("<=" . "⇐")
    ("⇐>" . "⇔")
    ("\\^'" . "′")
    ("\\^’" . "′")
    ("′'" . "″")
    ("′’" . "″")
    ("″'" . "‴")
    ("″’" . "‴"))
  "Regulære uttrykk som skal erstattes mens man skriver.
Uttrykkene erstattes i den rekkefølgen de er listet.

En oppføring har formen (UTTRYKK . ERSTATNING), der utrykket blir
erstattet i sin helhet hvis det ikke inneholder subexpressions.
Dersom det inneholder subexpressions, altså \\\\( ... \\\\), blir
subexpression nr. 1 erstattet. (Bruk \\\\(?: ... \\\\) for å gruppere
ting uten å lage en subexpression.)

Oppføringen kan også ha formen (UTTRYKK ERSTATNING PREDIKAT), der
predikatet er en funksjon eller uttrykk som avgjør hvorvidt
erstattingen skal finne sted; returverdi nil hvis ikke. Hvis
returverdien er `final', blir uttrykket erstattet, men de
resterende oppføringene i listen blir ignorert.")

(defvar diskusjon-ascii-regexps
  '(("\\(?:[[:blank:] ]\\|^\\)\\(\\^\\.\\)" . "·")
    ("\\(?:[[:blank:] ]\\|^\\)\\(\\^x\\)" . "×")
    ("  " . " ")
    ("\\+/-" . "±")))

;; «C-x 8»-bindinger for hevede og senkede tegn
;; (nyttig på fora uten [sup]/[sub])
(iso-transl-define-keys
 '(("^0" . [?⁰])
   ("^4" . [?⁴]) ; 1-3 allerede definert i iso-transl.el
   ("^5" . [?⁵])
   ("^6" . [?⁶])
   ("^7" . [?⁷])
   ("^8" . [?⁸])
   ("^9" . [?⁹])
   ("^+" . [?⁺])
   ("^-" . [?⁻])
   ("^=" . [?⁼])
   ("^(" . [?⁽])
   ("^)" . [?⁾])
   ("_0" . [?₀])
   ("_1" . [?₁])
   ("_2" . [?₂])
   ("_3" . [?₃])
   ("_4" . [?₄])
   ("_5" . [?₅])
   ("_6" . [?₆])
   ("_7" . [?₇])
   ("_8" . [?₈])
   ("_9" . [?₉])
   ("_+" . [?₊])
   ("_-" . [?₋])
   ("_=" . [?₌])
   ("_(" . [?₍])
   ("_)" . [?₎])))

;; GNU Emacs 22 mangler `region-active-p'
(unless (fboundp 'region-active-p)
  (defsubst region-active-p ()
    (and transient-mark-mode mark-active)))

(defun diskusjon-post-hook ()
  "Erstatt uttrykk i `diskusjon-regexps'."
  (when (and (null current-prefix-arg)
             (not (region-active-p))
             (memq this-command
                   '(diskusjon-double-quote
                     diskusjon-single-quote
                     self-insert-command)))
    (condition-case nil
        (catch 'final
          (dolist (entry diskusjon-regexps)
            (let ((regexp (car entry)) newtext predicate retval)
              (when (looking-back regexp)
                (setq newtext (cdr entry))
                (when (listp newtext)
                  (setq predicate (cadr newtext)
                        retval (cond
                                ((eq predicate 'final)
                                 'final)
                                ((functionp predicate)
                                 (funcall predicate))
                                (t
                                 (eval predicate)))
                        newtext (car newtext)))
                (when (or (null predicate) retval)
                  (save-excursion
                    (replace-match newtext nil nil nil
                                   (when (match-string 1) 1)))
                  (when (eq retval 'final)
                    (throw 'final nil)))))))
      (error nil))))

(defun diskusjon-end-of-word-p ()
  (and (not (looking-at "\\<"))
       (not (looking-back "^\\|\\s-\\|`"))
       (not (bobp))
       (not (memq (char-before) '(?\/)))
       (not (memq (char-syntax (char-before))
                  '(?\( ?\  ?\-)))))

(defun diskusjon-trim (string)
  (save-match-data
    (string-match "\\`[[:space:]]*\
\\(\\(?:[[:space:]]*[^[:space:]]+\\)*\\)[[:space:]]*\\'" string)
    (or (match-string 1 string) string)))

(defun diskusjon-double-quote (&optional single)
  "Sett inn doble anførselstegn.
Med prefiksargument, \\[universal-argument], sett inn enkle anførselstegn.
Dersom tekst er markert, omsluttes teksten av anførselstegn,
og doble anførselstegn erstattes med enkle.

Definisjonene av doble og enkle anførselstegn avhenger av
variable `diskusjon-lang' og `diskusjon-quotes'."
  (interactive "P")
  (let* ((quotes (cdr (assq (intern diskusjon-lang) diskusjon-quotes)))
         (left   (nth 0 quotes))
         (right  (nth 1 quotes))
         (sleft  (nth 2 quotes))
         (sright (nth 3 quotes)))
    (when single
      (setq left   (or sleft left)
            right  (or sright right)
            quotes (memq left quotes)))
    (cond
     ((region-active-p)
      (let ((beg (region-beginning))
            (end (region-end))
            alist char)
        (while (nth 2 quotes)
          (add-to-list 'alist (cons (nth 0 quotes) (nth 2 quotes)))
          (pop quotes))
        (save-excursion
          (goto-char beg)
          (while (< (point) end)
            (cond
             ((setq char (assq (char-after) alist))
              (delete-char 1)
              (if (= (point) beg)
                  (insert (string (cdr char)))
                (insert-before-markers (string (cdr char)))))
             (t
              (forward-char 1)))))
        (skeleton-insert `(nil left _ right) -1)))
     ((diskusjon-end-of-word-p)
      (insert right))
     (t
      (insert left)))))

(defun diskusjon-single-quote (arg)
  "Sett inn enkle anførselstegn.
Dersom tekst er markert, omsluttes teksten av anførselstegn."
  (interactive "P")
  (let ((open (if diskusjon-ascii ?' ?‘))
        (close (if diskusjon-ascii ?' ?’)))
    (cond
     ((region-active-p)
      (skeleton-insert `(nil ,open _ ,close) -1))
     ((and (null arg)
           (diskusjon-end-of-word-p))
      (insert close))
     (t
      (dotimes (arg (prefix-numeric-value arg))
        (insert open))))))

(define-skeleton diskusjon-italic
  "Kursiv skrift."
  nil "[i]" _ "[/i]")

(define-skeleton diskusjon-bold
  "Fet skrift."
  nil "[b]" _ "[/b]")

(define-skeleton diskusjon-underline
  "Understreket skrift."
  nil "[u]" _ "[/u]")

(define-skeleton diskusjon-strikethrough
  "Gjennomstreket skrift."
  nil "[s]" _ "[/s]")

;; (define-skeleton diskusjon-tt
;;   "Fastbreddeskrift."
;;   nil "[font=\"Courier New\"]" _ "[/font]")

(defun diskusjon-tt (beg end)
  "Fastbreddeskrift."
  (interactive (if (region-active-p)
                   (list (region-beginning) (region-end))
                 (list (point) (point))))
  (save-excursion
    (replace-regexp " " " " nil beg end)
    (replace-regexp "…" "..." nil beg end)
    (replace-regexp "‘" "'" nil beg end)
    (replace-regexp "’" "'" nil beg end))
  (skeleton-insert '(nil "[font=\"Courier New\"]" _ "[/font]") -1)
  (when (region-active-p)
    (deactivate-mark)))

(define-skeleton diskusjon-sup
  "Hevet skrift."
  nil "[sup]" _ "[/sup]")

(define-skeleton diskusjon-sub
  "Senket skrift."
  nil "[sub]" _ "[/sub]")

(define-skeleton diskusjon-url
  "Hyperlenke."
  nil
  (let ((v1 (diskusjon-trim (skeleton-read "URL: "))))
    (format "[url=\"%s\"]" v1)) _ "[/url]")

(define-skeleton diskusjon-list
  "Punktliste."
  nil "[list"
  (diskusjon-trim (skeleton-read
                   (if (equal diskusjon-lang "norsk")
                       "Liste: " "List: ") "=1"))
  "][*] " _ (newline) "[/list]")

(define-skeleton diskusjon-point
  "Listepunkt."
  nil (unless (bolp) (newline)) "[*] ")

(define-skeleton diskusjon-img
  "Bilde."
  nil "[img]" _ "[/img]")

(define-skeleton diskusjon-indent
  "Blokkinnrykk."
  nil "[indent]" _ "[/indent]")

(define-skeleton diskusjon-spoiler
  "Skjult tekst."
  nil "[spoiler]" _ "[/spoiler]")

(define-skeleton diskusjon-youtube
  "YouTube-film."
  nil "[media]" _ "[/media]")

(defvar diskusjon-tag-history nil)
(define-skeleton diskusjon-tag
  "BB-tagg."
  nil
  (let ((tag (diskusjon-trim (read-from-minibuffer
                              (if (equal diskusjon-lang "norsk")
                                  "Tagg: " "Tag: ")
                              (car diskusjon-tag-history)
                              nil nil 'diskusjon-tag-history))))
    (list nil "[" tag "]" '_ "[/" tag "]")))

(define-skeleton diskusjon-note
  "Kildehenvisning."
  nil _
  (let* ((regexp "\\[url=\"?[^]\"]*\"?\\]\\[\\([0-9]+\\)[^]]*\\]\\[/url\\]")
         (v0 (save-excursion
               ;; Finn tidligere henvisning og øk nummeret med 1
               (if (re-search-backward regexp nil t)
                   (1+ (string-to-number (match-string 1))) 1)))
         (v1 (diskusjon-trim (or (skeleton-read
                                  (if (equal diskusjon-lang "norsk")
                                      "Nr.: " "No.: ")
                                  (number-to-string v0))
                                 (number-to-string v0))))
         (v2 (diskusjon-trim (skeleton-read "URL: "))))
    ;; Finn senere henvisninger og oppdater numrene
    (save-excursion
      (while (re-search-forward regexp nil t)
        (setq v0 (1+ v0))
        (replace-match (number-to-string v0) nil nil nil 1)))
    (cond
     ((looking-back (concat regexp "\\(\\[/sup\\]\\)"))
      (replace-match "" nil nil nil 2)
      (concat ",[url=\"" v2 "\"][" v1 "][/url][/sup]"))
     (t
      (concat "[sup][url=\"" v2 "\"][" v1 "][/url][/sup]")))) -)

(define-skeleton diskusjon-quotation
  "Sitat."
  nil
  (let ((level 0) (opoint (point)) name names)
    ;; Finn tidligere navn og deres [quote]-tagger. Hvis et navn
    ;; gjentas, velg den taggen som er nærmest utgangspunktet. Hvis
    ;; navnet gjentas i nøstede tagger, velg den ytterste taggen.
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "\\[/?quote\\(?: name=[\"']\\([^\"']+\\)[\"']\
\\(?:[^]\"]*?\\|\"[^\"]*\"\\)*?\\)?\\]" nil t)
        (cond
         ((match-string 1)
          (setq level (1+ level)
                name (match-string 1))
          (when (or (not (assoc name names))
                    (>= (nth 2 (assoc name names)) level))
            (setq names (assq-delete-all (car (assoc name names)) names))
            (add-to-list 'names (list name (match-string 0) level)
                         (> (point) opoint))))
         ((and (> level 0)
               (string-match (match-string 0) "^\\[/"))
          (setq level (1- level)))
         (t
          (setq level (1+ level))))))
    ;; Finn det nærmeste navnet med det laveste nivået
    ;; for å bruke det som forslag
    (setq level -1)
    (dolist (n names)
      (when (or (< level 0) (> level (nth 2 n)))
        (setq name  (nth 0 n)
              level (nth 2 n))))
    ;; Les inn et navn
    (setq name (completing-read (if (equal diskusjon-lang "norsk")
                                    "Siter: " "Quote: ")
                                (mapcar 'car names) nil nil name))
    (cond
     ((string= name "")
      "[quote]")
     ;; Tidligere navn: resirkuler [quote]-taggen
     ((assoc name names)
      (nth 1 (assoc name names)))
     (t
      (format "[quote name='%s']" name))))
  (newline) _ (newline) "[/quote]" (newline))

(define-key diskusjon-mode-map "'"            'diskusjon-single-quote)
(define-key diskusjon-mode-map "\""           'diskusjon-double-quote)
(define-key diskusjon-mode-map "\C-ce"        'diskusjon-italic)
(define-key diskusjon-mode-map "\C-c\C-e"     'diskusjon-italic)
(define-key diskusjon-mode-map "\C-c\C-fe"    'diskusjon-italic)
(define-key diskusjon-mode-map "\C-c\C-f\C-e" 'diskusjon-italic)
(define-key diskusjon-mode-map "\C-cb"        'diskusjon-bold)
(define-key diskusjon-mode-map "\C-c\C-b"     'diskusjon-bold)
(define-key diskusjon-mode-map "\C-c\C-fb"    'diskusjon-bold)
(define-key diskusjon-mode-map "\C-c\C-f\C-b" 'diskusjon-bold)
(define-key diskusjon-mode-map "\C-c\C-u"     'diskusjon-underline)
(define-key diskusjon-mode-map "\C-c\C-f\C-u" 'diskusjon-underline)
(define-key diskusjon-mode-map "\C-c\C-s"     'diskusjon-strikethrough)
(define-key diskusjon-mode-map "\C-c\C-f\C-s" 'diskusjon-strikethrough)
(define-key diskusjon-mode-map "\C-ct"        'diskusjon-tt)
(define-key diskusjon-mode-map "\C-c\C-ft"    'diskusjon-tt)
(define-key diskusjon-mode-map "\C-c^"        'diskusjon-sup)
(define-key diskusjon-mode-map "\C-c+"        'diskusjon-sup)
(define-key diskusjon-mode-map "\C-c\C-f^"    'diskusjon-sup)
(define-key diskusjon-mode-map "\C-c\C-f+"    'diskusjon-sup)
(define-key diskusjon-mode-map "\C-c-"        'diskusjon-sub)
(define-key diskusjon-mode-map "\C-c_"        'diskusjon-sub)
(define-key diskusjon-mode-map "\C-c\C-f-"    'diskusjon-sub)
(define-key diskusjon-mode-map "\C-c\C-f_"    'diskusjon-sub)
(define-key diskusjon-mode-map "\C-cu"        'diskusjon-url)
(define-key diskusjon-mode-map "\C-c\C-fu"    'diskusjon-url)
(define-key diskusjon-mode-map "\C-cl"        'diskusjon-list)
(define-key diskusjon-mode-map "\C-c\C-l"     'diskusjon-list)
(define-key diskusjon-mode-map "\C-c\C-fl"    'diskusjon-list)
(define-key diskusjon-mode-map "\C-c\C-f\C-l" 'diskusjon-list)
(define-key diskusjon-mode-map "\C-cj"        'diskusjon-point)
(define-key diskusjon-mode-map "\C-c\C-j"     'diskusjon-point)
(define-key diskusjon-mode-map "\C-ci"        'diskusjon-img)
(define-key diskusjon-mode-map "\C-c\C-fi"    'diskusjon-img)
(define-key diskusjon-mode-map "\C-c\C-i"     'diskusjon-indent)
(define-key diskusjon-mode-map "\C-c\C-f\C-i" 'diskusjon-indent)
(define-key diskusjon-mode-map "\C-cs"        'diskusjon-spoiler)
(define-key diskusjon-mode-map "\C-c\C-fs"    'diskusjon-spoiler)
(define-key diskusjon-mode-map "\C-cm"        'diskusjon-youtube)
(define-key diskusjon-mode-map "\C-c\C-m"     'diskusjon-youtube)
(define-key diskusjon-mode-map "\C-c\C-fm"    'diskusjon-youtube)
(define-key diskusjon-mode-map "\C-c\C-f\C-m" 'diskusjon-youtube)
(define-key diskusjon-mode-map "\C-cy"        'diskusjon-youtube)
(define-key diskusjon-mode-map "\C-c\C-y"     'diskusjon-youtube)
(define-key diskusjon-mode-map "\C-c\C-fy"    'diskusjon-youtube)
(define-key diskusjon-mode-map "\C-c\C-f\C-y" 'diskusjon-youtube)
(define-key diskusjon-mode-map "\C-c\C-t"     'diskusjon-tag)
(define-key diskusjon-mode-map "\C-c\C-f\C-t" 'diskusjon-tag)
(define-key diskusjon-mode-map "\C-c\C-n"     'diskusjon-note)
(define-key diskusjon-mode-map "\C-c\C-f\C-n" 'diskusjon-note)
(define-key diskusjon-mode-map "\C-cn"        'diskusjon-note)
(define-key diskusjon-mode-map "\C-c\C-fn"    'diskusjon-note)
(define-key diskusjon-mode-map "\C-cq"        'diskusjon-quotation)
(define-key diskusjon-mode-map "\C-c\C-q"     'diskusjon-quotation)
(define-key diskusjon-mode-map "\C-c\C-fq"    'diskusjon-quotation)
(define-key diskusjon-mode-map "\C-c\C-f\C-q" 'diskusjon-quotation)

(provide 'diskusjon)

;;; diskusjon.el ends here
