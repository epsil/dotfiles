;;; die.el --- it is time to die -*- coding: utf-8 -*-

(require 'cl)
(require 'diskusjon)

(define-minor-mode die
  "It is time to die"
  nil nil (make-sparse-keymap "Die")

  (define-key-after global-map [menu-bar die]
    (cons "Die" (make-sparse-keymap "Die"))
    t)

  (define-key global-map [menu-bar die f12] '("Delete" . d-delete))
  (define-key-after global-map [menu-bar die restore] '("Restore" . d-restore) t)
  (define-key-after global-map [menu-bar die save] '("Save" . save-buffer) t))

(define-key die-map [f12] (cons "Restore" 'd-restore))
(define-key die-map [f11] 'd-delete-and-exit)
(define-key die-map [mouse-2] 'd-delete-and-exit)

(add-hook 'diskusjon-mode-hook 'die)

(defvar d-string "[i][/i]")

(defun d ()
  (interactive)
  (let ((date (read-from-minibuffer "Dato: "))
        (file buffer-file-name)
        backup)
    (setq date (replace-regexp-in-string "januar" "jan" date)
          date (replace-regexp-in-string "februar" "feb" date)
          date (replace-regexp-in-string "mars" "mar" date)
          date (replace-regexp-in-string "april" "apr" date)
          date (replace-regexp-in-string "mai" "may" date)
          date (replace-regexp-in-string "juni" "jun" date)
          date (replace-regexp-in-string "juli" "jul" date)
          date (replace-regexp-in-string "august" "aug" date)
          date (replace-regexp-in-string "september" "sep" date)
          date (replace-regexp-in-string "oktober" "oct" date)
          date (replace-regexp-in-string "november" "nov" date)
          date (replace-regexp-in-string "desember" "dec" date)
          date (replace-regexp-in-string
                "i dag" (regexp-quote
                         (format-time-string "%Y-%m-%d")) date)
          date (replace-regexp-in-string
                "i går" (regexp-quote
                         (format-time-string
                          "%Y-%m-%d"
                          (time-subtract (current-time)
                                         (days-to-time 1)))) date))
    (destructuring-bind
        (sec min hour day mon year dow dst tz)
        (parse-time-string date)
      (setq date (format "%04d-%02d-%02dT%02d:%02d"
                         year mon day hour min)
            backup (concat "~/Diskusjon.no/" date ".forum"))
      (write-file backup t)
      ;; (delete-region (point-min) (point-max))
      ;; (insert "[img]http://hekta.org/~vegardoy/diskusjon.jpg[/img]")
      ;; (write-file file)
      ;; (message "Wrote %s" backup)
      )))

(defun d-restore ()
  (interactive)
  (delete-region (point-min) (point-max))
  (save-excursion
    (insert d-string))
  (when (window-system)
    (set-frame-height (selected-frame) 10))
  (let ((date (read-from-minibuffer "Dato: "))
        (file buffer-file-name)
        backup)
    (dolist (entry '(("januar"    . "jan")
                     ("februar"   . "feb")
                     ("mars"      . "mar" )
                     ("april"     . "apr")
                     ("mai"       . "may" )
                     ("juni"      . "jun")
                     ("juli"      . "jul")
                     ("august"    . "aug")
                     ("september" . "sep")
                     ("oktober"   . "oct")
                     ("november"  . "nov")
                     ("desember"  . "dec")))
      (setq date (replace-regexp-in-string
                  (car entry) (cdr entry) date)))
    (setq date (replace-regexp-in-string
                "i dag" (regexp-quote
                         (format-time-string "%Y-%m-%d")) date)
          date (replace-regexp-in-string
                "i går" (regexp-quote
                         (format-time-string
                          "%Y-%m-%d"
                          (time-subtract (current-time)
                                         (days-to-time 1)))) date))
    (destructuring-bind
        (sec min hour day mon year dow dst tz)
        (parse-time-string date)
      (setq date (format "%04d-%02d-%02dT%02d:%02d"
                         year mon day hour min)
            backup (concat "~/backup/" date ".txt"))
      (save-window-excursion
        (find-file backup)
        (kill-ring-save (point-min) (point-max)))
      (delete-region (point-min) (point-max))
      (save-excursion (yank))
      (add-hook 'before-save-hook 'd-save-hook))))

(defun d-save-hook ()
  (let (name url)
    (remove-hook 'before-save-hook 'd-save-hook)
    (cond
     ;; ((eq this-command 'd-delete-and-exit)
     ;;  nil)
     ((y-or-n-p "Lagre? ")
      (setq name (diskusjon-trim (read-from-minibuffer "Navn: "))
            url  (diskusjon-trim (read-from-minibuffer "URL: ")))
      (save-window-excursion
        (find-file "~/backup.txt")
        (goto-char (point-max))
        (insert (format "\n%s %s" name url))
        (save-buffer 0)))
     (t
      (delete-region (point-min) (point-max))
      (save-excursion
        (insert d-string))))))

(defun d-delete ()
  (interactive)
  (when (window-system)
    (set-frame-height (selected-frame) 10))
  (delete-region (point-min) (point-max))
  (save-excursion
    (insert d-string))
  (save-buffer 0))

(defun d-delete-and-exit ()
  (interactive)
  (d-delete)
  (kill-emacs))

;; (global-set-key (kbd "<f5>") 'd)
(global-set-key [f12] 'd-restore)

;; (when (window-system)
;;   (set-frame-height (selected-frame) 10))

(defvar d-format-counter nil)

(defun d-format (beg end)
  (interactive "r")
  (let* ((beg (move-marker (make-marker) (or beg (point))))
         (end (move-marker (make-marker) (or end (point))))
         (num (or (string-to-number (read-from-minibuffer
                                    "Start at: " (when d-format-counter
                                                   (number-to-string d-format-counter)))) 1))
        (title (buffer-substring
                 (progn
                   (when (region-active-p) (goto-char beg))
                   (beginning-of-line)
                   (point))
                 (progn
                   (when (re-search-forward "http" nil t)
                     (goto-char (1- (match-beginning 0))))
                   (point)))))
    (beginning-of-line)
    (insert (format "[b]%s[/b]\n\n\n" title))
    (message title)
    (catch 'break
      (while (< (point) end)
        (delete-char -1)
        (delete-region
         (progn
           (point))
         (progn
           (if (re-search-forward "http" end t)
             (goto-char (match-beginning 0))
             (throw 'break nil))
           (point)))
        (insert "[url=")
        (end-of-line)
        (insert (format "]%s[/url], " num))
        (setq num (1+ num))
        (forward-line)
        (beginning-of-line)))
    (delete-char -2)
    (insert ".\n")
    (setq d-format-counter num)
    (set-marker beg nil)
    (set-marker end nil)))

(provide 'die)

;;; die.el ends here
