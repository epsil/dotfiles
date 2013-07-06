;; ~/.Xresources: Emacs.font: DejaVu Sans Mono-9
;; !!! Remember to xrdb -merge ~/.Xresources !!!
;; (set-frame-font "DejaVu Sans Mono-9")
;; (set-frame-font "Ubuntu Mono-10.5") ; a little variation
;; (set-frame-font "Consolas-10")
(setq user-full-name nil)
(set-scroll-bar-mode 'right)
(setq auto-window-vscroll nil)
(setq inhibit-startup-screen t)
(setq show-paren-delay 0)
(recentf-mode 1)
(show-paren-mode 1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(column-number-mode 1)
(global-hl-line-mode 1)
(set-face-background 'hl-line "#fffacd")

(setq-default indicate-empty-lines t)
(setq-default indent-tabs-mode nil)
(setq-default show-trailing-whitespace t)
(setq sentence-end-double-space nil)
(setq eval-expression-print-length nil)
(setq eval-expression-print-level nil)
(setq print-quoted t)
(setq sort-fold-case t)
;; (transient-mark-mode -1)
;; (cua-mode 1)
(cua-selection-mode t)
(setq x-select-enable-clipboard t
      x-select-enable-primary t)
(ido-mode 1)
(setq ido-enable-flex-matching t)
(defalias 'yes-or-no-p 'y-or-n-p)
(global-set-key (kbd "C-x TAB") nil)

;; (setq emacs-lisp-mode-map (make-keymap))
;; (setq resize-mini-windows nil)
(setq sh-basic-offset 8)

(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(defvar user-emacs-directory "~/.emacs.d")
(with-temp-buffer
  (cd user-emacs-directory)
  (add-to-list 'load-path default-directory)
  (normal-top-level-add-subdirs-to-load-path))

;; http://groups.google.com/group/gnu.emacs.help/browse_thread/thread/97bca63d022d3f5f
(setq redisplay-dont-pause t
      ;; scroll-margin 1
      scroll-step 1
      ;; scroll-conservatively 10000
      ;; scroll-preserve-screen-position 1
      )

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(global-set-key (kbd "C-M-2") "@")
(global-set-key (kbd "C-M-3") "£")
(global-set-key (kbd "C-M-4") "$")
(global-set-key (kbd "C-M-7") "{")
(global-set-key (kbd "C-M-8") "[")
(global-set-key (kbd "C-M-9") "]")
(global-set-key (kbd "C-M-0") "}")
(global-set-key (kbd "C-S-U") 'ucs-insert)

(global-set-key (kbd "M-<down>") 'windmove-down)
(global-set-key (kbd "M-<up>") 'windmove-up)
(global-set-key (kbd "M-<left>") 'windmove-left)
(global-set-key (kbd "M-<right>") 'windmove-right)

(global-set-key "(" 'skeleton-pair-insert-maybe)

(require 'parenface)
(set-face-foreground 'paren-face "blue4")
(set-face-foreground 'font-lock-string-face "VioletRed4")
(eval-after-load 'paren
  '(set-face-bold-p 'show-paren-match t))

(defun cleaner (&optional cleaner)
  "\"Who am I? I'll tell you who I am. I'm the CLEANER.\""
  (interactive)
  (save-excursion
    (unless (region-active-p)
      (mark-whole-buffer))
    (untabify (region-beginning) (region-end))
    (unless (eq 'text-mode major-mode)
      (indent-region (region-beginning) (region-end)))
    (save-restriction
      (narrow-to-region (region-beginning) (region-end))
      (delete-trailing-whitespace))
    (if (buffer-modified-p)
        (message "\"Whatever am I going to do with you?\"")
      (message "\"If we don't clean it, it's not dirty!\""))))

(global-set-key [f12] 'cleaner)

(defun sort-words (reverse beg end)
  "Sort words in region alphabetically, in REVERSE if negative.
Prefixed with negative \\[universal-argument], sorts in reverse.

The variable `sort-fold-case' determines whether alphabetic case
affects the sort order.

See `sort-regexp-fields'."
  (interactive "P\nr")
  (sort-regexp-fields reverse "\\w+" "\\&" beg end))

(defun sort-symbols (reverse beg end)
  "Sort symbols in region alphabetically, in REVERSE if negative.
See `sort-words'."
  (interactive "P\nr")
  (sort-regexp-fields reverse "\\(\\sw\\|\\s_\\)+" "\\&" beg end))

(defun ido-goto-symbol (&optional symbol-list)
  "Refresh imenu and jump to a place in the buffer using Ido."
  (interactive)
  (unless (featurep 'imenu)
    (require 'imenu nil t))
  (cond
   ((not symbol-list)
    (let ((ido-mode ido-mode)
          (ido-enable-flex-matching
           (if (boundp 'ido-enable-flex-matching)
               ido-enable-flex-matching t))
          name-and-pos symbol-names position)
      (unless ido-mode
        (ido-mode 1)
        (setq ido-enable-flex-matching t))
      (while (progn
               (imenu--cleanup)
               (setq imenu--index-alist nil)
               (ido-goto-symbol (imenu--make-index-alist))
               (setq selected-symbol
                     (ido-completing-read "Symbol? " symbol-names))
               (string= (car imenu--rescan-item) selected-symbol)))
      (unless (and (boundp 'mark-active) mark-active)
        (push-mark nil t nil))
      (setq position (cdr (assoc selected-symbol name-and-pos)))
      (cond
       ((overlayp position)
        (goto-char (overlay-start position)))
       (t
        (goto-char position)))))
   ((listp symbol-list)
    (dolist (symbol symbol-list)
      (let (name position)
        (cond
         ((and (listp symbol) (imenu--subalist-p symbol))
          (ido-goto-symbol symbol))
         ((listp symbol)
          (setq name (car symbol))
          (setq position (cdr symbol)))
         ((stringp symbol)
          (setq name symbol)
          (setq position
                (get-text-property 1 'org-imenu-marker symbol))))
        (unless (or (null position) (null name)
                    (string= (car imenu--rescan-item) name))
          (add-to-list 'symbol-names name)
          (add-to-list 'name-and-pos (cons name position))))))))

(global-set-key "\C-ci" 'ido-goto-symbol) ; or any key you see fit

;; Assembly
(push '("\\.[sS]$" . asm-mode) auto-mode-alist)
(add-hook 'asm-mode-hook
          (lambda ()
            (setq indent-tabs-mode t
                  asm-comment-char ?#)))

(eval-after-load 'ess
  '(push '("\\.s$" . asm-mode) auto-mode-alist))

;; Java
(add-hook 'java-mode-hook
          (lambda ()
            "Treat Java 1.5 @-style annotations as comments."
            (setq c-comment-start-regexp "(@|/(/|[*][*]?))")
            (modify-syntax-entry ?@ "< b" java-mode-syntax-table)))
(modify-coding-system-alist 'file "\\.java$" 'utf-8)
(setq cua-auto-tabify-rectangles nil)
(defadvice align (around smart-tabs activate)
  (let ((indent-tabs-mode nil)) ad-do-it))
(defadvice align-regexp (around smart-tabs activate)
  (let ((indent-tabs-mode nil)) ad-do-it))
(defadvice indent-relative (around smart-tabs activate)
  (let ((indent-tabs-mode nil)) ad-do-it))
(defadvice indent-according-to-mode (around smart-tabs activate)
  (let ((indent-tabs-mode indent-tabs-mode))
    (if (memq indent-line-function
              '(indent-relative
                indent-relative-maybe))
        (setq indent-tabs-mode nil))
    ad-do-it))
(defmacro smart-tabs-advice (function offset)
  (defvaralias offset 'tab-width)
  `(defadvice ,function (around smart-tabs activate)
     (cond
      (indent-tabs-mode
       (save-excursion
         (beginning-of-line)
         (while (looking-at "\t*\\( +\\)\t+")
           (replace-match "" nil nil nil 1)))
       (setq tab-width tab-width)
       (let ((tab-width fill-column)
             (,offset fill-column))
         ad-do-it))
      (t
       ad-do-it))))
(smart-tabs-advice c-indent-line c-basic-offset)
(smart-tabs-advice c-indent-region c-basic-offset)

(defun c-indent-hook ()
  (c-set-offset 'substatement-open 0)
  ;; (c-set-offset 'case-label '+)
  (setq indent-tabs-mode nil ; t
        tab-width 4 ; 2
        ))

(add-hook 'java-mode-hook 'c-indent-hook)
(add-hook 'c-mode-common-hook 'c-indent-hook)

;; Scala
(require 'scala-mode-auto)

;; Maude
(require 'maude-mode)
(setq maude-indent 2)

;; Diskusjon
(require 'diskusjon)
(setq diskusjon-lang "english"
      diskusjon-ascii nil)

(defun words ()
  (interactive)
  (setq diskusjon-lang "norsk"
        diskusjon-ascii nil)
  (diskusjon-mode))

(defun words-en ()
  (interactive)
  (setq diskusjon-lang "english"
        diskusjon-ascii t)
  (diskusjon-mode))

;; LaTeX
(modify-coding-system-alist 'file "\\.tex$" 'latin-1)
(modify-coding-system-alist 'file "\\.bib$" 'latin-1)
(setq-default TeX-PDF-mode t)
(setq bibtex-user-optional-fields
      '(("annote"       "Personal annotation (ignored)")
        ("isbn"         "ISBN")
        ("translator"   "Translator (or use the note field)")
        ("pages"        "Pages")
        ("url"          "URL")
        ("language"     "Language")
        ("howpublished" "How it was published")))

(setq bibtex-autokey-titleword-separator  "-"
      bibtex-autokey-year-title-separator ":-")

;; Scheme
(setq scheme-program-name "racket")
;; (require 'quack)
;; (quack-install)
(setq-default geiser-scheme-implementation "racket")
(font-lock-add-keywords 'scheme-mode
                        '(("(\\|)\\|\\[\\|\\]\\|{\\|}" . 'paren-face)))

(eval-after-load 'scheme
  '(paren-face-add-support scheme-font-lock-keywords-2))

;; paredit.el
(require 'paredit)
(add-hook 'emacs-lisp-mode-hook (lambda () (paredit-mode 1)))
(add-hook 'lisp-interaction-mode-hook (lambda () (paredit-mode 1)))
(add-hook 'lisp-mode-hook (lambda () (paredit-mode 1)))
(add-hook 'scheme-mode-hook (lambda () (paredit-mode 1)))
(define-key paredit-mode-map "\C-j" 'eval-print-last-sexp)

;; ElDoc
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-mode-hook 'turn-on-eldoc-mode)

;; imenu
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (add-to-list 'imenu-generic-expression
                         '("Tests"
                           "(\\(ert-deftest\\)\\s-+\\(\\(\\sw\\|\\s_\\)+\\)"
                           2) t)))

;; ERT
(require 'ert)

;; Evil
(setq evil-want-C-w-in-emacs-state t)
(setq evil-want-C-w-delete nil)
(setq evil-repeat-move-cursor nil)
;; (setq debug-on-error t ; debug
;;       ;; debug-on-signal t
;;       debug-ignored-errors nil)

(require 'evil)
(evil-mode 1)

(define-key evil-ex-map "e " 'ido-find-file)
(define-key evil-ex-map "w " 'ido-write-file)
(define-key evil-ex-map "b " 'ido-switch-buffer)
(define-key evil-ex-map "bd " 'ido-kill-buffer)

(define-key evil-normal-state-map [(tab)] 'evil-normal-state)
(define-key evil-insert-state-map [(tab)] 'evil-normal-state)
(define-key evil-visual-state-map [(tab)] 'evil-exit-visual-state)
(define-key evil-replace-state-map [(tab)] 'evil-normal-state)
(evil-declare-key 'insert comint-mode-map [(tab)] 'evil-normal-state)

(define-key evil-motion-state-map (kbd "¤") 'evil-end-of-line)
(define-key evil-motion-state-map (kbd "Æ") 'evil-forward-paragraph)
(define-key evil-motion-state-map (kbd "ø") 'evil-goto-mark)
(define-key evil-motion-state-map (kbd "Ø") 'evil-goto-mark-line)
(define-key evil-motion-state-map (kbd "å") 'backward-up-list)
(define-key evil-motion-state-map (kbd "æ") 'beginning-of-defun)
(define-key evil-motion-state-map (kbd "Å") 'evil-backward-paragraph)

(define-key evil-motion-state-map "g/" 'ido-goto-symbol)
(define-key evil-motion-state-map "g?" 'ido-goto-symbol)
(define-key evil-normal-state-map "gc" 'evil-comment-region)
(define-key evil-normal-state-map (kbd "©") 'evil-comment-region)
(define-key evil-visual-state-map ",c" 'comment-dwim)
(define-key evil-visual-state-map ",," 'eval-region)

(define-key evil-normal-state-map (kbd "M-.") nil)

(evil-define-operator evil-comment-region (beg end)
  :motion evil-line
  (evil-with-active-region beg end
    (call-interactively 'comment-dwim)))

;; surround
(require 'surround)
(global-surround-mode 1)

;; Limble
(require 'limble)
(add-hook 'emacs-lisp-mode-hook 'turn-on-limble-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-limble-mode)
(add-hook 'lisp-mode-hook 'turn-on-limble-mode)
(add-hook 'scheme-mode-hook 'turn-on-limble-mode)

(define-key evil-normal-state-map "\C-d" 'delete-char)

;; Info
(defadvice texinfo-make-menu (before fix activate)
  (when (or current-prefix-arg (region-active-p))
    (setq beginning (region-beginning)
          end (region-end))))

(add-hook 'texinfo-mode-hook
          (lambda ()
            (setq sentence-end-double-space t)))

;; Dired
(evil-declare-key 'normal dired-mode-map "ø" 'execute-extended-command)
(put 'narrow-to-region 'disabled nil)

(evil-define-text-object evil-a-comment ()
  "Select a comment."
  (list (evil-comment-beginning) (evil-comment-end)))

(define-key evil-outer-text-objects-map "c" 'evil-a-comment)

;; (evil-declare-key 'motion Info-mode-map
;;   "\t" 'Info-next-reference
;;   "n" 'Info-history-back
;;   "/" 'Info-history-forward
;;   "l" 'Info-top-node
;;   "d" 'Info-directory
;;   "y" 'evil-yank)

(defun evil-passthrough (&optional arg)
  "Execute the next command in Emacs state."
  (interactive "p")
  (cond
   (arg
    (add-hook 'post-command-hook 'evil-passthrough nil t)
    (evil-emacs-state))
   ((not (eq this-command #'evil-passthrough))
    (remove-hook 'post-command-hook 'evil-passthrough t)
    (evil-exit-emacs-state))))

(global-set-key (kbd "M-p") 'evil-passthrough)

;; Use Ex!
;; (global-set-key "\C-ci" nil)
;; (global-set-key "\C-x\C-f" nil)
;; (global-set-key "\C-xb" nil)
;; (global-set-key "\C-x\C-w" nil)
;; (global-set-key "\C-x\C-c" nil)

(evil-define-operator my-yank (beg end type register yank-handler)
  "Yank to global mark."
  :move-point nil
  :repeat nil
  (interactive "<R><x><y>")
  (if (and (fboundp 'cua--global-mark-active)
           (cua--global-mark-active))
      (progn
        (push-mark beg)
        (goto-char end)
        (cua-copy-to-global-mark))
    (evil-yank beg end type register yank-handler)))
;; (define-key evil-normal-state-map "y" 'my-yank)

(evil-ex-define-cmd "ls" 'list-buffers)
