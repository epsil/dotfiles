;; ~/.Xresources: Emacs.font: DejaVu Sans Mono-9
;; !!! Remember to xrdb -merge ~/.Xresources !!!
;; (set-frame-font "DejaVu Sans Mono-9")
;; (set-frame-font "Consolas-10.5") ; a little variation
;; (set-frame-font "Ubuntu Mono-10.5")
(setq ns-alternate-modifier 'none)
(setq ns-command-modifier   'meta)
(setq ns-function-modifier  'super)
(setq inhibit-startup-screen t)
(when (equal default-directory "/") (setq default-directory "~/"))
(set-scroll-bar-mode 'right)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(column-number-mode 1)
(when (window-system)
  (global-hl-line-mode 1)
  (set-face-background 'default "#ffffff")
  (set-face-background 'hl-line "#fffacd")
  (set-face-foreground 'region nil)
  (set-face-background 'region "lightgoldenrod2"))
(set-face-attribute 'fixed-pitch nil :family 'unspecified)

(setq user-full-name nil)
(setq auto-window-vscroll nil)
(setq-default indicate-empty-lines t)
(setq-default indent-tabs-mode nil)
(setq-default show-trailing-whitespace t)
(setq sentence-end-double-space nil)
(setq eval-expression-print-length nil)
(setq eval-expression-print-level nil)
(setq print-quoted t)
(setq sort-fold-case t)
(setq show-paren-delay 0)
(show-paren-mode 1)
(recentf-mode 1)
;; (transient-mark-mode -1)
;; (cua-mode 1)
(cua-selection-mode t)
(setq x-select-enable-clipboard t
      x-select-enable-primary t)
(ido-mode 1)
(setq ido-enable-flex-matching t)
(defalias 'yes-or-no-p 'y-or-n-p)
(global-auto-revert-mode t)
(global-set-key (kbd "C-x TAB") nil)

;; (setq emacs-lisp-mode-map (make-keymap))
;; (setq resize-mini-windows nil)
(setq sh-basic-offset 8)

(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(with-temp-buffer
  (cd user-emacs-directory)
  (cd "lisp")
  (add-to-list 'load-path default-directory)
  (normal-top-level-add-subdirs-to-load-path))

;; Smex
(require 'smex)
(smex-initialize)

;; http://groups.google.com/group/gnu.emacs.help/browse_thread/thread/97bca63d022d3f5f
(setq redisplay-dont-pause t
      ;; scroll-margin 1
      scroll-step 1
      ;; scroll-conservatively 10000
      ;; scroll-preserve-screen-position 1
      )

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(when (window-system)
  (set-frame-height (selected-frame) 35))

;; (global-set-key (kbd "M-2") "@")
;; (global-set-key (kbd "M-3") "£")
;; (global-set-key (kbd "M-4") "$")
;; (global-set-key (kbd "M-7") "{")
;; (global-set-key (kbd "M-8") "[")
;; (global-set-key (kbd "M-9") "]")
;; (global-set-key (kbd "M-0") "}")

;; (global-set-key (kbd "M-/") (lambda () (interactive) (insert "\\")))

;; (global-set-key (kbd "C-M-2") "@")
;; (global-set-key (kbd "C-M-3") "£")
;; (global-set-key (kbd "C-M-4") "$")
;; (global-set-key (kbd "C-M-7") "{")
;; (global-set-key (kbd "C-M-8") "[")
;; (global-set-key (kbd "C-M-9") "]")
;; (global-set-key (kbd "C-M-0") "}")
;; (global-set-key (kbd "C-S-U") 'ucs-insert)

;; (global-set-key (kbd "M-<down>") 'windmove-down)
;; (global-set-key (kbd "M-<up>") 'windmove-up)
;; (global-set-key (kbd "M-<left>") 'windmove-left)
;; (global-set-key (kbd "M-<right>") 'windmove-right)

;; (global-set-key (kbd "RET") 'newline-and-indent)

;; (global-set-key "(" 'skeleton-pair-insert-maybe)

;; (global-set-key (kbd "s-q") 'fill-paragraph)

(require 'parenface)
(set-face-foreground 'paren-face "blue4")
;; (set-face-foreground 'font-lock-string-face "VioletRed4")
(eval-after-load 'paren
  '(set-face-bold-p 'show-paren-match t))

(defun cleaner (&optional cleaner)
  "\"Who am I? I'll tell you who I am. I'm the CLEANER.\""
  (interactive)
  (save-excursion
    (unless (region-active-p)
      (mark-whole-buffer))
    ;; (untabify (region-beginning) (region-end))
    ;; (unless (eq 'text-mode major-mode)
    ;;   (indent-region (region-beginning) (region-end)))
    ;; (save-restriction
    ;;   (narrow-to-region (region-beginning) (region-end))
    ;;   (delete-trailing-whitespace))
    (asciify (region-beginning) (region-end))
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
                  asm-comment-char ?#)
            (define-key asm-mode-map [remap asm-comment] 'self-insert-command)))

;; Java
(add-hook 'java-mode-hook
          (lambda ()
            "Treat Java 1.5 @-style annotations as comments."
            (setq c-comment-start-regexp "(@|/(/|[*][*]?))")
            (setq tab-width 4)
            (setq indent-tabs-mode t)
            (c-set-offset 'func-decl-cont '++)
            (modify-syntax-entry ?@ "< b" java-mode-syntax-table)))
(modify-coding-system-alist 'file "\\.java$" 'utf-8)
(modify-coding-system-alist 'file "\\.txt$" 'utf-8)
(modify-coding-system-alist 'file "\\.md$" 'utf-8)
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
  (c-set-offset 'case-label '+)
  (setq indent-tabs-mode nil ; t
        tab-width 4 ; 2
        ))

(add-hook 'java-mode-hook 'c-indent-hook)
(add-hook 'c-mode-common-hook 'c-indent-hook)
(add-to-list 'auto-mode-alist '("\\.glsl" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.scss" . css-mode))
;; (add-to-list 'auto-mode-alist '("\\.cu" . c-mode))
(add-to-list 'auto-mode-alist '("\\.cup" . text-mode))
(add-to-list 'auto-mode-alist '("\\.cs$" . java-mode))

;; temp fix for Greasemonkey scripts
;; from Sylecn's ~/.emacs file
(eval-after-load 'js
  '(progn
     (setq js--regexp-literal-fix
	   "[^=][=(,:]\\(?:\\s-\\|\n\\)*\\(/\\)\\(?:\\\\.\\|[^/*\\]\\)\\(?:\\\\.\\|[^/\\]\\)*\\(/\\)")
     (setq js-font-lock-syntactic-keywords-fix
           ;; "|" means generic string fence
	   `((,js--regexp-literal-fix (1 "|") (2 "|"))))
     (setq js-font-lock-syntactic-keywords js-font-lock-syntactic-keywords-fix)))

;; JavaScript
(add-to-list 'auto-mode-alist '("\\.mjs" . js-mode))
(add-to-list 'auto-mode-alist '("\\.ts" . js-mode))
(add-to-list 'auto-mode-alist '("\\.jsx" . js-mode))
(setq js-indent-level 2) ; 4

;; Scala
(require 'scala-mode-auto)

;; ML
(setq sml-indent-level 2)
(add-hook 'sml-mode-hook (lambda () (setq evil-auto-indent nil)))

;; Maude
(require 'maude-mode)
(setq maude-indent 2)

;; Markdown
(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(setq auto-mode-alist
      (cons '("\\.md" . markdown-mode) auto-mode-alist))
(setq auto-mode-alist
      (cons '("\\.txt" . markdown-mode) auto-mode-alist))
;; (add-hook 'markdown-mode-hook #'visual-line-mode)

;; Diskusjon
(require 'diskusjon)
(setq diskusjon-lang "english" ; "norsk"
      diskusjon-ascii t)
(define-key diskusjon-mode-map "\C-cm" 'mediawiki-mode)
;; (define-key diskusjon-mode-map "\C-cM" 'markdown-mode)

(defun words ()
  (interactive)
  (diskusjon-mode)
  (setq diskusjon-lang "norsk"
        diskusjon-ascii t))

(defun words-en ()
  (interactive)
  (diskusjon-mode)
  (setq diskusjon-lang "english"
        diskusjon-ascii t))

;; MediaWiki
(require 'mediawiki)
(add-to-list 'auto-mode-alist '("\\.\\(media\\)?wiki$" . mediawiki-mode))
(add-hook 'mediawiki-mode-hook #'turn-on-visual-line-mode)
;; (add-hook 'mediawiki-mode-hook #'turn-on-diskusjon-punctuation-mode)

;; LaTeX/AUCTeX
;; (add-to-list 'load-path "/usr/share/emacs24/site-lisp/auctex")
(modify-coding-system-alist 'file "\\.tex$" 'latin-1)
(modify-coding-system-alist 'file "\\.bib$" 'latin-1)
(add-to-list 'auto-mode-alist '("\\.lhs$" . LaTeX-mode))
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

(eval-after-load 'latex
  '(progn
     (add-hook 'LaTeX-mode-hook #'turn-on-reftex)
     (add-hook 'LaTeX-mode-hook #'turn-on-auto-fill)
     ;; (add-to-list 'ispell-tex-skip-alists
     ;;              '("lstlisting\\*?" . "\\\\end[ 	\n]*{[ 	\n]*lstlisting\\*?[ 	\n]*}"))
     (setq LaTeX-verbatim-regexp "tikzpicture\\|lstlisting\\|[Vv]erbatim\\*?")
     (setq LaTeX-document-regexp "document\\|multicols\\|hyphenrules")
     (add-to-list 'LaTeX-verbatim-environments "code")
     (add-to-list 'LaTeX-verbatim-environments "Verbatim")
     (add-to-list 'LaTeX-verbatim-environments "lstlisting")
     (add-to-list 'LaTeX-verbatim-environments "tikzpicture")
     (add-to-list 'LaTeX-verbatim-macros-with-delims "lstinline")
     (add-to-list 'LaTeX-indent-environment-list
                  '("tikzpicture" current-indentation))
     (add-to-list 'LaTeX-indent-environment-list
                  '("lstlisting" current-indentation))
     (add-to-list 'LaTeX-indent-environment-list
                  '("code" current-indentation))
     (add-to-list 'LaTeX-indent-environment-list
                  '("Verbatim" current-indentation))
     (setq TeX-view-program-selection
           (assq-delete-all 'output-pdf TeX-view-program-selection))
     ;; (add-to-list 'TeX-view-program-selection '(output-pdf "xdg-open"))
     ;; (add-to-list 'TeX-view-program-selection '(output-pdf "open"))
     ))

;; Scheme
(setq scheme-program-name "racket")
;; (require 'quack)
;; (quack-install)
(require 'geiser)
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

(define-key evil-ex-map " " 'smex)
(define-key evil-ex-map "e " 'ido-find-file)
(define-key evil-ex-map "w " 'ido-write-file)
(define-key evil-ex-map "b " 'ido-switch-buffer)
(define-key evil-ex-map "bd " 'ido-kill-buffer)

;; (define-key evil-normal-state-map [(tab)] 'evil-normal-state)
;; (define-key evil-insert-state-map [(tab)] 'evil-normal-state)
;; (define-key evil-visual-state-map [(tab)] 'evil-exit-visual-state)
;; (define-key evil-replace-state-map [(tab)] 'evil-normal-state)
;; (evil-declare-key 'insert comint-mode-map [(tab)] 'evil-normal-state)

;; (unless (window-system)
;;   (define-key evil-normal-state-map "\C-i" 'evil-normal-state)
;;   (define-key evil-insert-state-map "\C-i" 'evil-normal-state)
;;   (define-key evil-visual-state-map "\C-i" 'evil-change-to-previous-state)
;;   (define-key evil-operator-state-map "\C-i" 'evil-change-to-previous-state)
;;   (define-key evil-replace-state-map "\C-i" 'evil-normal-state)
;;   (evil-declare-key 'insert comint-mode-map "\C-i" 'evil-normal-state))

;; (define-key evil-motion-state-map (kbd "¤") 'evil-end-of-line)
;; (define-key evil-motion-state-map (kbd "Æ") 'evil-forward-paragraph)
;; (define-key evil-motion-state-map (kbd "ø") 'evil-goto-mark)
;; (define-key evil-motion-state-map (kbd "Ø") 'evil-goto-mark-line)
;; (define-key evil-motion-state-map (kbd "å") 'backward-up-list)
;; (define-key evil-motion-state-map (kbd "æ") 'beginning-of-defun)
;; (define-key evil-motion-state-map (kbd "Å") 'evil-backward-paragraph)

(define-key evil-motion-state-map "g/" 'ido-goto-symbol)
(define-key evil-motion-state-map "g?" 'ido-goto-symbol)
(define-key evil-visual-state-map ",c" 'comment-dwim)
(define-key evil-visual-state-map ",," 'eval-region)
(define-key evil-normal-state-map (kbd "M-.") nil)

;; (define-key evil-visual-state-map ",x" 'diskusjon-del-tag)
(define-key evil-visual-state-map ",x" 'diskusjon-strike)
(define-key evil-visual-state-map ",p" 'diskusjon-code-tag)

(evil-ex-define-cmd "c" 'cleaner)

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

;; Ace
(require 'ace-jump-mode)
(global-set-key (kbd "C-c SPC") 'ace-jump-mode)
;; (define-key evil-motion-state-map "f" 'ace-jump-mode)

(defadvice ace-jump-mode (after evil activate)
  (recursive-edit))

(defadvice ace-jump-done (after evil activate)
  (exit-recursive-edit))

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

;; Terminal colors
(unless (window-system)
  (set-face-attribute 'font-lock-keyword-face nil :foreground "orange" :weight 'normal)
  (set-face-attribute 'font-lock-string-face nil :foreground "cyan")
  (set-face-attribute 'font-lock-comment-face nil :foreground "blue")
  (set-face-attribute 'font-lock-comment-delimiter-face nil :foreground "blue")
  (set-face-attribute 'font-lock-constant-face nil :foreground "green")
  (set-face-attribute 'paren-face nil :foreground "violet")
  (menu-bar-mode -1))

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
"j"

(define-key visual-line-mode-map [remap evil-next-line] 'evil-next-visual-line)
(define-key visual-line-mode-map [remap evil-previous-line] 'evil-previous-visual-line)

(remove-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
(add-hook 'haskell-mode-hook (lambda () (setq evil-auto-indent nil)))

;; (setq haskell-indentation-layout-offset 2
;;       haskell-indentation-starter-offset 1
;;       haskell-indentation-left-offset 2
;;       haskell-indentation-ifte-offset 2)

(defadvice evil-search-forward (before mark activate)
  "Store the current position in the '/ mark."
  (evil-set-marker ?/))

(defadvice evil-search-backward (before mark activate)
  "Store the current position in the '? mark."
  (evil-set-marker ??))

(evil-declare-key 'normal haskell-mode-map "=" 'indent-for-tab-command)
(put 'upcase-region 'disabled nil)

(evil-define-motion my-goto-line ()
  :jump t
  :type line
  (interactive)
  (evil-save-column
    (evil-goto-line)
    (forward-line -1)))

;; (define-key evil-motion-state-map "G" 'my-goto-line)

(setq evil-insert-state-modes (delq 'wdired-mode evil-insert-state-modes))

;; ELPA
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
;; (add-to-list 'package-archives
;;              '("melpa-stable" . "https://stable.melpa.org/packages/"))
;; (add-to-list 'package-archives
;;              '("melpa" . "https://melpa.org/packages/"))
;; (add-to-list 'package-archives
;;              '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives
             '("melpa-stable" . "http://stable.melpa.org/packages/") t)
;; You don't need this one if you have marmalade:
;; (add-to-list 'package-archives
;;  '("geiser" . "http://download.savannah.gnu.org/releases/geiser/packages"))
(package-initialize)

(setq password-cache-expiry nil)

(eval-after-load 'python
  '(define-key python-mode-map (kbd "RET") 'newline-and-indent))

(eval-after-load 'mediawiki
  '(modify-syntax-entry ?\" "\"" mediawiki-mode-syntax-table))

;; (eval-after-load 'electric
;;   '(add-to-list 'electric-pair-pairs '(?« . ?») t))

;; (electric-pair-mode 1)

(defun dwim-before ()
  (interactive)
  (cond
   ((memq this-command
          '(delete-backward-char
            evil-delete-backward-char
            evil-delete-backward-char-and-join))
    (cond
     ((or (and (not (bobp)) (not (eobp))
               (eq (char-after)
                   (cdr-safe (aref (syntax-table) (char-before)))))
          (and (looking-back "\"") (looking-at "\""))
          (and (looking-back "«") (looking-at "»")))
      (delete-char 1))))
   ;; ((eq this-command 'self-insert-command)
   ;;  (when (and (looking-at ")")
   ;;             (equal (this-command-keys) ")"))
   ;;    (forward-char)
   ;;    (setq this-command nil)))
   ))

(defun dwim-after ()
  (interactive)
  (cond
   ((memq this-command
          '(self-insert-command
            diskusjon-single-quote
            diskusjon-double-quote))
    (cond
     ((looking-back "^[ ]*- ")
      (insert "  "))
     ;; ((looking-back "^\\([#*]+\\) \\([#*]\\)")
     ;;  (save-excursion
     ;;    (backward-char 2)
     ;;    (delete-char 1))
     ;;  (insert " "))
    ;; ((looking-back "\\[")
    ;;   (save-excursion (insert "]")))
    ;; ((and (looking-at "\\]") (looking-back "\\]"))
    ;;  (delete-char -1)
    ;;  (forward-char))
    ;; ((looking-back "(")
    ;;   (save-excursion (insert ")")))
    ;; ((and (looking-at ")") (looking-back ")"))
    ;;  (delete-char -1)
    ;;  (forward-char))
    ((looking-back "«")
      (save-excursion (insert "»")))
    ((and (looking-at "»") (looking-back "»"))
     (delete-char -1)
     (forward-char))
    ((and (looking-back "=")
          (looking-back "^[= ]+"))
     (delete-char -1)
     (when (looking-back " ")
       (delete-char -1))
     (when (looking-at " ")
       (delete-char 1))
     (insert "= ")
     (save-excursion (insert " =")))
    ((and (looking-back "'+")
          (looking-at "'*$"))
     (cond
      ((and
        ;; (looking-back "\\([ ]+\\|^\\)'+")
        (looking-at "''")
        (looking-back "'''"))
       (save-excursion
         (insert "'")))
      ((looking-at "'+")
       (delete-char -1)
       (forward-char))
      ((and (looking-back "\\([ ]+\\|^\\)'+")
            (looking-back "''+" nil t))
       (save-excursion
         (if (looking-at "'")
             (insert "'")
           (insert (match-string 0)))))))))
   ((memq this-command
          '(delete-backward-char
            evil-delete-backward-char-and-join))
    (cond
     ;; ((looking-back "^\\([#*]+\\)")
     ;;  (delete-char -1)
     ;;  (insert " ")
     ;;  (when (looking-back "^ ")
     ;;    (delete-char -1)))
     ))))

(defvar diskusjon-punctuation-mode-hook nil)
(add-hook 'diskusjon-punctuation-mode-hook
          (lambda ()
            (message "Loading hooks ...")
            (add-hook 'pre-command-hook 'dwim-before nil t)
            (add-hook 'post-command-hook 'dwim-after nil t)))

;; (defvaralias 'evil-shift-width 'tab-width)

(setq-default evil-shift-width 4)

(eval-after-load 'mediawiki
  '(progn
     (add-to-list 'mediawiki-font-lock-keywords
                  '("<code>\\(\\(\n?.\\)*?\\)</code>"
                    (1 font-mediawiki-verbatim-face)) t)
     (add-to-list 'mediawiki-font-lock-keywords
                  '("<pre>\\(\\(\n\\|.\\)*?\\)</pre>"
                    (1 font-mediawiki-verbatim-face)) t)
     ))

(defadvice evil-paste-before (around save-column activate)
  (if (eq evil-this-type 'line)
      (evil-save-column
        ad-do-it)
    ad-do-it))

(defadvice evil-paste-after (around save-column activate)
  (if (eq evil-this-type 'line)
      (evil-save-column
        ad-do-it)
    ad-do-it))

;; (eval-after-load 'markdown-mode
;;   '(define-key markdown-mode-map "\"" 'diskusjon-double-quote))

(add-hook 'markdown-mode-hook
          (lambda ()
            ;; (if (string-match "wiki\\|gollum" (or (buffer-file-name) ""))
            ;;     (visual-line-mode 1)
            ;;   (turn-on-auto-fill))
            (visual-line-mode 1)))
(add-hook 'markdown-mode-hook #'turn-on-diskusjon-punctuation-mode)

(add-hook 'latex-mode-hook 'turn-on-auto-fill)

(defun asciify (beg end)
  "Replace typographical punctuation."
  (interactive "r")
  (save-excursion
    (replace-regexp "[  ]* [  ]*" " " nil beg end)
    (replace-regexp "[“”]" "\"" nil beg end)
    (replace-regexp "[‘’´]" "'" nil beg end)
    (replace-regexp "″" "\"" nil beg end)
    (replace-regexp "−" "-" nil beg end)
    (replace-regexp "[–―]" "--" nil beg end)
    (replace-regexp "—" "---" nil beg end)
    (replace-regexp "…" "..." nil beg end)
    (replace-regexp "•" "*" nil beg end)
    (replace-regexp "→" "->" nil beg end)
    (replace-regexp "\\$" "$" nil beg end)
    (replace-regexp "\n\n\n*" "\n\n" nil beg end)
    (delete-trailing-whitespace beg end)))

(defun htmlcleanup (beg end)
  "Remove tags."
  (interactive "r")
  (save-excursion
    (replace-regexp "<[^>]*>" "" nil beg end)
    (asciify beg end)))

(defalias 'ascify 'asciify)

;; (define-skeleton markdown-url
;;   "Hyperlenke."
;;   nil
;;   "[" _ "]"
;;   (let ((v1 (diskusjon-trim (skeleton-read "URL: "))))
;;     (format "(%s)" v1)))

(evil-define-text-object evil-inner-asterisk (count &optional beg end type)
  "Select inner double-asterisked expression."
  :extend-selection nil
  (evil-quote-range count beg end type ?\* ?\* t))

;; (define-key evil-visual-state-map ",u" 'markdown-url)
(define-key evil-visual-state-map "i*" 'evil-inner-asterisk)

;; (defadvice markdown-mode (after font-lock activate)
;;   "Disable font-lock-mode."
  ;; (message "hello from my markdown mode advice I HATE MARKDOWN MODE")
  ;; (font-lock-mode -1)
  ;; (call-interactively
  ;;  (lambda ()
  ;;    (interactive)
  ;;    (font-lock-mode -1)))
  ;; (add-hook 'post-command-hook 'markdown-mode-helper nil t))

;; (defun markdown-mode-helper ()
;;   (interactive)
;;   (font-lock-mode -1)
;;   (remove-hook 'post-command-hook 'markdown-mode-helper t))

;; (eval-after-load 'markdown-mode
;;   '(progn
;;      (define-key markdown-mode-map "\C-c\C-f" nil)
;;      (define-key markdown-mode-map "\C-c\C-fb" 'markdown-insert-bold)
;;      (define-key markdown-mode-map "\C-c\C-f\C-b" 'markdown-insert-bold)
;;      (define-key markdown-mode-map "\C-c\C-fe" 'markdown-insert-italic)
;;      (define-key markdown-mode-map "\C-c\C-f\C-e" 'markdown-insert-italic)))

;; (add-hook 'markdown-mode-hook
;;           (lambda ()
;;             (evil-delay t
;;                 '(font-lock-mode -1)
;;               'post-command-hook
;;               nil t)))

(require 'powershell-mode)
(add-to-list 'auto-mode-alist '("\\.ps1" . powershell-mode))

(defun unhex-region (beg end)
  (interactive "r")
  (let ((str (url-unhex-string (buffer-substring beg end))))
    (delete-region beg end)
    (insert str)))

(defun insert-link ()
  (interactive)
  (evil-execute-macro
   1
   [73 33 91 93 40 escape 65 41 escape 73 91 escape 65 93 40 escape 74 65 41 escape 106 106]))

(global-set-key "\C-ci" 'insert-link)

(load "lilypond-init")
