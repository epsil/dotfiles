SHELL = /bin/sh
EVIL = ../evil

.PHONY: compile clean

compile: clean
	emacs --batch -Q -L . -L $(EVIL) -f batch-byte-compile evil-ex2.el
	rm -f *.elc

clean:
	rm -f *~
	rm -f \#*\#
	rm -f *.elc

indent: clean
	emacs --batch evil-ex2.el -Q -L . -L $(EVIL) -l $(EVIL)/evil.el \
--eval "(dolist (buffer (reverse (buffer-list))) \
(when (buffer-file-name buffer) \
(set-buffer buffer) \
(message \"Indenting %s\" (current-buffer)) \
(setq-default indent-tabs-mode nil) \
(untabify (point-min) (point-max)) \
(indent-region (point-min) (point-max)) \
(delete-trailing-whitespace) \
(untabify (point-min) (point-max)) \
(goto-char (point-min)) \
(while (re-search-forward \"\\n\\\\{3,\\\\}\" nil t) \
(replace-match \"\\n\\n\")) \
(when (buffer-modified-p) (save-buffer 0))))"
