;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Information...
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;
;; Key maps
;;
;; Explanation by Kevin Rodgers on gnu-emacs-users
;;
;; You can use the kbd macro:
;; (global-set-key (kbd "<S-tab>") 'tetris)
;;
;; ?x is the read syntax for the character x.
;;
;; x is the read syntax for the symbol x.
;;
;; A string is delimited by double quotes, and by definition contains only
;; characters, so one writes "x" (not "?x", which has 2 characters).
;;
;; A vector is delimited by square brackets and can contain any Lisp
;; object, so [?\x] is a vector of 1 character and [x] is a vector of 1
;; symbol.
;;
;; So the character Control-t is ?\C-t, the string containing just that
;; character is "\C-t", and the corresponding vector is [?\C-t].
;;
;; The TAB character is ?\t, etc.  But the Tab key is handled like a
;; function key, which are represented by symbols e.g. tab or f1.  So
;; whereas (most) modified characters are also characters (e.g. ?\C-x or
;; ?\M-y), (all) modified function keys are also symbols (e.g. C-tab or
;; M-f1).
;;
;;;;;;;;;
;; Coding Systems
;;
;; There are an enormous number of ways to specify coding systems:
;; - via file cookies at the beginning of the file: -*- coding: utf-8 -*-
;; - via file cookies at the end of the file:  Local Variables: eval: (eldoc-mode)
;; - via filenames: auto-coding-alist
;; - via regexp on first few bytes of file: auto-coding-regexp-alist
;; - via arb function on buffer: auto-coding-functions
;; - via lisp call: set-buffer-process-coding-system, set-buffer-file-coding-system
;;
;; Also useful to know about universal-coding-system-argument, which
;; says "run the next command with given coding system"
;;
;;;;;;;;;;
;; Fonts
;;
;; Definitions:
;; Faces: logically distinct text (one for each text style of each mode)
;; Fonts: physically distinct rendering (one for each way to render text)
;; Fontset: ???
;; Many faces may use same font.
;;
;; Useful tidbits:
;; M-x x-select-font
;; (frame-parameter nil 'font)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; set up package and use-package
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'package)

(setq package-enable-at-startup nil)

(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
;; ;; Certificate expired.  Abandoned (?) according to
;; ;; https://www.emacswiki.org/emacs?action=browse;id=MarmaladeRepo
;; (add-to-list 'package-archives
;;             '("marmalade" . "http://marmalade-repo.org/packages") t)
(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tidbits
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; get rid of tool bar and tool tips
(tool-bar-mode -1)
(tooltip-mode -1)

;; Set initial window size if we're running in a windowing system
(if (display-graphic-p)
    (setq initial-frame-alist
          '((width . 160)
            (height . 60))))

;; answer y or n instead of yes or no
(fset 'yes-or-no-p 'y-or-n-p)

;; enable upcase and downcase commands
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;; Always make tabs into spaces
(setq-default indent-tabs-mode nil)

;; Don't count screen-wrapped lines as lines when moving around
(setq line-move-visual nil)

;; add local dir to search path
(add-to-list 'load-path "~/bin/elisp")
(add-to-list 'load-path "/usr/local/Cellar/maxima/5.41.0/share/maxima/5.41.0/emacs")

;; Put all backup files into one directory.
(setq make-backup-files t
      backup-by-copying t ;; Backup-by-move messes with dropbox
      vc-make-backup-files t
      backup-directory-alist '(("." . "~/.emacs.backup"))
      tramp-backup-directory-alist '(("." . "~/.emacs.backup"))
      kept-new-versions 5
      kept-old-versions 2
      delete-old-versions t
      version-control t)

;; E-mail for changelog entries
(setq add-log-mailing-address "greg.novak@gmail.com")

;; stop asking whether to save newly added abbrev when quitting emacs
(setq save-abbrevs nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Non-standard global key maps
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq ns-option-modifier 'meta
      ns-command-modifier 'meta)

;; transpose seems useless -- make it to-line instead
(bind-key "C-t" 'goto-line)

;;; Use ibuffer instead of list-buffers
(bind-key "C-x C-b" 'ibuffer)

;;; When I look up tags, I do it for documentation.  I like the info
;;; to appear in the other window and have the point stay in this
;;; window.  Achieve this by advising find-tag-other-window to stay in
;;; the current buffer, and swapping the conventional bindings for
;;; M-. and C-x 4 .
(bind-key "C-x 4." 'find-tag)
(bind-key "M-." 'find-tag-other-window)

;; Much prefer ediff to diff.
(bind-key "C-x v =" 'vc-ediff)

(bind-key "<M-up>" 'gsn/scroll-window-backward-line)
(bind-key "<M-down>" 'gsn/scroll-window-forward-line)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Global user key maps
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(bind-key "C-h B" 'describe-personal-keybindings)

(bind-key "C-c u" 'gsn/prev-window-this-frame)
(bind-key "C-c i" 'gsn/next-window-this-frame)
(bind-key "C-c j" 'gsn/prev-window-visible-frame)
(bind-key "C-c k" 'gsn/next-window-visible-frame)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Code for global key bindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun gsn/next-window-this-frame ()
  (interactive)
  (other-window 1))

(defun gsn/prev-window-this-frame ()
  (interactive)
  (other-window -1))

(defun gsn/next-window-visible-frame ()
  ;; inspired by next-multiframe-window
  (interactive)
  (select-window (next-window (selected-window)
			      (> (minibuffer-depth) 0)
			      'visible))
  (select-frame-set-input-focus (selected-frame)))

(defun gsn/prev-window-visible-frame ()
  ;; inspired by next-multiframe-window
  (interactive)
  (select-window (previous-window (selected-window)
				  (> (minibuffer-depth) 0)
				  'visible))
  (select-frame-set-input-focus (selected-frame)))

(defun gsn/scroll-window-forward-line (n)
  (interactive "p")
  (save-excursion
    (goto-char (window-start))
    (forward-line n)
    (set-window-start (selected-window) (point)))
  (forward-line n))

(defun gsn/scroll-window-backward-line (n)
  (interactive "p")
  (gsn/scroll-window-forward-line (- n)))

(defun gsn/scroll-window-forward-paragraph (n)
  (interactive "p")
  (let ((start-line (line-number-at-pos))
        (finish-line (progn (forward-paragraph n) (line-number-at-pos))))
    (save-excursion
      (goto-char (window-start))
      (forward-line (- finish-line start-line))
      (set-window-start (selected-window) (point)))))

(defun gsn/scroll-window-backward-paragraph (n)
  (interactive "p")
  (gsn/scroll-window-forward-paragraph (- n)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Advice
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defadvice switch-to-buffer (before gsn/existing-buffer activate compile)
  "When interactive, switch to existing buffers only (thus
   providing tab-completion), unless giving a prefix argument."
  (interactive
   (list (read-buffer "Switch to buffer: " (other-buffer)
                      (null current-prefix-arg)))))

(defadvice find-tag-other-window
    (after gsn/find-tag-other-window-stay-this-window activate compile)
  "Leave point in the current window when you call find-tag-other-window"
  (other-window 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Maybe-useful functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun gsn/duplex-print (buffer)
  "From Patrik Jonsson.  Prints the buffer using the Duplex
  feature of the color printer."
  (interactive (list (current-buffer)))
  (save-excursion
    (let ((ps-spool-duplex t)
          (ps-printer-name color-printer-name))
      (set-buffer buffer)
      (ps-spool-buffer-with-faces)
      (set-buffer "*PostScript*")
      (beginning-of-buffer)
      (insert "%!\ngsave\n%%BeginFeature: *Duplex\n")
      (insert "<< /Duplex true >> setpagedevice\n\n%%EndFeature\n")
      (end-of-buffer)
      (insert "grestore\n")
      (ps-despool))))

(defun mapc-buffers (fn)
  (mapc (lambda (buffer)
          (with-current-buffer buffer
            (funcall fn)))
        (buffer-list)))

;;;;;;;;;
;;; Fonts

(defun insert-font-samples (font-list)
  (dolist (font font-list)
    (let ((face-name (intern (concat "font-test-" font)))
          (test-text "The quick brown fox jumped over the lazy dog\n\n"))
      (custom-declare-face face-name `((t (:font ,font))) "Docstring")
      (message font)
      (insert (concat font "\n"))
      ;; some fonts are inexplicably not defined, even though they
      ;; show up in the x-select-font window.  Try to ignore
      ;; these cases
      (ignore-errors
        (insert (propertize test-text 'face face-name))))))

(defun font-sample-buffer ()
  (interactive)
  (switch-to-buffer (generate-new-buffer "*font-samples*"))
  (insert-font-samples (font-family-list)))

(defun prop-font ()
  (interactive)
  (set-face-attribute 'default nil :family "apple-verdana"))

(defun mono-font ()
  (interactive)
  (set-face-attribute 'default nil :family "menlo") )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Configure internal packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package compile
  :custom
  (compile-command "make -k -j 2 ")
  (compilation-scroll-output t "put the cursor at the end of the buffer")
  :bind
  ("C-c c" . compile))

(use-package ispell
  :custom
  (ispell-dictionary "english")
  (ispell-program-name "aspell")
  (ispell-extra-args '("-W" "2") "consider all 1-2 char words as correct"))

(use-package grep
  :custom
  ;; -n: include line numbers
  ;; -H: print filenames
  ;; -i: ignore case
  ;; -e: arg for the pattern to search for
  (grep-command "grep -nHi -e "))

(use-package doc-view
  :hook
  ;; Auto reload documents when file changes (for latex)
  (doc-view-mode . auto-revert-mode))

(use-package dired
  :bind
  (:map dired-mode-map ("v" . gsn/dired-find-file-other-window-stay-in-this-window))
  :config
  (defun gsn/dired-find-file-other-window-stay-in-this-window ()
    (interactive)
    (dired-find-file-other-window)
    (other-window 1)))

(use-package emacs-lisp-mode
  :bind
  (:map emacs-lisp-mode-map ("C-<tab>" . completion-at-point)))

(use-package calendar
  :custom
  (calendar-week-start-day 1 "Start on Mondays in the European fashion")
  (calendar-intermonth-text
   '(propertize
     (format "%2d"
             (car
              (calendar-iso-from-absolute
               (calendar-absolute-from-gregorian (list month day year)))))
     'font-lock-face 'font-lock-function-name-face)
   "Display ISO week numbers in calendar"))

(use-package comint
  :custom
  (ansi-color-for-comint-mode t)
  :hook ((kill-buffer . comint-write-input-ring)
         (kill-emacs . comint-write-input-ring-all-buffers)
         (comint-mode . turn-on-comint-history))

  :bind (:map comint-mode-map
              ("<up>" . gsn/comint-history-up)
              ("C-p" . gsn/comint-history-up)
              ("<down>" . gsn/comint-history-down)
              ("C-n" . gsn/comint-history-down))
  :config
  ;; Comint mode key maps
  ;; If the cursor is after the command prompt, make <up> and <down> do
  ;; command history matching rather than just sequentially pulling
  ;; commands from the history.  If the cursor is before the command
  ;; prompt, make <up> and <down> do the normal cursor-movement stuff.
  ;; Note that this means that you'll have to hit <left arrow> to get
  ;; the cursor off of the command line so that <up> and <down> start
  ;; moving the cursor instead of fooling around with the command history.
  (defun gsn/comint-history-up ()
    (interactive)
    (if (not (comint-after-pmark-p))
        ;; if we're before the prompt, move around
        (forward-line -1)
        ;; if we're after the prompt, do history matching
      (comint-previous-matching-input-from-input 1)
      ;; fool comint into thinking that the last command was
      ;; comint-... b/c of the way it remembers the search string
      (setq this-command 'comint-previous-matching-input-from-input)))

  (defun gsn/comint-history-down ()
    (interactive)
    (if (not (comint-after-pmark-p))
        (forward-line 1)
      (comint-next-matching-input-from-input 1)
      (setq this-command 'comint-next-matching-input-from-input)))

  (defun comint-write-input-ring-all-buffers ()
    (mapc-buffers 'comint-write-input-ring))

  ;; Persistent history in emacs comint-derived buffers, snarfed from:
  ;; http://oleksandrmanzyuk.wordpress.com/2011/10/23/a-persistent-command-history-in-emacs/

  (defun comint-write-history-on-exit (process event)
    (comint-write-input-ring)
    (let ((buf (process-buffer process)))
      (when (buffer-live-p buf)
        (with-current-buffer buf
          (insert (format "\nProcess %s %s" process event))))))

  (defun turn-on-comint-history ()
    (let ((process (get-buffer-process (current-buffer))))
      (when process
        (setq comint-input-ring-file-name
              (format "~/.emacs.d/inferior-%s-history"
                      (process-name process)))
        (comint-read-input-ring)
        (set-process-sentinel process
                              #'comint-write-history-on-exit)))))

(use-package python-mode
  :ensure t
  :custom
  ;; Matplotlib with interactive backends requires that python be
  ;; built as a framework with a command like this:
  ;;
  ;; env PYTHON_CONFIGURE_OPTS="--enable-framework" pyenv install 3.6.5
  ;;
  ;; However, under python3 built as a framework with matplotlib and
  ;; ipython installed, running a jupyter notebook, something about
  ;; the event loop gets messed up and it takes 10-20 seconds to
  ;; evaluate notebook cells.  This has something to do with
  ;; communication between the server and the kernel -- the kernel
  ;; itself evaluates the result instantly.
  ;;
  ;; So the upshot is that I've set up my system so that jupyter
  ;; notebooks work by default, and then if I'm running ipython under
  ;; emacs, I have to call the framework-built python specifically,
  ;; and override the config file setting for the matplotlib backend.
  (python-shell-interpreter "/Users/gregorynovak/.pyenv/versions/sf-py36-framework/bin/ipython")
  (python-shell-interpreter-args "--matplotlib=macos")
  ;; ;; Commands to start remote python shell
  ;; ;; -t to force ssh to allocate a tty
  ;; (setq python-shell-interpreter "/usr/bin/ssh")
  ;; (setq python-shell-interpreter-args "-t remote-host ipython")
  :bind
  ("C-c z" . gsn/py-windows)
  :config
  (defun gsn/py-windows ()
    "Set up windows the way I like them for python coding"
    (interactive)
    (delete-other-windows)
    (split-window-horizontally)
    ;; switch to the most recently visited python buffer
    (let ((buffers (buffer-list))
          done)
      (while (not done)
        (with-current-buffer (car buffers)
          (setq buffers (cdr buffers))
          (when (eql major-mode 'python-mode)
            (switch-to-buffer (current-buffer))
            (setq done t)))))
    (other-window 1)
    (switch-to-buffer "*Python*")
    (end-of-buffer)))

(use-package eshell
  :config
  (defun gsn/eshell-after-prompt-p ()
    "Check to see if you're on the last line of a buffer"
    (interactive)
    (let ((ppoint (point)))
      (save-excursion
        (end-of-buffer)
        (beginning-of-line)
        (eshell-skip-prompt)
        (>= ppoint (point)))))

  (defadvice eshell-previous-matching-input-from-input
      (around gsn/normal-up activate)
    (if (gsn/eshell-after-prompt-p)
        ad-do-it
      (forward-line -1)))

  (defadvice eshell-next-matching-input-from-input
      (around gsn/normal-down activate)
    (if (gsn/eshell-after-prompt-p)
        ad-do-it
      (forward-line 1)))

  ;; ;; Should have the binding defn below, but can't figure out how to
  ;; ;; get it to work...
  ;; ;;
  ;; ;; (use-package eshell ..) gives "symbol is void eshell-mode-map
  ;; ;; (use-package eshell-mode ...) doesn't set the keys
  ;; ;; (use-package eshell .. :requires eshell-mode) doesn't set the keys
  ;; ;;
  ;; ;; So instead use eshell-mode-hook
  ;; ;;
  ;; :bind
  ;; (:map eshell-mode-map (
  ;;              ("C-p" . eshell-previous-matching-input-from-input)
  ;;              ("C-n" . eshell-next-matching-input-from-input)))
  :hook
  (eshell-mode .
     (lambda ()
       (bind-key "C-p" 'eshell-previous-matching-input-from-input
                 eshell-mode-map)
       (bind-key "C-n" 'eshell-next-matching-input-from-input
                 eshell-mode-map))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org mode gets its own section
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package org
  :ensure t
  :bind
  (("C-c p" . gsn/org-plan)
   ("C-c r" . org-capture)
   ("C-c l" . org-store-link)
   ("C-c n" . gsn/org-now-i-am-working-on)
   (:map org-mode-map (("C-c w" . gsn/org-work-on-this))))

  :custom
  (org-directory "~/Dropbox/Brain")
  (org-hide-leading-stars t)
  (org-odd-levels-only t)
  (org-catch-invisible-edits 'smart)
  (org-list-demote-modify-bullet '(("-" . "+") ("+" . "-")))
  (org-default-notes-file (concat org-directory "/in.org"))
  (org-log-done 'time)
  (org-log-into-drawer t)  ;; For repeating tasks, put timestamps into the LOGBOOK drawer
  (org-table-auto-blank-field nil) ;; Don't clobber table cells by typing
  (org-enforce-todo-dependencies t)
  (org-use-fast-todo-selection t)
  (org-special-ctrl-a/e 'reversed) ;; go to actual bol first, then beginning of todo item

  (org-todo-keywords '((sequence "TODO" "DONE")
                       (type "READ"       ;; reading material
                             "PERIODIC"   ;; Periodic task
                             "EVENT"      ;; Do on specific day
                             "DEPENDENCY" ;; Requires me to do something else first
                             "BLOCKED"    ;; Requires someone else do do something
                             "DEFERRED"   ;; don't want to do it right now for
                             "NEXT"       ;; Next task for this project
                             "STARTED"    ;; actually started task
                             "|" "CANCELLED" "DONE")))

  (org-todo-keyword-faces '(("READ" . "cyan")
                            ("PERIODIC" . "brown")
                            ("EVENT" . "brown")
                            ("DEPENDENCY" . "brown")
                            ("BLOCKED" . "brown")
                            ("DEFERRED" . "brown")
                            ("NEXT" . "orange")
                            ("STARTED" . "black")
                            ("CANCELLED" . "red")))

  :config
  (add-to-list 'org-modules 'org-habit)
  (defun gsn/org-plan (arg)
    (interactive "P")
    (if arg
        (find-file-other-window "~/Dropbox/Brain/Desk.org")
      (find-file "~/Dropbox/Brain/Desk.org")))

  (defvar gsn/org-current-task)

  (defun gsn/org-work-on-this ()
    (interactive)
    (setq gsn/org-current-task (buffer-substring (point-at-bol) (point-at-eol))))

  (defun gsn/org-now-i-am-working-on ()
    (interactive)
    (if gsn/org-current-task
        (message gsn/org-current-task)
      "No current task")))

(use-package org-agenda
  :bind
   (("C-c a" . org-agenda))
  :custom
  (org-agenda-files (list org-directory))
  (org-agenda-include-diary t)
  (org-agenda-sorting-strategy  '((agenda time-up category-down habit-up)
                                  todo-state-down priority-down)
                                (todo priority-down category-keep)
                                (tags priority-down category-keep)
                                (search category-keep))
  :config
  (defadvice org-agenda
      (around gsn/org-agenda-prefer-existing-buffer activate)
    (if (member "*Org Agenda*" (mapcar 'buffer-name (buffer-list)))
        (switch-to-buffer "*Org Agenda*")
      ad-do-it)))

(use-package org-mobile
  ;; Mobile org wants all files in UTF-8, but the Emacs writes the
  ;; agenda file in iso-latin-1.  Force it to utf-8 for that file.
  ;; Unfortunately this seems to require hard coding the home directory.
  ;; :-(
  :config
  (add-to-list 'auto-coding-alist (cons "/Users/gregorynovak/Dropbox/Apps/MobileOrg/agendas.org" 'utf-8))
  :custom
  (org-mobile-directory "~/Dropbox/Apps/MobileOrg")
  (org-mobile-inbox-for-pull (concat org-directory "/from-mobile.org"))
  (org-mobile-use-encryption nil)
  ;; org-mobile-files '(org-agenda-files)
  (org-mobile-files (list (concat org-directory "/home.org")
                          (concat org-directory "/sf.org")
                          (concat org-directory "/work.org")
                          (concat org-directory "/notes.org")
                          (concat org-directory "/out.org")))
  (org-agenda-custom-commands
   ;; Included in default init for org
   '(("n" "Agenda and all TODO's" ((agenda "") (alltodo "")))
     ;; Add a custom agenda view showing errands so Mobile org generates it
     ("E" tags-todo "errand"))))


(use-package toc-org
  :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; External Packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package ediff
  :ensure t
  :custom
  (ediff-split-window-function 'split-window-horizontally))

(use-package magit
  :ensure t
  :bind
  ("C-c v" . magit-status))

(use-package ein
  :ensure t)

(use-package restclient
  :ensure t)

(use-package tex-site
  :ensure auctex
  :config
  (setq TeX-PDF-mode t
	LaTeX-table-label "tab-"
	LaTeX-equation-label "eq-"
	LaTeX-eqnarray-label "eq-"
	LaTeX-figure-label "fig-"
	LaTeX-section-label   '(("chapter" . "ch-")
				("section" . "sec-")
				("subsection" . "sec-"))))

(use-package sql
  ;; Config relies heavily on:
  ;; http://truongtx.me/2014/08/23/setup-emacs-as-an-sql-database-client/

  :hook
  (sql-mode . gsn/enable-sql-upcase)
  (sql-interactive-mode . gsn/enable-sql-upcase)
  (sql-interactive-mode . gsn/do-not-wrap-lines)
  (pre-abbrev-expand . gsn/only-upcase-in-code-context)

  :custom
  (sql-send-terminator ";")
  ;; Set the default sql product to presto.  This allows
  ;; sql-send-paragraph to find the comint buffer.  You can also drop
  ;; this line at the top of a file to have that file talk to a
  ;; different sql product by default:
  ;; -- -*- sql-product: postgres -*-
  (sql-product 'presto)
  (sql-postgres-program "psql")
  (sql-presto-program "sane-presto")
  (sql-presto-login-params '((user :default "novak")
                             (database :default "novak")
                             server))
  ;; Trying to give the password on the command line confuses mysql
  ;; Give it as an env var inside my-sql-connect
  (sql-mysql-login-params '(user database server))

  :config
  (add-to-list 'sql-product-alist
               '(presto
		 :name "Presto"
		 :free-software t
		 :font-lock sql-mode-postgres-font-lock-keywords
		 :sqli-program sql-presto-program
		 ;; :sqli-options sql-presto-options
		 :sqli-login sql-presto-login-params
		 :sqli-comint-func sql-comint
		 ;; :list-all ()
		 ;; :list-table ()
		 ;; :completion-object sql-presto-completion-object
		 ;; :completion-column ()
		 :prompt-regexp "^\\w*[#>] "
		 :prompt-length 8
		 :prompt-cont-regexp "^\\w*[-(]*[#>] "
		 :input-filter sql-remove-tabs-filter
		 ;; :statement
		 ;; :syntax-alist
		 :terminator ("\\(^\\s-*\\\\g$\\|;\\)" . "\\g")))

  (define-abbrev-table 'sql-mode-abbrev-table
    (mapcar #'(lambda (v) (list v (upcase v) nil 1))
            '("absolute" "action" "add" "after" "all" "allocate" "alter" "and" "any" "are" "array" "as" "asc" "asensitive" "assertion" "asymmetric" "at" "atomic" "authorization" "avg" "before" "begin" "between" "bigint" "binary" "bit" "bitlength" "blob" "boolean" "both" "breadth" "by" "call" "called" "cascade" "cascaded" "case" "cast" "catalog" "char" "char_length" "character" "character_length" "check" "clob" "close" "coalesce" "collate" "collation" "column" "commit" "condition" "connect" "connection" "constraint" "constraints" "constructor" "contains" "continue" "convert" "corresponding" "count" "create" "cross" "cube" "current" "current_date" "current_default_transform_group" "current_path" "current_role" "current_time" "current_timestamp" "current_transform_group_for_type" "current_user" "cursor" "cycle" "data" "date" "day" "deallocate" "dec" "decimal" "declare" "default" "deferrable" "deferred" "delete" "depth" "deref" "desc" "describe" "descriptor" "deterministic" "diagnostics" "disconnect" "distinct" "do" "domain" "double" "drop" "dynamic" "each" "element" "else" "elseif" "end" "equals" "escape" "except" "exception" "exec" "execute" "exists" "exit" "external" "extract" "false" "fetch" "filter" "first" "float" "for" "foreign" "found" "free" "from" "full" "function" "general" "get" "global" "go" "goto" "grant" "group" "grouping" "handler" "having" "hold" "hour" "identity" "if" "immediate" "in" "indicator" "initially" "inner" "inout" "input" "insensitive" "insert" "int" "integer" "intersect" "interval" "into" "is" "isolation" "iterate" "join" "key" "language" "large" "last" "lateral" "leading" "leave" "left" "level" "like" "limit" "local" "localtime" "localtimestamp" "locator" "loop" "lower" "map" "match" "map" "member" "merge" "method" "min" "minute" "modifies" "module" "month" "multiset" "names" "national" "natural" "nchar" "nclob" "new" "next" "no" "none" "not" "null" "nullif" "numeric" "object" "octet_length" "of" "old" "on" "only" "open" "option" "or" "order" "ordinality" "out" "outer" "output" "over" "overlaps" "pad" "parameter" "partial" "partition" "path" "position" "precision" "prepare" "preserve" "primary" "prior" "privileges" "procedure" "public" "range" "read" "reads" "real" "recursive" "ref" "references" "referencing" "relative" "release" "repeat" "resignal" "restrict" "result" "return" "returns" "revoke" "right" "role" "rollback" "rollup" "routine" "row" "rows" "savepoint" "schema" "scope" "scroll" "search" "second" "section" "select" "sensitive" "session" "session_user" "set" "sets" "signal" "similar" "size" "smallint" "some" "space" "specific" "specifictype" "sql" "sqlcode" "sqlerror" "sqlexception" "sqlstate" "sqlwarning" "start" "state" "static" "submultiset" "substring" "sum" "symmetric" "system" "system_user" "table" "tablesample" "temporary" "then" "time" "timestamp" "timezone_hour" "timezone_minute" "to" "trailing" "transaction" "translate" "translation" "treat" "trigger" "trim" "true" "under" "undo" "union" "unique" "unknown" "unnest" "until" "update" "upper" "usage" "user" "using" "value" "values" "varchar" "varying" "view" "when" "whenever" "where" "while" "window" "with" "within" "without" "work" "write" "year" "zone" "greatest" "least")))

  (defun gsn/enable-sql-upcase ()
    (abbrev-mode 1)
    ;; Make underscore a word character so that abbrev stops expanding
    ;; send_count to send_COUNT
    (modify-syntax-entry ?_ "w" sql-mode-syntax-table))

  (defun gsn/do-not-wrap-lines ()
    (toggle-truncate-lines t))

  (defun gsn/only-upcase-in-code-context ()
    ;; Allow our abbrevs only in a code context.
    (setq local-abbrev-table
          (if (sql-in-code-context-p)
              sql-mode-abbrev-table)))

  (defun sql-in-code-context-p ()
    (if (fboundp 'buffer-syntactic-context) ; XEmacs function.
        (null (buffer-syntactic-context))
      ;; Attempt to simulate buffer-syntactic-context
      ;; I don't know how reliable this is.
      (let* ((beg (save-excursion
 		    (beginning-of-line)
 		    (point)))
 	     (list
 	      (parse-partial-sexp beg (point))))
        (and (null (nth 3 list))	; inside string.
 	     (null (nth 4 list))))))	; inside comment

  (defun my-sql-connect (product connection)
    "Actually connect to a database"
    ;; load the password
    ;;  (require 'sql-my-password "sql-my-password.el.gpg")
    (let* ((sql-product product)
           (password (cadr (assoc connection sql-my-password)))
           (sql-connection-alist (cons (list 'password password)
                                       sql-connection-alist)))
      ;; Postgres doesn't allow providing password on command line,
      ;; handle that case.  This should go inside sql-postgres.
      (when (and (eq sql-product 'postgres) password)
        (setenv "PGPASSWORD" password))
      (when (and (eq sql-product 'mysql) password)
        (setenv "MYSQL_PWD" password))
      (sql-connect connection)
      (when (and (eq sql-product 'postgres) password)
        (setenv "PGPASSWORD" nil))
      (when (and (eq sql-product 'mysql) password)
        (setenv "MYSQL_PWD" nil)))
    (rename-buffer (concat "*SQL-" (symbol-name connection) "*")))

  (defun sql-presto ()
    "stub to connect to presto"
    (interactive)
    (let ((sql-product 'presto))
      (sql-connect 'presto)))

  (defun sql-fashionthing ()
    "stub to connect to fashionthing"
    (interactive)
    (my-sql-connect 'postgres 'fashionthing))

  (defun sql-production ()
    "stub to connect to transmetro"
    (interactive)
    (my-sql-connect 'postgres 'production))

  (defun sql-hms ()
    "stub to connect to hive metastore"
    (interactive)
    (my-sql-connect 'mysql 'hms))

  (defadvice sql-send-string
      (around gsn/ask-for-sqli-buffer activate)
    "Don't just bail with an error, ask for a sql buffer"
    (unless (sql-buffer-live-p sql-buffer)
      (call-interactively 'sql-set-sqli-buffer))
    ad-do-it))

(use-package sql-indent
  :ensure t)

(use-package slime
  :ensure t
  ;; Slime and remote servers: swank only listens on the loopback
  ;; interface, so writing stuff via 'socket 127.0.0.1 4005' works but
  ;; 'socket my-ip 4005' doesn't work.  For ssh forwarding, you have to
  ;; _forward to_ 127.0.0.1 _and_ write to 127.0.0.1 (the loopback on
  ;; both machines) to get slime to work.  Anything else doesn't work!
  :bind
  (:map slime-mode-map (("C-c s" . slime-selector)
                        ("M-n" . gsn/slime-next-note)
                        ("M-p" . gsn/slime-previous-note)
                        ("<C-tab>" . completion-at-point))
   :map slime-repl-mode-map (("C-c s" . slime-selector)
                             ("C-p" . slime-repl-previous-input)
                             ("C-n" . slime-repl-next-input)))
  :custom
  (slime-lisp-implementations `((sbcl ("/usr/local/bin/sbcl"))
                                (clisp ("/usr/local/bin/clisp"))))
  (common-lisp-hyperspec-root "/usr/local/share/doc/hyperspec/HyperSpec/")
  (common-lisp-hyperspec-symbol-table (concat common-lisp-hyperspec-root "Data/Map_Sym.txt"))
  (common-lisp-hyperspec-issuex-table (concat common-lisp-hyperspec-root "Data/Map_IssX.txt"))

  (inferior-lisp-program "sbcl")
  ;; (lisp-indent-function 'common-lisp-indent-function)

  :config
  (slime-setup '(slime-repl slime-fancy slime-asdf slime-banner))
  (add-to-list 'slime-completion-at-point-functions 'slime-fuzzy-complete-symbol)

  (defadvice slime-repl-previous-input (around gsn/slime-normal-up activate)
    "Make <up> do history completion after (point) and movement before"
    (if (>= (point) slime-repl-input-start-mark)
        ad-do-it
      (forward-line -1)))

  (defadvice slime-repl-next-input (around gsn/slime-normal-down activate)
    "Make <down> do history completion after (point) and movement before"
    (if (>= (point) slime-repl-input-start-mark)
        ad-do-it
      (forward-line 1)))

  (defvar gsn/slime-scan-note-by-type-history nil
    "List storing history of entries to minibuffer prompt in
  gsn/slime-read-type")

  (defvar gsn/slime-scan-note-by-type-current
    '(:error :read-error :warning :style-warning :note)
    "Current type of note for which to search.  This is stored so
  that the behavior is 'sticky' between invocations of the
  commands.")

  (defun gsn/slime-next-note (reset-type)
    "Interactively search for the next compiler note of the type
given by gsn/slime-scan-note-by-type-current.  With prefix arg,
prompt for the value of gsn/slime-scan-note-by-type-current."
    (interactive "P")
    (when reset-type
      (gsn/slime-read-type))
    (gsn/slime-next-note-by-type gsn/slime-scan-note-by-type-current))

  (defun gsn/slime-previous-note (reset-type)
    "Interactively search for the previous compiler note of the
type given by gsn/slime-scan-note-by-type-current.  With prefix
arg, prompt for the value of
gsn/slime-scan-note-by-type-current."
    (interactive "P")
    (when reset-type
      (gsn/slime-read-type))
    (gsn/slime-previous-note-by-type gsn/slime-scan-note-by-type-current))

  (defun gsn/slime-read-type ()
    "Prompt for the value of gsn/slime-scan-note-by-type-current.
Store history (as strings) in
gsn/slime-scan-note-by-type-history.  Convert the string to a
symbol and set gsn/slime-scan-note-by-type-current."
    (let ((type-string (completing-read "Type (default error): "
                                        '("all"
                                          "error"
                                          "read-error"
                                          "warning"
                                          "style-warning"
                                          "note")
                                        nil t nil
                                        'gsn/slime-scan-note-by-type-history
                                        "error")))
      (push type-string gsn/slime-scan-note-by-type-history)
      (setq gsn/slime-scan-note-by-type-current
            (cdr (assoc type-string
                        '(("all" . (:error :read-error :warning
                                           :style-warning :note))
                          ("error" . (:error))
                          ("read-error" . (:read-error))
                          ("warning" . (:warning))
                          ("style-warning" . (:style-warning))
                          ("note" . (:note))))))))

  (defun gsn/slime-scan-note-by-type (type scan-func)
    "Move point to the next/previous compiler note of type TYPE.
SCAN-FUNC specifies how to advance through the notes so that this
function doens't have to be duplicated for -next- and -previous-"
    (let ((original-pos (point))
          (last-pos (point))
          (sought-note-p
           (lambda (type)
             (and (slime-note-at-point)
                  (memq (overlay-get (slime-note-at-point) 'severity)
                        type)))))

      (funcall scan-func)
      (while (and (/= last-pos (point))
                  (not (funcall sought-note-p type)))
        (setq last-pos (point))
        (funcall scan-func))

      ;; let the user know if there are no more notes
      (if (funcall sought-note-p type)
          (slime-show-note (slime-note-at-point))
        ;; If no next note, go back to where you started
        (goto-char original-pos)
        (message "No more notes."))))

  (defun gsn/slime-next-note-by-type (type)
    "Move point to the next compiler note of type TYPE."
    (gsn/slime-scan-note-by-type type 'slime-find-next-note))

  (defun gsn/slime-previous-note-by-type (type)
    "Move point to the next/previous compiler note of type TYPE."
    (gsn/slime-scan-note-by-type type 'slime-find-previous-note))

  (defun gsn/slime-eval-last-expression-in-frame (sexp)
    "Eval the expression at POINT in a frame in the slime debugger.
  The frame used is determined by the location of POINT in the
  slime debugger buffer.  The idea is to be able to be able to
  debug by using sldb-show-source in the slime debug buffer and
  then easily evaluate expression in your source file near the
  problem."
    (interactive (list (slime-last-expression)))
    (if (not (sldb-get-default-buffer))
        (error "No debugger buffer")
      (save-excursion
        (set-buffer (sldb-get-default-buffer))
        (sldb-eval-in-frame sexp)))))

(use-package ess
  :ensure t
  ;; Sweet lord, you absolutely may not mess with the underscore key
  ;; because your language has an assignment operator that requires
  ;; three key presses.
  :bind (:map inferior-ess-mode-map ("_" . self-insert-command)))

(use-package js-comint
  :ensure t
  :custom
  (inferior-js-program-command "js")
  :bind
  (:map js-comint-mode-map (("C-x C-e" . js-send-last-sexp)
                            ("C-M-x" . js-send-last-sexp-and-go)
                            ("C-c b" . js-send-buffer)
                            ("C-c C-b" . js-send-buffer-and-go)
                            ("C-c l" . js-load-file-and-go))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Non elpa / melpa packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package gsn)

(use-package point-stack
  :bind
  (("C-c m" . point-stack-push)
   ("C-c b" . point-stack-pop)))

(use-package maxima
  :bind
  (:map maxima-mode-map (("C-m" . gsn/inferior-maxima-check-and-send-line)))
  :config
  (defadvice inferior-maxima-check-and-send-line
      (before gsn/ensure-trailing-semicolon activate)
    (end-of-line)
    (if (not (eq (preceding-char) 59))
        (insert ";"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Local customizations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (file-exists-p "~/.emacs_local")
  (load "~/.emacs_local"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Auto-managed custom section
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (ein restclient use-package toc-org sql-indent slime python-mode magit js-comint ess auctex))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
