;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Set up flags for system specific stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq moving-mail nil)

; Options set manually in .emacs.local
(setq clio-flag nil
      thalia-flag nil
      dionysus-flag nil
      euterpe-flag nil
      pleiades-flag nil)

;; I use the same .emacs file on many machines.  Occasionally I want
;; emacs to do different things based on where I'm running it.
;; .emacs.local contains a single line like this:
;; (setq euterpe-flag t)
(when (file-exists-p "~/.emacs.local")
  (load "~/.emacs.local"))

; Options which should be set automatically below
(setq remote-flag nil)

; Try to detect when ssh has set DISPLAY to something funny
; indicating that it's forwarding the X11 connection over the 
; ssh link.
(unless (string-match ":0." (or (getenv "DISPLAY") ""))
  (setq remote-flag t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Fonts
;; 
;; Definitions:
;; Faces: logically distinct text (one for each text style of each mode)
;; Fonts: physically distinct rendering (one for each way to render text)
;; Fontset: ???
;; Many faces may use same font.
;; 
;; Useful functions: 
;; x-font-list, x-list-fonts, set-default-font
;; list-fontsets, describe-fontset
;; 
;; Useful programs: 
;; xfontsel
;;
;; Useful tidbits:
;; Shift-click brings up font menu
;; (frame-parameter nil 'font)
;; (setq default-frame-alist '((width  . 82)
;;                            (height . 48)
;;                            (font . "fontset-mac")))

(defun make-font-sample-buffer (font-list) 
  (with-current-buffer (generate-new-buffer "*font-samples*")
    (dolist (font font-list)
      (let ((face-name (intern (concat "font-test-" font))))
 	(custom-declare-face face-name `((t (:font ,font))) "Docstring")
	(insert font)
	(insert (propertize test-text 'face face-name))))))

(defun prop-font ()
  (interactive)
  (set-face-attribute 'default nil :family "apple-verdana"))

(defun mono-font ()
  (interactive)
  (set-face-attribute 'default nil :font 
   "-apple-lucida grande ce-medium-r-normal--0-0-0-0-m-0-mac-centraleurroman"))

; Force mono font
;(when (featurep 'mac-carbon)
;  (mono-font))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Paths
(add-to-list 'load-path "~/bin/elisp")

(when thalia-flag
  (add-to-list 'load-path "/opt/local/share/emacs/site-lisp/slime"))

(when clio-flag
  (add-to-list 'load-path "/opt/local/share/maxima/5.24.0/emacs/"))

;; (add-to-list 'Info-directory-list "/usr/share/info")

(when (or thalia-flag clio-flag)
  (add-to-list 'exec-path "/opt/local/bin"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Starting loading packages, etc
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Yikes... take away the disgusting new parts of emacs
(when (>= emacs-major-version 22)
  (tool-bar-mode -1)
  (tooltip-mode -1))

;; type "y"/"n" instead of "yes"/"no"
(fset 'yes-or-no-p 'y-or-n-p)

(setq-default indent-tabs-mode nil)

(setq comint-input-ring-size 500
      message-log-max 500
      font-lock-maximum-size 1024000
      woman-cache-filename "~/.woman-cache.el"
      ;; When running ispell, consider all 1-3 character words as correct.
      ;; ispell-extra-args '("-W" "3")
      color-printer-name "hp"
      ;; default to better frame titles
      frame-title-format (concat  "%b - emacs@" (system-name)))      

;; Put all backup files into one directory.
(setq make-backup-files t      
      vc-make-backup-files t            
      backup-directory-alist '(("." . "~/.emacs.backup"))
      tramp-backup-directory-alist '(("." . "~/.emacs.backup"))
      kept-new-versions 5
      kept-old-versions 2
      delete-old-versions t ; Don't ask before deleting
      ;; delete-old-versions nil ; Ask before deleting
      version-control t)

;; Make conditional loads more readable: require the mode
;; if it's available
(defun request (name)
  (when (locate-library (symbol-name name))
    (require name)))

;; More general form of above to allow arbitrary initialization.
(defmacro request-and-init (name &rest body)
  (if (consp name)
      `(when (and ,@(mapcar (lambda (x) 
                              `(locate-library (symbol-name ',x))) 
                            name))
         ,@(mapcar (lambda (x) 
                     `(require ',x))
                   name)
         ,@body)    
    `(when (locate-library (symbol-name ',name))
       (require ',name)
       ,@body)))


(request 'gsn)
(request 'point-stack)
(request 'eform-mode)
(request 'calendar)
;;(load "cm.el")

(request 'ee-autoloads)

(request-and-init miniedit
  (miniedit-install))

;; (defadvice TeX-command-master
;;   (around gsn/switch-to-thesis activate)
;;   "If thesis.tex is loaded, switch to it before running tex commands"
;;   (save-excursion
;;     (when (or (string-match "chapter-" (buffer-name))
;;               (string-match "appendix-" (buffer-name))
;;               (string-match "abstract.tex" (buffer-name)))
;;       (unless (member "thesis.tex" (mapcar 'buffer-name (buffer-list)))
;;         (find-file "~/Papers/2008/thesis/thesis.tex"))
;;       (set-buffer "thesis.tex"))
;;     ad-do-it))

(defadvice switch-to-buffer (before gsn/existing-buffer activate compile)
  "When interactive, switch to existing buffers only, unless giving a
   prefix argument."
  (interactive
   (list (read-buffer "Switch to buffer: " (other-buffer)
                      (null current-prefix-arg)))))

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

;; Display ISO week numbers in calendar
(setq calendar-week-start-day 1
      calendar-intermonth-text
      '(propertize
        (format "%2d"
                (car
                 (calendar-iso-from-absolute
                  (calendar-absolute-from-gregorian (list month day year)))))
        'font-lock-face 'font-lock-function-name-face))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Life -- org mode
;; org-remember deprecated.
;;(request-and-init (org remember)
;;  (org-remember-insinuate))

;; Org install instructions say that this helps with autoloads.
(require 'org-install)

(defvar gsn/org-current-task)

(defun gsn/org-work-on-this ()
  (interactive)
  (setq gsn/org-current-task (buffer-substring (point-at-bol) (point-at-eol))))

(defun gsn/org-what-am-i-working-on ()
  (interactive)
  (if gsn/org-current-task
      (message gsn/org-current-task)
      "No current task"))

(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))

(setq org-directory "~/Dropbox/Brain"
      org-default-notes-file "~/Dropbox/Brain/in.org"      
      org-hide-leading-stars t
      org-odd-levels-only t
      org-log-done t
      org-table-auto-blank-field nil
      org-enforce-todo-dependencies t
      org-list-demote-modify-bullet '(("-" . "+") ("+" . "-"))
      org-tags-exclude-from-inheritance '("project")
      ;; STARTED NEXT
      org-todo-keywords '((sequence "TODO" "DONE")
                          (sequence "PENDING" "DELEGATED" "SOMEDAY" 
                           "NEXT" "|" "DONE" "CANCELLED"))
      org-todo-keyword-faces '(("PENDING" . "orange")
                               ("DELEGATED" . "orange")
                               ("SOMEDAY" . "orange"))
      ;; org-mobile-index-file "index.org"
      org-mobile-force-id-on-agenda-items t
      org-mobile-inbox-for-pull (concat org-directory "/from-mobile.org")
      org-mobile-use-encryption nil
      org-mobile-files (list (concat org-directory "/home.org")
                             (concat org-directory "/work.org")
                             )
      org-mobile-directory "~/Dropbox/MobileOrg")


;; (setq org-use-fast-todo-selection t)
      ;; org-highest-priority "A"
      ;; org-default-priority "C"
      ;; org-lowest-priority "E"      
      ;; org-special-ctrl-a/e t
      ;; org-agenda-include-diary t
      ;; org-reverse-note-order 
      ;; org-refile-targets
      ;; org-cycle-include-plain-lists

      ;; org-remember-templates 
      ;; could set this up with a few functions that will
      ;; automatically generate dated todos, undated todos, and notes.

      ;; org-agenda-todo-list-sublevels
      ;; I think I'd like to opposite of this: Show "leaves" but don't
      ;; show the higher level stuff

(setq org-refile-targets '( (org-agenda-files :level . 1) )
      org-link-abbrev-alist 
      '(("arxiv" . "http://arxiv.org/abs/")
        ("arXiv" . "http://arxiv.org/abs/")
        ("arx" . "http://arxiv.org/abs/")
        ("xxx" . "http://arxiv.org/abs/")
        ("google"   . "http://www.google.com/search?q=")
        ("ads" . "http://adsabs.harvard.edu/cgi-bin/nph-abs_connect?author=%s&db_key=AST")))

;; (request 'org-toc)

;; (add-hook org-mode-hook           
;;           (lambda () 
;;             ;; org-mode itself seems to reset this, so set it when
;;             ;; entering the buffer
;;             (setq org-todo-keywords '((sequence "TODO" "NEXT" "DONE")
;;                                       (sequence "PENDING" "SOMEDAY" "CANCELLED")))))

;; org-refile-targets is a bit of a bear, documentation is a little
;; murky and google doens't find any examples.  Don't forget to
;; refresh the list of targets with
;;   (setq org-refile-target-table nil)
;; or 
;;   C-u C-u C-c C-w 
;; The following list is in order of precedence.  For example, if a
;; symbol has both an associated value and function, then the function
;; is called.

;; (setq org-refile-targets 
;;       ;; special treatment, calls (org-agenda-files) to expand into an
;;       ;; explicit list of filenames (rather than a directory)
;;       '((org-agenda-files :keyword . arg))  
;;       ;; calls function, can produce string, list of strings, list of
;;       ;; buffers
;;       (function-name :keyword . arg)     
;;       ;; alternate rendering, perhaps more readable depending on one's
;;       ;; taste
;;       (function-name . (:keyword . arg)) 
;;       ;; use value, can be string, list of strings, list of buffers
;;       (variable-name :keyword . arg)     
;;       ;; Give a specific filename
;;       ("filename" :keyword . arg)
;;       ;; Give a list of specific filenames
;;       (("fn1" "fn2") :keyword . arg))

;; (defadvice org-schedule 
;;   (around gsn/org-prevent-rescheduling-repeated-tasks activate)
;;   "If the current task has a repeater, prevent rescheduling it to avoid obliterating the repeater."
;;   (if (org-get-repeat) 
;;       (message "*** Can't reschedule this task without obliterating repeater ***")
;;       ad-do-it))

;; (defadvice org-deadline 
;;   (around gsn/org-prevent-rescheduling-repeated-deadlines activate)
;;   "If the current task has a repeater, prevent rescheduling it to avoid obliterating the repeater."
;;   (if (org-get-repeat) 
;;       (message "*** Can't reschedule this deadline without obliterating repeater ***")
;;       ad-do-it))

(defadvice org-schedule 
  (around gsn/org-prevent-rescheduling-repeated-tasks activate)
  "If the current task has a repeater, prevent rescheduling it to avoid obliterating the repeater."
  ad-do-it)

(defadvice org-deadline 
  (around gsn/org-prevent-rescheduling-repeated-deadlines activate)
  "If the current task has a repeater, prevent rescheduling it to avoid obliterating the repeater."
  ad-do-it)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Life -- mail, bbdb, and planner
      
;; BBDB
;; (autoload 'bbdb-insinuate-mew      "bbdb-mew"   "Hook BBDB into Mew")
;; (add-hook 'mew-init-hook 'bbdb-insinuate-mew)
(setq bbdb-default-country nil
      bbdb/mail-auto-create-p nil
;;       bbdb-send-mail-style 'mew
      bbdb-use-pop-up nil)

;; Address book
(defun gsn/address-book ()
  (let ((bbdb-print-omit-fields 
         (append bbdb-print-omit-fields ; defaults
                 '(creation-date timestamp) ; normal omits
                 '(interactions next-contact 2005-02-16 2005-02-17)))
        (bbdb-print-require 'address))
    (if (assoc 'n-phones bbdb-print-full-alist)
        (setcdr (assoc 'n-phones bbdb-print-full-alist) 0)
      (add-to-list 'bbdb-print-full-alist '(n-phones . 0)))     
    (bbdb-print nil "~/Documents/bbdb-addresses.tex" nil)))
  
(defun gsn/contact-book ()
  (let ((bbdb-print-require t))
    (if (assoc 'n-phones bbdb-print-full-alist)
        (setcdr (assoc 'n-phones bbdb-print-full-alist) 10))
    (if (assoc 'n-addresses bbdb-print-full-alist)
        (setcdr (assoc 'n-addresses bbdb-print-full-alist) 10))
    (bbdb-print nil "~/Documents/bbdb-contacts.tex" nil)))
  
;; Phone book
(defun gsn/phone-book ()
  (let ((bbdb-print-require 'phone))
    (setcdr (assoc 'omit-area-code bbdb-print-alist ) "^(831)")
    (setcdr (assoc 'n-phones bbdb-print-brief-alist) 3)
    (setcdr (assoc 'n-addresses bbdb-print-brief-alist) 0)
    (bbdb-print nil "~/Documents/bbdb-phone.tex" t)))

;; Planner/Muse
;; (when (locate-library "planner")
;;   (load "~/.planner"))

;; Document Processing
(request 'tex-site)
(setq TeX-PDF-mode t
      LaTeX-table-label "tab-"
      LaTeX-equation-label "eq-"
      LaTeX-eqnarray-label "eq-"
      LaTeX-figure-label "fig-"
      LaTeX-section-label   '(("chapter" . "ch-")
                              ("section" . "sec-")
                              ("subsection" . "sec-")))

;;; HTML stuff
;; Don't use this for editing... just steal the timestamp functions
;; (request 'html-helper-mode)
;; (setq gsn/html-timestamps nil)
    
;; (add-hook 'html-mode-hook   
;;           (lambda ()
;;             (when gsn/html-timestamps
;;               (add-hook 'local-write-file-hooks 
;;                         'html-helper-update-timestamp))))

;; Jabber
(defun gsn/jabber-settings (server)
  (interactive (list (completing-read "Server: " '("google" "jabber") 
                                      nil t nil nil "google" nil)))
  (cond ((equal server "google") (setq jabber-nickname "greg.novak"
                                       jabber-username "greg.novak"
                                       jabber-server "gmail.com"
                                       jabber-network-server "talk.google.com"
                                       jabber-connection-type 'ssl))
        (equal server "jabber") (setq jabber-nickname "Greg Novak"
                                      jabber-username "gnovak"
                                      jabber-server "jabber.org"
                                      jabber-network-server nil
                                      jabber-connection-type 'ssl)))

(defun gsn/jabber-choose-buffer ()
  (interactive)
  (let ((buffers (filter (lambda (x) (string-match "*-jabber-chat" x)) 
                         (mapcar 'buffer-name (buffer-list)))))
    (cond ((= (length buffers) 0) (message "No chat buffers found"))
          ((= (length buffers) 1) (switch-to-buffer (car buffers)))
          (t (switch-to-buffer (completing-read 
                                "Chat: " buffers
                                nil t "*-jabber-chat-"))))))

(request-and-init jabber
   (gsn/jabber-settings "google"))

;; Tramp
(setq tramp-default-method "scp"
      tramp-debug-buffer t
      tramp-verbose 10)

;; (defadvice tramp-wait-for-output (after gsn/tramp-unecho activate)
;;    "Westhost doesn't have stty installed, so Tramp can't turn off
;;     echoing, so Tramp gets confused when it finds the commands it
;;     sent rather than their output.  This is a hack to kill the
;;     first line of the output buffer for hosts that look like
;;     they're my westhost account."
;;    (if (any 
;;         (mapcar (lambda (hostname)
;;                   (string-match (concat "^\\*tramp/[A-z]* " hostname "\\*$")
;;                                 (buffer-name)))   
;;                 '("web" "jenandgreg@jenandgreg.org")))
;;        (delete-region (line-beginning-position) (+ 1 (line-end-position)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; URL Browsing
(defun gsn/browse-url-firefox-on-linux (url &optional new-window)
  (shell-command 
   (concat "/usr/lib/mozilla/mozilla-xremote-client \"openurl("
           url
           ",new-tab)\"")))

(defun gsn/browse-url-safari-on-osx (url &optional new-window)
  (shell-command (concat "/Users/novak/bin/url-open safari " url)))

(defun gsn/browse-url-firefox-on-osx (url &optional new-window)
  (shell-command (concat "/Users/novak/bin/url-open firefox " url)))

(defun gsn/browse-url-remotely (url &optional new-window)
  (shell-command (concat "echo " url "| socket localhost 2081")))

(defun gsn/browse-url (&rest args)
  (cond ((and dionysus-flag remote-flag)
         (apply 'gsn/browse-url-remotely args))
        (dionysus-flag
         (apply 'gsn/browse-url-firefox-on-linux args))
        (euterpe-flag 
         (apply 'gsn/browse-url-safari-on-osx args))
        (t (error "Don't know what to do!"))))

;; Disable this for tnow to see if default URL browsing has evolved.
;; (setq browse-url-browser-function 'gsn/browse-url)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Programming
;; for compilation, put the cursor at the end of the buffer
(setq compilation-scroll-output t
      compile-command "make -k -j 2 ")

(defadvice find-tag-other-window 
  (after gsn/find-tag-other-window-stay-this-window activate compile)
  "Leave point in the current window when you call find-tag-other-window"
  (other-window 1))

;; Python
(setq ipython-command (cond ((or clio-flag thalia-flag)
                             "/opt/local/bin/ipython-2.7")
                            (pleiades-flag 
                             "/home/novak/bin/local/bin/ipython"))
      py-python-command-args '("-pylab" "-colors" "LightBG"))

(request 'python-mode)
(request 'ipython)

; this is a hack to fix the fact that the space went away from the
; ipython debugger prompt in version 0.7.3 of ipython.  Sheesh.
;; (when (or (and euterpe-flag (string= ipython-command "/sw/bin/ipython2.5")))
;;   (setq py-pdbtrack-input-prompt "\n[(<]*[Ii]?[Pp]db[>)]+ ?"))

(defadvice py-fill-paragraph 
  (around gsn/backup-at-end-of-string activate)
  "If the previous char is a quote and the current char isn't,
  back up by one character so that py-fill-paragraph doesn't fill
  my docstrings so strangely."
  (if (and (equal (preceding-char) (string-to-char "\""))
           (not (equal (following-char) (string-to-char "\""))))
      (save-excursion 
        (forward-char -1)
        ad-do-it)
    ad-do-it))

;; (when euterpe-flag
;;   (add-hook 'py-shell-hook 
;;             '(lambda () 
;;               (setenv "DYLD_LIBRARY_PATH" 
;;                "/Applications/rsi/idl_6.1/bin/bin.darwin.ppc")
;;               (setenv "XPPATH" "/Applications/rsi/idl_6.1/resource/xprinter"))
;;             t))
 
(when (locate-library "lush")
  (load "lush"))

;; Slime and remote servers: swank only listens on the loopback
;; interface, so writing stuff via 'socket 127.0.0.1 4005' works but
;; 'socket my-ip 4005' doesn't work.  For ssh forwarding, you have to
;; _forward to_ 127.0.0.1 _and_ write to 127.0.0.1 (the loopback on
;; both machines) to get slime to work.  Anything else doesn't work!

;; (font-lock-add-keywords
;;  'slime-mode
;;  '("mvb"))

;; Slime init code recommended by macports
;; 
;; (add-hook 'lisp-mode-hook
;;            (lambda ()
;;              (cond ((not (featurep 'slime))
;;                     (require 'slime) 
;;                     (normal-mode)))))

(setq slime-lisp-implementations `((sbcl ("/opt/local/bin/sbcl"))
                                   (clisp ("/opt/local/bin/clisp")))
      common-lisp-hyperspec-root "file:///opt/local/share/doc/lisp/HyperSpec-7-0/HyperSpec/")
      ; lisp-indent-function 'common-lisp-indent-function)
      ; slime-startup-animation nil
      ; inferior-lisp-program "openmcl" ; sbcl, openmcl, clisp, 
      ; slime-net-coding-system 'utf-8-unix ;; TEMP
      ; slime-complete-symbol-function 'slime-fuzzy-complete-symbol)

(request-and-init slime
  (slime-setup '(slime-fancy slime-banner)))

(when (and (= emacs-major-version 22) 
	   (fboundp 'slime-create-filename-translator))
  (setq slime-filename-translations (list (slime-create-filename-translator 
                                           :machine-instance "zuggerific.local"
                                           :remote-host "jen"
                                           :username "")
                                          (list ".*" 'identity 'identity))))

(defadvice slime-fuzzy-completions (around gsn/slime-prevent-long-completions
                                           activate compile)
  "Prevent slime from trying to complete the empty string, which
takes forever"
  (unless (string= prefix "")
    ad-do-it))

(defadvice slime-repl-previous-input (around gsn/slime-normal-up activate)
  (if (>= (point) slime-repl-input-start-mark)
      ad-do-it
    (previous-line 1)))

(defadvice slime-repl-next-input (around gsn/slime-normal-down activate)
  (if (>= (point) slime-repl-input-start-mark)
      ad-do-it
    (next-line 1)))

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

;; Maxima
(request 'maxima)

;; Random stuff
(defun gsn/maxima-untabify-output (s)
  (with-temp-buffer
    (insert s)
    (untabify (point-min) (point-max))
    (buffer-string)))

(defun gsn/maxima-add-untabify-output-filter-function ()
  (add-hook 'comint-preoutput-filter-functions 
            'gsn/maxima-untabify-output
            ;; append and make buffer-local
            t t))

(add-hook 'inferior-maxima-mode-hook 
          'gsn/maxima-add-untabify-output-filter-function)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
        (sldb-eval-in-frame sexp))))

;; (defun gsn/pgg-decrypt ()
;;   (interactive)
;;   (save-excursion
;;     (goto-char (point-min))
;;     (set-mark-command nil)
;;     (goto-char (point-max))
;;     (call-interactively 'pgg-decrypt-region)))

;; (defun gsn/pgg-encrypt ()
;;   (interactive
;;    (list (split-string (read-string "Recipients: ") "[ \t,]+")))
;;   (pgg-encrypt-region (point-min) (point-max)))

;; (fset pgg-decrypt gsn/pgg-decrypt)
;; (fset pgg-encrypt gsn/pgg-encrypt)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Code for keymaps

(defun gsn/py-windows ()
  (interactive)
  (delete-other-windows)
  (split-window-vertically)
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
  (end-of-buffer))

;;; Eshell keymaps 
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
    (previous-line 1)))

(defadvice eshell-next-matching-input-from-input 
  (around gsn/normal-down activate)
  (if (gsn/eshell-after-prompt-p)      
      ad-do-it
    (next-line 1)))

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

;; View a file in the other window, but stay in this window
(defun gsn/dired-find-file-other-window-stay-in-this-window ()
  (interactive) 
  (dired-find-file-other-window)
  (other-window 1))
  
;;; Comint key maps
;;; If the cursor is after the command prompt, make <up> and <down> do
;;; command history matching rather than just sequentially pulling
;;; commands from the history.  If the cursor is before the command
;;; prompt, make <up> and <down> do the normal cursor-movement stuff.
;;; Note that this means that you'll have to hit <left arrow> to get
;;; the cursor off of the command line so that <up> and <down> start
;;; moving the cursor instead of fooling around with the command history.
(defun gsn/comint-history-up () 
  (interactive)
  (if (not (comint-after-pmark-p))
      ;; if we're before the prompt, move around
      (previous-line 1) 
    ;; if we're after the prompt, do history matching
    (comint-previous-matching-input-from-input 1)
    ;; fool comint into thinking that the last command was 
    ;; comint-... b/c of the way it remembers the search string
    (setq this-command 'comint-previous-matching-input-from-input)))

(defun gsn/comint-history-down () 
  (interactive)
  (if (not (comint-after-pmark-p))
      (next-line 1)      
    (comint-next-matching-input-from-input 1)   
    (setq this-command 'comint-next-matching-input-from-input)))

(defun gsn/comint-history-keymaps () 
  (local-set-key [up] 'gsn/comint-history-up)
  (local-set-key "\C-p" 'gsn/comint-history-up)
  (local-set-key [down] 'gsn/comint-history-down)
  (local-set-key "\C-n" 'gsn/comint-history-down))

;;; Maxima keymaps 
;; In Maxima mode, put the fricking semicolon on the line automatically
(defun gsn/inferior-maxima-check-and-send-line ()
  "Stick a semicolon on the end of the line if there isn't one there"
  (interactive)
  (end-of-line)
  ;; check to see if last character is not a semicolon
  (message (prin1-to-string (preceding-char)))
  (if (not (eq (preceding-char) 59))
      (insert ";"))
  (inferior-maxima-check-and-send-line))

(defun gsn/maxima-keymaps ()
  (interactive)
  (gsn/comint-history-keymaps)
  (local-set-key "\C-m"  'gsn/inferior-maxima-check-and-send-line))

(defun gsn/python-mode-repl ()
  (interactive) 
  (py-shell) 
  (end-of-buffer))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Key maps
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Use global-set-key command and then repeat-complex-command
;; (global-set-key (kbd "<S-tab>") 'tetris)
;; [C-tab] and [S-tab]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; NONSTANDARD GLOBAL MAPS
;;; I can't freaking stand the ctrl-t command being transpose
;;; Make it "To-line" instead
(global-set-key "\C-t" 'goto-line)

;;; For my money, \C-f and \C-b are better mapped as forward/back word
(global-set-key "\C-f" 'forward-word)
(global-set-key "\C-b" 'backward-word)

;;; When I look up tags, I do it for documentation.  I like the info
;;; to appear in the other window and have the point stay in this
;;; window.  Achieve this by advising find-tag-other-window to stay in
;;; the current buffer, and swapping the conventional bindings for
;;; M-. and C-x 4 .
(global-set-key "\C-x4." 'find-tag)
(global-set-key "\M-." 'find-tag-other-window)

(when (= emacs-major-version 22)
  (global-set-key "\C-x\C-b" 'ibuffer))

(global-set-key (kbd "<M-up>") 'gsn/scroll-window-backward-line)
(global-set-key (kbd "<M-down>") 'gsn/scroll-window-forward-line)
;;(global-set-key (kbd "<M-left>") 'gsn/scroll-window-backward-paragraph)
;;(global-set-key (kbd "<M-right>") 'gsn/scroll-window-forward-paragraph)

;;; USER MAPS
;; compile macros
(global-set-key "\C-cc" 'compile)
(global-set-key "\C-cC" 'tramp-compile)

;; Point stack key bindings
(global-set-key "\C-cm" 'point-stack-push)
(global-set-key "\C-cb" 'point-stack-pop)

;; Org mode
(defun gsn/org-plan (arg) 
  (interactive "P") 
  (if arg
      (find-file-other-window "~/Dropbox/Brain/Desk.org")
      (find-file "~/Dropbox/Brain/Desk.org")))

(defun gsn/org-agenda ()
  (interactive)  
  (if (member "*Org Agenda*" (mapcar 'buffer-name (buffer-list)))
    (switch-to-buffer "*Org Agenda*")
    (call-interactively 'org-agenda)))

;; (defadvice org-agenda 
;;   (around gsn/org-agenda-use-existing-buffer activate)
;;   "If an *Org Agenda* buffer already exists, switch to it.  Otherwise"
;;   (if (and (equal (preceding-char) (string-to-char "\""))
;;            (not (equal (following-char) (string-to-char "\""))))
;;       (save-excursion 
;;         (forward-char -1)
;;         ad-do-it)
;;     ad-do-it))

(global-set-key "\C-ca" 'gsn/org-agenda)
(global-set-key "\C-cs" 'org-agenda)
(global-set-key "\C-cn" 'gsn/org-what-am-i-working-on) ;; mneumonic is "now"
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cp" 'gsn/org-plan)

;; Planner mode
;; (global-set-key "\C-cn" 'planner-goto-today)
(global-set-key "\C-ct" 'gsn/planner-create-task)
(global-set-key "\C-cu" 'gsn/planner-create-undated-task)
; (global-set-key "\C-cl" 'gsn/planner-annotation)
; (global-set-key "\C-cr" 'org-remember)
(global-set-key "\C-cr" 'org-capture)
(global-set-key "\C-cz" 'gsn/py-windows)
(global-set-key "\C-cr" 'org-remember)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Local Maps
;; (define-key calendar-mode-map "\C-m" 'planner-calendar-goto)
; (define-key emacs-lisp-mode-map "\C-<tab>" 'lisp-complete-symbol)

(add-hook 'eshell-mode-hook 
          (lambda () 
            (local-set-key "\C-a" 'eshell-bol)
            (local-set-key "\C-p" 'eshell-previous-matching-input-from-input)
            (local-set-key "\C-n" 'eshell-next-matching-input-from-input)))

(add-hook 'dired-mode 
          (lambda () 
            (local-set-key "v" 'gsn/dired-find-file-other-window-stay-in-this-window)))

(add-hook 'org-mode-hook
          (lambda ()
            (define-key org-mode-map "\C-cw" 'gsn/org-work-on-this) 
            (define-key org-mode-map "\C-n" 'org-next-link)
            (define-key org-mode-map "\C-p" 'org-previous-link)))

; Facilitate insertion of timestamps into HTML files
; Mneumonic here is "when"
;; (add-hook 'html-mode-hook 
;;           (lambda () 
;;             (local-set-key "\C-c\C-w" 
;;                            'html-helper-insert-timestamp-delimiter-at-point)))

(add-hook 'planner-mode-hook
          (lambda () 
            (local-set-key "\C-c\C-r" 'planner-replan-task)))

(add-hook 'org-agenda-mode-hook
          (lambda () 
            (org-defkey org-agenda-mode-map (kbd "<right>") 'forward-char)
            (org-defkey org-agenda-mode-map (kbd "<left>") 'backward-char)
            (org-defkey org-agenda-mode-map (kbd "<C-S-right>") 'org-agenda-later)
            (org-defkey org-agenda-mode-map (kbd "<C-S-left>") 'org-agenda-earlier)
            (org-defkey org-agenda-keymap (kbd "<right>") 'forward-char)
            (org-defkey org-agenda-keymap (kbd "<left>") 'backward-char)
            (org-defkey org-agenda-keymap (kbd "<C-S-right>") 'org-agenda-later)
            (org-defkey org-agenda-keymap (kbd "<C-S-left>") 'org-agenda-earlier)))

(add-hook 'comint-mode-hook 'gsn/comint-history-keymaps t)

(add-hook 'py-shell-hook 'gsn/comint-history-keymaps)
(add-hook 'python-mode-hook 
          (lambda () 
            (local-set-key "\C-c\C-z" 'gsn/python-mode-repl)))

(add-hook 'idlwave-shell-mode-hook 'gsn/comint-history-keymaps t)

(add-hook 'inferior-maxima-mode-hook 'gsn/maxima-keymaps t)
(add-hook 'imaxima-startup-hook 'gsn/maxima-keymaps t)

(add-hook 'inferior-lisp-mode-hook 'gsn/comint-history-keymaps t)
(add-hook 'ielm-mode-hook 'gsn/comint-history-keymaps t)

(add-hook 'slime-mode-hook   
          (lambda ()
         (slime-define-key "\C-cs" 'slime-selector)
            (slime-define-key "\M-n" 'gsn/slime-next-note)
            (slime-define-key "\M-p" 'gsn/slime-previous-note)
            (slime-define-key (kbd "<C-tab>") 'slime-complete-symbol)))

(add-hook 'slime-repl-mode-hook   
          (lambda () 
         (slime-define-key "\C-cs" 'slime-selector)
            (local-set-key "\C-p" 'slime-repl-previous-input)
            (local-set-key "\C-n" 'slime-repl-next-input)))

;(add-hook 'bbdb-mode-hook (lambda () 
;                           (local-set-key "I" 'bbdb-add-interaction)
;                           (local-set-key "f" 'bbdb)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Stuff I'm not sure works
;;; Set up imenu so I have a menu of function/section/whatever names
;;(defun gsn/imenu-enable ()
;;  (setq imenu-sort-function 'imenu--sort-by-name)
;;  (imenu-add-to-menubar "Imenu"))
;;(add-hook 'c-mode-hook 'gsn/imenu-enable t)
;;(add-hook 'LaTeX-mode-hook 'gsn/imenu-enable t)

;; Colors
;; (set-face-foreground 'modeline          "navy")
;; (set-face-background 'modeline          "lightblue1")

;; (define-minor-mode gpg-mode
;;   "Toggle GPG mode"
;;   nil ;; Initial
;;   "GPG"
;;   '(("\C-c)" . pgg-encrypt)
;;     ("\C-c(" . pgg-decrypt)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Emacs/automatic stuff
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(TeX-output-view-style (quote (("^dvi$" ("^landscape$" "^pstricks$\\|^pst-\\|^psfrag$") "%(o?)dvips -t landscape %d -o && gv %f") ("^dvi$" "^pstricks$\\|^pst-\\|^psfrag$" "%(o?)dvips %d -o && gv %f") ("^dvi$" ("^a4\\(?:dutch\\|paper\\|wide\\)\\|sem-a4$" "^landscape$") "%(o?)xdvi %dS -paper a4r -s 0 %d") ("^dvi$" "^a4\\(?:dutch\\|paper\\|wide\\)\\|sem-a4$" "%(o?)xdvi %dS -paper a4 %d") ("^dvi$" ("^a5\\(?:comb\\|paper\\)$" "^landscape$") "%(o?)xdvi %dS -paper a5r -s 0 %d") ("^dvi$" "^a5\\(?:comb\\|paper\\)$" "%(o?)xdvi %dS -paper a5 %d") ("^dvi$" "^b5paper$" "%(o?)xdvi %dS -paper b5 %d") ("^dvi$" "^letterpaper$" "%(o?)xdvi %dS -paper us %d") ("^dvi$" "^legalpaper$" "%(o?)xdvi %dS -paper legal %d") ("^dvi$" "^executivepaper$" "%(o?)xdvi %dS -paper 7.25x10.5in %d") ("^dvi$" "." "%(o?)xdvi %dS %d") ("^pdf$" "." "open %o ") ("^html?$" "." "netscape %o"))))
 '(auto-compression-mode t nil (jka-compr))
 '(auto-image-file-mode t)
 '(browse-url-netscape-program "mozilla")
 '(canlock-password "76c482cedbccca505ba78b5407836db33e0dc7d0")
 '(current-language-environment "Latin-1")
 '(default-input-method "latin-1-prefix")
 '(fast-lock-cache-directories (quote ("~/.font-lock" "~/.emacs-flc")))
 '(font-lock-support-mode nil)
 '(global-font-lock-mode t nil (font-lock))
 '(imenu-sort-function (quote imenu--sort-by-name))
 '(ispell-dictionary-alist (quote ((nil "[A-Za-z]" "[^A-Za-z]" "[']" nil ("-B") nil iso-8859-1) ("american" "[A-Za-z]" "[^A-Za-z]" "[']" nil ("-B") nil iso-8859-1) ("english" "[A-Za-z]" "[^A-Za-z]" "[']" nil ("-B") nil iso-8859-1))) t)
 '(jabber-connection-ssl-program nil)
 '(org-agenda-files (quote ("~/Dropbox/Brain")))
 '(org-modules (quote (org-bbdb org-bibtex org-docview org-gnus org-info org-jsinfo org-habit org-irc org-mew org-mhe org-protocol org-rmail org-vm org-wl org-w3m org-eshell org-mac-link-grabber org-screen)))
 '(require-final-newline nil)
 '(safe-local-variable-values (quote ((package . net\.aserve))))
 '(text-mode-hook (quote (turn-on-auto-fill text-mode-hook-identify)))
 '(transient-mark-mode t)
 '(uniquify-buffer-name-style nil nil (uniquify))
 '(vc-cvs-stay-local nil))

(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(jabber-chat-prompt-foreign ((t nil)))
 '(jabber-chat-prompt-local ((t nil))))

;; (add-hook 'org-mode-hook 
;;           (lambda () 
;;             (fset 'org-align-tags-here 'gsn/org-align-tags-here)))

;; Think this is fixed in most recent org-mode
;; (defun gsn/org-align-tags-here (to-col)
;;   ;; Assumes that this is a headline
;;   (let ((pos (point)) (col (current-column)) ncol tags-l p)
;;     (beginning-of-line 1)
;;     (if (and (looking-at (org-re ".*?\\([ \t]+\\)\\(:[[:alnum:]_@:]+:\\)[ \t]*$"))
;;              (< pos (match-beginning 2)))
;;         (progn
;;           (setq tags-l (- (match-end 2) (match-beginning 2)))
;;           (goto-char (match-beginning 1))
;;           (insert " ")
;;           (delete-region (point) (1+ (match-beginning 2)))
;;           (setq ncol (max (1+ (current-column))
;;                           (1+ col)
;;                           (if (> to-col 0)
;;                               to-col
;;                             (- (abs to-col) tags-l))))
;;           (setq p (point))
;;           (insert (make-string (- ncol (current-column)) ?\ ))
;;           (setq ncol (current-column))
;;           (when indent-tabs-mode
;;             (tabify p (point-at-eol)))
;;           (org-move-to-column (min ncol col) t))
;;         (goto-char pos))))
 
(put 'downcase-region 'disabled nil)

