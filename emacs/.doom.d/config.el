;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Theme and Appearance

(setq doom-font (font-spec :family "Iosevka Nerd Font" :size 18 :weight 'semi-light))
(setq doom-theme 'doom-homage-white)
(setq display-line-numbers-type 'relative)


;; projectile

(setq
 projectile-project-search-path '("~/Documents/dev/" "~/Documents/icl/" "~/Documents/org/"))


;; vterm

(after! vterm (setq vterm-shell "/sbin/zsh"))


;; org

;; Configure directories
(setq org-directory "~/Documents/org/")
(setq org-agenda-files (directory-files-recursively "~/Documents/org/" "\\.org$"))
(defun custom/gtd-file (filename)
  (let ((gtd-directory (concat (file-name-as-directory org-directory) "gtd/")))
    (concat gtd-directory filename)))

;; Hide emphasis markers in *bold* or /italic/
(setq org-hide-emphasis-markers 't)

;; Resize headings
(custom-set-faces
  '(org-document-title ((t (:height 2.0 :weight bold))))
  '(org-level-1 ((t (:inherit outline-1 :height 1.7 :spacing 2))))
  '(org-level-2 ((t (:inherit outline-2 :height 1.6 :spacing 2))))
  '(org-level-3 ((t (:inherit outline-3 :height 1.5))))
  '(org-level-4 ((t (:inherit outline-4 :height 1.4))))
  '(org-level-5 ((t (:inherit outline-5 :height 1.3))))
  '(org-level-6 ((t (:inherit outline-5 :height 1.2))))
  '(org-level-7 ((t (:inherit outline-5 :height 1.1)))))

;; Evil keybinds to move headings up or down
(after! org
  (map! :map org-mode-map
        :n "M-j" #'org-metadown
        :n "M-k" #'org-metaup
   )
  )

;; Set up org-bullets to make heading slook prettier
(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
(setq org-bullets-bullet-list
        '("⁖"))

;; When a section is collapsed, this is display instead of '...'
(setq org-ellipsis " ▾ ")

;; Set up logging.
(after! org
  (setq org-log-into-drawer "LOGBOOK"))

;; Set the TODO keywords
;; /! Means timestamp when leaving this state.
;; @ means prompt for a note when entering this state.
(after! org
  (setq org-todo-keywords
        '((sequence "TODO(t/!)" "IDEA(i/!)" "NEXT(n/!)" "WAIT(w@/!)" "|" "DONE(d/!)" "VOID(v@/!)"))
        org-log-done 'time          ;; Log the time at which items are closed
        org-log-reschedule 'note))  ;; Prompt for a note when rescheduling an item

(after! org
  (setq ;; org-archive-location (custom/gtd-file "archive.org") ;; Set up archive location for tasks
        org-agenda-log-mode-items '(clock closed)            ;; Shows log information in agenda view
        org-agenda-start-with-log-mode t))                   ;; Starts the agenda with log mode enabled.

;; Set available tags for TODO items
(after! org
  (setq org-tag-alist
        '(("@home" . ?h)
          ("@uni" . ?u))))

;; Set up org capture templates
(setq org-capture-templates
      `(("i" "inbox" entry (file ,(custom/gtd-file "inbox.org"))
         "* TODO %?")
        ("l" "link" entry (file ,(custom/gtd-file "inbox.org"))
         "* TODO %? | %a")
        ("c" "org-protocol-capture" entry (file ,(custom/gtd-file "inbox.org"))
         "* TODO [[%:link][%:description]]\n\n %i" :immediate-finish t)
        ("e" "email" entry (file ,(custom/gtd-file "inbox.org"))
         "* TODO %? | %:fromname | %a")))

;; Function to process all items in the inbox, within the agenda view
;; https://blog.jethro.dev/posts/processing_inbox/
(defun custom/org-process-inbox ()
  "Called in org-agenda-mode, processes all inbox items."
  (interactive)
  (custom/org-agenda-bulk-mark-regexp-category "inbox")  ;; Bulk select all entries in the inbox
  (custom/bulk-process-entries))                         ;; Bulk process selected entries

;; Function to buk select agenda entries based on their category
(defun custom/org-agenda-bulk-mark-regexp-category (regexp)
    "Mark entries whose category matches REGEXP for future agenda bulk action."
    (interactive "sMark entries with category matching regexp: ")
    (let ((entries-marked 0) txt-at-point)
      (save-excursion
        (goto-char (point-min))
        (goto-char (next-single-property-change (point) 'org-hd-marker))
        (while (and (re-search-forward regexp nil t)
                    (setq category-at-point
                          (get-text-property (match-beginning 0) 'org-category)))
          (if (get-char-property (point) 'invisible)
              (beginning-of-line 2)
            (when (string-match-p regexp category-at-point)
              (setq entries-marked (1+ entries-marked))
              (call-interactively 'org-agenda-bulk-mark)))))
      (unless entries-marked
        (message "No entry matching this regexp."))))

;; Function to process selected inbox entries in the org agenda.
;; https://blog.jethro.dev/posts/processing_inbox/
(defun custom/bulk-process-entries ()
  (interactive)
  (if (not (null org-agenda-bulk-marked-entries))
      (let ((entries (reverse org-agenda-bulk-marked-entries))
            (processed 0)
            (skipped 0))
        (dolist (e entries)
          (let ((pos (text-property-any (point-min) (point-max) 'org-hd-marker e)))
            (if (not pos)
                (progn (message "Skipping removed entry at %s" e)
                       (cl-incf skipped))
              (goto-char pos)
              (let (org-loop-over-headlines-in-active-region) (funcall 'custom/process-single-inbox-entry))
              ;; `post-command-hook' is not run yet.  We make sure any
              ;; pending log note is processed.
              (when (or (memq 'org-add-log-note (default-value 'post-command-hook))
                        (memq 'org-add-log-note post-command-hook))
                (org-add-log-note))
              (cl-incf processed))))
        (org-agenda-redo)
        (unless org-agenda-persistent-marks (org-agenda-bulk-unmark-all))
        (message "Acted on %d entries%s%s"
                 processed
                 (if (= skipped 0)
                     ""
                   (format ", skipped %d (disappeared before their turn)"
                           skipped))
                 (if (not org-agenda-persistent-marks) "" " (kept marked)")))))

;; Process a single inbox entry at the cursor
;; https://blog.jethro.dev/posts/processing_inbox/
(defun custom/process-single-inbox-entry ()
  "Process a single item in the org-agenda."
  (interactive)
  (org-with-wide-buffer
   (org-agenda-set-tags)
   (org-agenda-priority)
   (when (y-or-n-p "Schedule this item? ")
     (call-interactively 'org-agenda-schedule))
   (when (y-or-n-p "Set a deadline for this item? ")
     (call-interactively 'org-agenda-deadline))
   (org-agenda-refile nil nil t)))

;; Set up a keybinding to process the inbox.
(after! org
  (map! :map org-agenda-mode-map
        :leader
        :desc "Process an item in the inbox"
        "m R" #'custom/org-process-inbox))

;; Automatically change states when clocking in or out
(defun custom/set-todo-state-next ()
  "Visit each parent task and change states to NEXT"
  (org-todo "NEXT"))
;; (defun custom/set-todo-state-done ()
;;   "Visit each parent task and change states to DONE"
;;   (org-todo "DONE"))
(add-hook 'org-clock-in-hook 'custom/set-todo-state-next 'append)
;; (add-hook 'org-clock-out-hook 'custom/set-todo-state-done 'append)

;; Configure refiling menue when refiling tasks
(setq org-refile-use-outline-path 'file
      org-outline-path-complete-in-steps nil)
(setq org-refile-allow-creating-parent-nodes 'confirm)  ;; Allow creation of headings under which to group tasks, when refiling them
(setq org-refile-targets '(("life.org" :level . 0)      ;; Display life.org, do not refile to headings within this file
                           ("reading.org" :level . 0)   ;; Display reading.org, do not refile to headings within this file
                           ("projects.org" :level . 1)  ;; Display projects.org, show headings
                           ("work.org" :level . 1)))    ;; Display work.org, show headings

;; Configure the custom org agenda view
(after! org (setq org-agenda-custom-commands
      '((" " "Agenda"
        ((agenda ""
                 ((org-agenda-span 'week)
                  (org-agenda-start-on-weekday nil)
                  (org-agenda-start-day "+0d")
                  (org-deadline-warning-days 365)))
         (todo "TODO"
               ((org-agenda-overriding-header "To Refile")
                (org-agenda-files (list (custom/gtd-file "inbox.org")))))
         (todo "NEXT"
               ((org-agenda-overriding-header "In Progress")
                (org-agenda-files (list
                                   (custom/gtd-file "life.org")
                                   (custom/gtd-file "work.org")
                                   (custom/gtd-file "reading.org")
                                   (custom/gtd-file "projects.org")
                                   (custom/gtd-file "repeaters.org")))
                ))
         (todo "TODO"
               ((org-agenda-overriding-header "Project Tasks")
                (org-agenda-files (list (custom/gtd-file "projects.org")))
                ))
         (todo "TODO"
               ((org-agenda-overriding-header "Work Tasks")
                (org-agenda-files (list (custom/gtd-file "work.org")))))
         (todo "TODO"
               ((org-agenda-overriding-header "Life Tasks")
                (org-agenda-files (list (custom/gtd-file "life.org")))))
         (todo "TODO"
               ((org-agenda-overriding-header "Reading List")
                (org-agenda-files (list (custom/gtd-file "reading.org")))))
         (todo "WAIT"
               ((org-agenda-overriding-header "Blocked Tasks")
                (org-agenda-files (list
                                   (custom/gtd-file "life.org")
                                   (custom/gtd-file "work.org")
                                   (custom/gtd-file "reading.org")
                                   (custom/gtd-file "projects.org")
                                   (custom/gtd-file "repeaters.org")))
                ))
         nil))
        )))

;; Set up a shortcut to open this custom agenda view
(defun custom/switch-to-agenda ()
  (interactive)
  (org-agenda nil " "))

(map! :leader
      "o A" #'custom/switch-to-agenda)


;; org-protocol
;; Allows capturing TODOs from the browser, using a bookmarklet.

(after! org (require 'org-protocol))


;; org-roam

;; Configure directory
(setq org-roam-directory "~/Documents/org/roam")


;; org-roam-ui

(use-package! websocket
    :after org-roam)

(use-package! org-roam-ui
    :after org-roam ;; or :after org
;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
;;         a hookable mode anymore, you're advised to pick something yourself
;;         if you don't care about startup time, use
;;  :hook (after-init . org-roam-ui-mode)
    :config
    (setq org-roam-ui-sync-theme t
          org-roam-ui-follow t
          org-roam-ui-update-on-save t
          org-roam-ui-open-on-start t))


;; org-cite
;; https://www.riccardopinosio.com/blog/posts/zotero_notes_article

;; Set the bibliography file location.
(after! oc (setq org-cite-global-bibliography '("~/Documents/org/refs/bibliography.bib")))


;; citar
;; https://www.riccardopinosio.com/blog/posts/zotero_notes_article

(use-package! citar
  :after oc
  :custom
  (org-cite-insert-processor 'citar)
  (org-cite-follow-processor 'citar)
  (org-cite-activate-processor 'citar)
  (citar-org-roam-note-title-template "${author} - ${title}\npdf: ${file}")
  (citar-bibliography '("~/Documents/org/refs/bibliography.bib")))

(after! citar
  (add-to-list 'citar-file-open-functions '("pdf" . citar-file-open-external)))


;; mu4e

(after! mu4e
  (setq
   ;; General settings
   mu4e-root-maildir "~/email"
   mu4e-update-interval 300 ;; Update every 5 minutes
   mu4e-get-mail-command "mbsync -a" ;; Command to sync emails
   mu4e-change-filenames-when-moving t

   ;; Account-specific settings
   user-mail-address "anand.tk.03@gmail.com"
   user-full-name "Shiva Tamil Kumaran"
   mu4e-message-signature "- Shiva Tamil Kumaran"

   ;; Gmail-specific folders
   mu4e-sent-folder "/gmail/[Google Mail]/Sent Mail"
   mu4e-drafts-folder "/gmail/[Google Mail]/Drafts"
   mu4e-trash-folder "/gmail/[Google Mail]/Trash"
   mu4e-refile-folder "/gmail/[Google Mail]/All Mail"

   ;; Contexts for new and old email addresses
   mu4e-contexts
      `(,(make-mu4e-context
          :name "Old Gmail"
          :match-func (lambda (msg)
                        (when msg
                          (and (string-prefix-p "/gmail" (mu4e-message-field msg :maildir))
                               (or (string-match-p "anand.tk.03@gmail.com"
                                                   (or (plist-get (mu4e-message-field msg :to) :email) ""))
                                   (string-match-p "anand.tk.03@gmail.com"
                                                   (or (plist-get (mu4e-message-field msg :from) :email) ""))))))
          :vars '((user-mail-address      . "anand.tk.03@gmail.com")
                  (user-full-name         . "Shiva Tamil Kumaran")
                  (mu4e-drafts-folder  . "/gmail/[Google Mail]/Drafts")
                  (mu4e-sent-folder  . "/gmail/[Google Mail]/Sent Mail")
                  (mu4e-refile-folder  . "/gmail/[Google Mail]/All Mail")
                  (mu4e-trash-folder  . "/gmail/[Google Mail]/Trash")))

        ,(make-mu4e-context
          :name "New Gmail"
          :match-func (lambda (msg)
                        (when msg
                          (and (string-prefix-p "/gmail" (mu4e-message-field msg :maildir))
                               (or (string-match-p "shivatk01@gmail.com"
                                                   (or (plist-get (mu4e-message-field msg :to) :email) ""))
                                   (string-match-p "shivatk01@gmail.com"
                                                   (or (plist-get (mu4e-message-field msg :from) :email) ""))))))
          :vars '((user-mail-address      . "shivatk01@gmail.com")
                  (user-full-name         . "Shiva Tamil Kumaran")
                  (mu4e-drafts-folder  . "/gmail/[Google Mail]/Drafts")
                  (mu4e-sent-folder  . "/gmail/[Google Mail]/Sent Mail")
                  (mu4e-refile-folder  . "/gmail/[Google Mail]/All Mail")
                  (mu4e-trash-folder  . "/gmail/[Google Mail]/Trash"))))

   ;; Set up addresses
   +mu4e-personal-addresses '("anand.tk.03@gmail.com"
                              "shivatk01@gmail.com")
   ;; Set up shortcuts
   mu4e-maildir-shortcuts
      '(("/gmail/Inbox"                   . ?i)
        ("/gmail/[Google Mail]/Sent Mail" . ?e)
        ("/gmail/[Google Mail]/Starred"   . ?s)
        ("/gmail/[Google Mail]/Trash"     . ?t)
        ("/gmail/[Google Mail]/Drafts"    . ?d)
        ("/gmail/[Google Mail]/All Mail"  . ?a))))

;; Configuration to facilitate sending emails
(setq
 message-send-mail-function 'smtpmail-send-it
 smtpmail-smtp-server "smtp.gmail.com"
 smtpmail-smtp-service 587
 smtpmail-stream-type 'starttls
 smtpmail-auth-supported '(plain login)
 smtpmail-smtp-user "anand.tk.03@gmail.com")

;; smtp auth information found here
(setq auth-sources
    '((:source "~/Documents/.authinfo.gpg")))

;; Set up a shortcut to allow capturing emails into my TODO inbox
(after! mu4e
  (map! :map mu4e-mode-map
        :leader
        :desc "Capture email to Org"
        "m c" #'mu4e-org-store-and-capture))


;; tex

(setq
 +latex-viewers '(zathura)            ;; Set the viewer in which to open previews
 TeX-fold-unfold-around-mark 't       ;; ???
 lsp-tex-server 'texlab               ;; lsp
 TeX-electric-sub-and-superscript 't  ;; Show subscript and superscript below and above the line.
 )
