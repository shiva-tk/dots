;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
;; (setq user-full-name "John Doe"
;;       user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-symbol-font' -- for symbols
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
(setq doom-font (font-spec :family "Iosevka Nerd Font" :size 18 :weight 'semi-light))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-homage-white)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

(after! org
  (setq org-todo-keywords
        '((sequence "TODO(t/!)" "IDEA(i/!)" "PROJ(p)" "NEXT(n/!)" "WAIT(w@/!)" "|" "DONE(d/!)" "VOID(v@/!)"))))

(after! org
  (setq org-tag-alist
        '(("@home" . ?h)
          ("@uni" . ?u))))

;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

(setq
 projectile-project-search-path '("~/Documents/dev/" "~/Documents/icl/" "~/Documents/org/")
 vterm-shell "/sbin/zsh"
 +latex-viewers '(zathura)
 TeX-fold-unfold-around-mark 't
 lsp-tex-server 'texlab
 TeX-electric-sub-and-superscript 't
 )

(setq org-directory "~/Documents/org/")
(setq org-roam-directory "~/Documents/org/roam")
(setq org-agenda-files (directory-files-recursively "~/Documents/org/" "\\.org$"))
(setq org-hide-emphasis-markers 't)
(after! oc (setq org-cite-global-bibliography '("~/Documents/org/refs/bibliography.bib")))

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

(after! org
  (map! :map org-mode-map
        :n "M-j" #'org-metadown
        :n "M-k" #'org-metaup
   )
  )

;; Resize org mode headings
(custom-set-faces
  '(org-document-title ((t (:height 2.0 :weight bold))))
  '(org-level-1 ((t (:inherit outline-1 :height 1.7))))
  '(org-level-2 ((t (:inherit outline-2 :height 1.6))))
  '(org-level-3 ((t (:inherit outline-3 :height 1.5))))
  '(org-level-4 ((t (:inherit outline-4 :height 1.4))))
  '(org-level-5 ((t (:inherit outline-5 :height 1.3))))
  '(org-level-6 ((t (:inherit outline-5 :height 1.2))))
  '(org-level-7 ((t (:inherit outline-5 :height 1.1)))))

(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
(setq org-bullets-bullet-list
        '("⁖"))
(setq org-ellipsis " ▾ ")

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

(after! org (require 'org-protocol))

(setq org-capture-templates
      `(("i" "inbox" entry (file, "~/Documents/org/gtd/inbox.org")
         "* TODO %?")
        ("l" "link" entry (file, "~/Documents/org/gtd/inbox.org")
         "* TODO %? | %a")
        ("c" "org-protocol-capture" entry (file, "~/Documents/org/gtd/inbox.org")
         "* TODO [[%:link][%:description]]\n\n %i" :immediate-finish t)
        ("e" "email" entry (file, "~/Documents/org/gtd/inbox.org")
         "* TODO %? | %:fromname | %a")))

(after! org
  (setq org-log-into-drawer "LOGBOOK"))

(defun jethro/org-agenda-process-inbox-item ()
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

(defun jethro/org-process-inbox ()
  "Called in org-agenda-mode, processes all inbox items."
  (interactive)
  (custom/org-agenda-bulk-mark-regexp-category "inbox")
  (jethro/bulk-process-entries))

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

(defun jethro/bulk-process-entries ()
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
              (let (org-loop-over-headlines-in-active-region) (funcall 'jethro/org-agenda-process-inbox-item))
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

(defun jethro/set-todo-state-next ()
  "Visit each parent task and change NEXT states to TODO"
  (org-todo "NEXT"))

(defun jethro/set-todo-state-done ()
  "Visit each parent task and change NEXT states to TODO"
  (org-todo "DONE"))

(add-hook 'org-clock-in-hook 'jethro/set-todo-state-next 'append)
(add-hook 'org-clock-out-hook 'jethro/set-todo-state-done 'append)

(setq org-refile-use-outline-path 'file
      org-outline-path-complete-in-steps nil)
(setq org-refile-allow-creating-parent-nodes 'confirm)
(setq org-refile-targets '(("life.org" :level . 0)
                           ("reading.org" :level . 0)
                           ("projects.org" :level . 1)
                           ("work.org" :level . 1)))

(after! org
  (map! :map org-agenda-mode-map
        :leader
        :desc "Process an item in the inbox"
        "m R" #'jethro/org-process-inbox))

(after! org (setq org-agenda-custom-commands
      '((" " "Agenda"
        ((agenda ""
                 ((org-agenda-span 'week)
                  (org-deadline-warning-days 365)))
         (todo "TODO"
               ((org-agenda-overriding-header "To Refile")
                (org-agenda-files '("~/Documents/org/gtd/inbox.org"))))
         (todo "NEXT"
               ((org-agenda-overriding-header "In Progress")
                (org-agenda-files '("~/Documents/org/gtd/life.org"
                                    "~/Documents/org/gtd/work.org"
                                    "~/Documents/org/gtd/reading.org"
                                    "~/Documents/org/gtd/projects.org"
                                    "~/Documents/org/gtd/repeaters.org"))
                ))
         (todo "TODO"
               ((org-agenda-overriding-header "Project Tasks")
                (org-agenda-files '("~/Documents/org/gtd/projects.org"))
                ))
         (todo "TODO"
               ((org-agenda-overriding-header "Work Tasks")
                (org-agenda-files '("~/Documents/org/gtd/work.org"))))
         (todo "TODO"
               ((org-agenda-overriding-header "Life Tasks")
                (org-agenda-files '("~/Documents/org/gtd/life.org"))))
         (todo "TODO"
               ((org-agenda-overriding-header "Reading List")
                (org-agenda-files '("~/Documents/org/gtd/reading.org"))))
         (todo "WAIT"
               ((org-agenda-overriding-header "Blocked Tasks")
                (org-agenda-files '("~/Documents/org/gtd/life.org"
                                    "~/Documents/org/gtd/work.org"
                                    "~/Documents/org/gtd/reading.org"
                                    "~/Documents/org/gtd/projects.org"
                                    "~/Documents/org/gtd/repeaters.org"))
                ))
         nil))
        )))

(after! org
  (setq org-archive-location "~/Documents/org/gtd/archive.org::"
        org-log-done 'time
        org-log-reschedule 'note
        org-agenda-log-mode-items '(clock closed)
        org-agenda-start-with-log-mode t))

(after! mu4e
  (setq
   ;; General settings
   mu4e-root-maildir "~/email/gmail"
   mu4e-update-interval 300 ;; Update every 5 minutes
   mu4e-get-mail-command "mbsync -a" ;; Command to sync emails
   mu4e-change-filenames-when-moving t

   ;; Account-specific settings
   user-mail-address "anand.tk.03@gmail.com"
   user-full-name "Shiva Tamil Kumaran"
   mu4e-message-signature "- Shiva Tamil Kumaran"

   ;; Gmail-specific folders
   mu4e-sent-folder "/[Google Mail]/Sent Mail"
   mu4e-drafts-folder "/[Google Mail]/Drafts"
   mu4e-trash-folder "/[Google Mail]/Trash"
   mu4e-refile-folder "/[Google Mail]/All Mail"

   ;; Contexts for new and old email addresses
   mu4e-contexts
      `(,(make-mu4e-context
          :name "Old Gmail"
          :match-func (lambda (msg)
                        (when msg
                          (string-match-p "anand.tk.03@gmail.com"
                                          (or (plist-get (mu4e-message-field msg :to) :email) ""))))
          :vars '((user-mail-address      . "anand.tk.03@gmail.com")
                  (user-full-name         . "Shiva Tamil Kumaran")))

        ,(make-mu4e-context
          :name "New Gmail"
          :match-func (lambda (msg)
                        (when msg
                          (string-match-p "shivatk01@gmail.com"
                                          (or (plist-get (mu4e-message-field msg :to) :email) ""))))
          :vars '((user-mail-address      . "shivatk01@gmail.com")
                  (user-full-name         . "Shiva Tamil Kumaran"))))

   +mu4e-personal-addresses '("anand.tk.03@gmail.com"
                              "shivatk01@gmail.com")
   mu4e-maildir-shortcuts
      '(("/Inbox"                   . ?i)
        ("/[Google Mail]/Sent Mail" . ?e)
        ("/[Google Mail]/Starred"   . ?s)
        ("/[Google Mail]/Trash"     . ?t)
        ("/[Google Mail]/Drafts"    . ?d)
        ("/[Google Mail]/All Mail"  . ?a))))

(setq
 message-send-mail-function 'smtpmail-send-it
 smtpmail-smtp-server "smtp.gmail.com"
 smtpmail-smtp-service 587
 smtpmail-stream-type 'starttls
 smtpmail-auth-supported '(plain login)
 smtpmail-smtp-user "anand.tk.03@gmail.com")

(setq auth-sources
    '((:source "~/Documents/.authinfo.gpg")))

(after! mu4e
  (map! :map mu4e-mode-map
        :leader
        :desc "Capture email to Org"
        "m c" #'mu4e-org-store-and-capture))
