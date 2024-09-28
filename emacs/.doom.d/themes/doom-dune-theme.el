;;; doom-dune-theme.el
(require 'doom-themes)

;;
(defgroup doom-dune-theme nil
  "Options for doom-themes"
  :group 'doom-themes)

(defcustom doom-dune-brighter-modeline nil
  "If non-nil, more vivid colors will be used to style the mode-line."
  :group 'doom-dune-theme
  :type 'boolean)

(defcustom doom-dune-brighter-comments nil
  "If non-nil, comments will be highlighted in more vivid colors."
  :group 'doom-dune-theme
  :type 'boolean)

(defcustom doom-dune-comment-bg doom-dune-brighter-comments
  "If non-nil, comments will have a subtle, darker background. Enhancing their
legibility."
  :group 'doom-dune-theme
  :type 'boolean)

(defcustom doom-dune-padded-modeline nil
  "If non-nil, adds a 4px padding to the mode-line. Can be an integer to
determine the exact padding."
  :group 'doom-dune-theme
  :type '(or integer boolean))

;;
(def-doom-theme doom-dune
  "Doom dune theme"

  ;; name        default   256       16
  ((bg         '("#090909" nil       nil            ))
   (bg-alt     '("#222222" nil       nil            ))
   (base0      '("#15100C" "#121212" "black"        ))
   (base1      '("#2B2017" "#1C1C1C" "brightblack"  ))
   (base2      '("#403123" "#303030" "brightblack"  ))
   (base3      '("#56412F" "#444444" "brightblack"  ))
   (base4      '("#6B513A" "#4E4E4E" "brightblack"  ))
   (base5      '("#816146" "#875F5F" "brightblack"  ))
   (base6      '("#967252" "#875F5F" "brightblack"  ))
   (base7      '("#A98260" "#AF875F" "brightblack"  ))
   (base8      '("#B49376" "#AF8787" "white"        ))
   (fg-alt     '("#CDB6A2" "#D7AFAF" "brightwhite"  ))
   (fg         '("#C0A48B" "#AFAF87" "white"        ))

   (grey       base4)
   (red        '("#D17B49" "#D7875F" "red"          ))
   (orange     '("#C08052" "#AF875F" "brightred"    ))
   (green      '("#636C3A" "#585858" "green"        ))
   (teal       '("#7B8748" "#87875F" "brightgreen"  ))
   (yellow     '("#AF865A" "#AF875F" "yellow"       ))
   (blue       '("#535C5C" "#585858" "brightblue"   ))
   (dark-blue  '("#424A4A" "#444444" "blue"         ))
   (magenta    '("#5F4647" "#4E4E4E" "magenta"      ))
   (violet     '("#775759" "#875F5F" "brightmagenta"))
   (cyan       '("#6D715E" "#6C6C6C" "brightcyan"   ))
   (dark-cyan  '("#575A4B" "#585858" "cyan"         ))

   ;; face categories -- required for all themes
   (highlight      blue)
   (vertical-bar   (doom-lighten bg 0.05))
   (selection      dark-blue)
   (builtin        blue)
   (comments       (if doom-dune-brighter-comments dark-cyan base5))
   (doc-comments   (doom-lighten (if doom-dune-brighter-comments dark-cyan base5) 0.25))
   (constants      red)
   (functions      yellow)
   (keywords       blue)
   (methods        cyan)
   (operators      blue)
   (type           yellow)
   (strings        teal)
   (variables      cyan)
   (numbers        magenta)
   (region         dark-blue)
   (error          red)
   (warning        yellow)
   (success        green)
   (vc-modified    orange)
   (vc-added       green)
   (vc-deleted     red)

   ;; custom categories
   (hidden     `(,(car bg) "black" "black"))
   (-modeline-bright doom-dune-brighter-modeline)
   (-modeline-pad
    (when doom-dune-padded-modeline
      (if (integerp doom-dune-padded-modeline) doom-dune-padded-modeline 4)))

   (modeline-fg     nil)
   (modeline-fg-alt base5)

   (modeline-bg
    (if -modeline-bright
        base3
        `(,(doom-darken (car bg) 0.15) ,@(cdr base0))))
   (modeline-bg-l
    (if -modeline-bright
        base3
        `(,(doom-darken (car bg) 0.1) ,@(cdr base0))))
   (modeline-bg-inactive   (doom-darken bg 0.1))
   (modeline-bg-inactive-l `(,(car bg) ,@(cdr base1))))


  ;; --- extra faces ------------------------
  ((elscreen-tab-other-screen-face :background "#353a42" :foreground "#1e2022")

   ((line-number &override) :foreground fg-alt)
   ((line-number-current-line &override) :foreground fg)
   ((line-number &override) :background (doom-darken bg 0.025))

   (font-lock-comment-face
    :foreground comments
    :background (if doom-dune-comment-bg (doom-lighten bg 0.05)))
   (font-lock-doc-face
    :inherit 'font-lock-comment-face
    :foreground doc-comments)

   (doom-modeline-bar :background (if -modeline-bright modeline-bg highlight))

   (mode-line
    :background modeline-bg :foreground modeline-fg
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg)))
   (mode-line-inactive
    :background modeline-bg-inactive :foreground modeline-fg-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive)))
   (mode-line-emphasis
    :foreground (if -modeline-bright base8 highlight))
   (mode-line-buffer-id
    :foreground highlight)

   (solaire-mode-line-face
    :inherit 'mode-line
    :background modeline-bg-l
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-l)))
   (solaire-mode-line-inactive-face
    :inherit 'mode-line-inactive
    :background modeline-bg-inactive-l
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive-l)))

   (telephone-line-accent-active
    :inherit 'mode-line
    :background (doom-lighten bg 0.2))
   (telephone-line-accent-inactive
    :inherit 'mode-line
    :background (doom-lighten bg 0.05))
   (telephone-line-evil-emacs
    :inherit 'mode-line
    :background dark-blue)

   ;; --- major-mode faces -------------------
   ;; css-mode / scss-mode
   (css-proprietary-property :foreground orange)
   (css-property             :foreground green)
   (css-selector             :foreground blue)

   ;; markdown-mode
   (markdown-markup-face :foreground base5)
   (markdown-header-face :inherit 'bold :foreground red)
   (markdown-code-face :background (doom-lighten base3 0.05))

   ;; org-mode
   (org-hide :foreground hidden)
   (org-block :background base2)
   (org-block-begin-line :background base2 :foreground comments)
   (solaire-org-hide-face :foreground hidden))


  ;; --- extra variables ---------------------
  ;; ()
  )

;;; doom-dune-theme.el ends here