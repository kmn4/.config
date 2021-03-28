;; -*- mode: emacs-lisp; lexical-binding: t -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

;; This file defines the following functions (prefixed with `dotspacemacs/'):
;; - layers
;; - init
;; - user-env
;; - user-load
;; - user-init
;; - user-config
(defun dotspacemacs/layers ()
  "Layer configuration:
This function should only modify configuration layer settings."
  ;; Set the following variables (prefixed with `dotspacemacs-'):
  ;; - distribution
  ;; - enable-lazy-installation
  ;; - ask-for-lazy-installation
  ;; - configuration-layer-path
  ;; - configuration-layers
  ;; - additional-packages
  ;; - frozen-packages
  ;; - excluded-packages
  ;; - install-packages
  (setq-default
   ;; For now available distributions are `spacemacs-base'or `spacemacs'.
   dotspacemacs-distribution 'spacemacs ; layers/+distribution/spacemacs

   ;; Possible values are `used-only', `used-but-keep-unused' and `all'.
   dotspacemacs-enable-lazy-installation nil
   dotspacemacs-ask-for-lazy-installation t

   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path nil

   ;; List of configuration layers to load.
   dotspacemacs-configuration-layers
   '((auto-completion :variables
                      auto-completion-return-key-behavior 'complete
                      auto-completion-tab-key-behavior 'cycle
                      auto-completion-idle-delay 0.2
                      auto-completion-enable-help-tooltip t
                      auto-completion-enable-snippets-in-popup t
                      auto-completion-enable-sort-by-usage t
                      auto-completion-use-company-box t)
     (c-c++ :variables c-c++-enable-clang-format-on-save t)
     coq
     (elfeed :variables elfeed-enable-web-interface t)
     emacs-lisp
     emoji
     git
     (go :variables
         go-tab-width 4
         go-format-before-save t)
     graphviz
     haskell
     html
     ibuffer
     ivy
     (javascript :variables
                 javascript-fmt-on-save t
                 javascript-fmt-tool 'prettier)
     (json :variables json-fmt-on-save 'prettier
           json-backend 'company-json
           json-fmt-on-save t)
     (latex :variables
            latex-backend 'company-auctex
            latex-build-command "LatexMk"
            latex-enable-magic t
            latex-enable-folding t
            latex-enable-auto-fill nil)
     (lsp :variables lsp-ui-sideline-enable t)
     markdown
     (mu4e :variables
           mu4e-installation-path "/usr/share/emacs/site-lisp/mu4e"
           mu4e-enable-notifications t
           mu4e-enable-mode-line t)
     multiple-cursors
     nginx
     ocaml
     org
     pdf
     prettier
     react
     restclient
     rust
     scala
     (shell :variables
            multi-term-program "/bin/bash"
            shell-default-shell 'vterm
            shell-default-height 30
            shell-default-position 'bottom)
     shell-scripts
     (spacemacs-evil :variables
                     spacemacs-evil-collection-allowed-list '(info dired))
     sql
     syntax-checking
     systemd
     treemacs
     (typescript :variables
                 typescript-fmt-on-save t
                 typescript-fmt-tool 'prettier
                 typescript-linter 'eslint
                 typescript-backend 'lsp)
     version-control
     yaml)

   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   ;; To use a local version of a package, use the `:location' property:
   ;; '(your-package :location "~/path/to/your-package/")
   ;; Also include the dependencies as they will not be resolved automatically.
   dotspacemacs-additional-packages
   '(
     agda2-mode ; For agda-input-method
     migemo ; search by Japanese
     multi-vterm
     edit-server
     mu4e-alert
     dired-du
     ;; Python
     blacken
     py-isort
     )

   ;; A list of packages that cannot be updated.
   dotspacemacs-frozen-packages '()

   ;; A list of packages that will not be installed and loaded.
   dotspacemacs-excluded-packages '()

   ;; Possible values are `used-only', `used-but-keep-unused' and `all'.
   dotspacemacs-install-packages 'used-only))

(defun dotspacemacs/init ()
  "Initialization:
This function is called at the very beginning of Spacemacs startup,
before layer configuration.
It should only modify the values of Spacemacs settings."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;;; Elpa
   dotspacemacs-elpa-https t
   dotspacemacs-elpa-timeout 5          ; in second
   dotspacemacs-use-spacelpa nil
   dotspacemacs-verify-spacelpa-archives t
   dotspacemacs-elpa-subdirectory 'emacs-version ; like 'emacs/elpa/27.1/...'

   dotspacemacs-editing-style 'hybrid
   ;;; Keys
   ;; see DOCUMENTATION.org/Concepts/Editing Styles
   dotspacemacs-leader-key "SPC"
   dotspacemacs-emacs-command-key "SPC" ; M-x
   dotspacemacs-ex-command-key ":"
   dotspacemacs-major-mode-leader-key "," ; <leader> m
   dotspacemacs-emacs-leader-key "C-SPC"  ; In 'emacs state' or 'insert state'
   dotspacemacs-major-mode-emacs-leader-key "C-,"
   dotspacemacs-distinguish-gui-tab t   ; `C-i', `TAB' and `C-m', `RET'.

   ;;; Startup behaviour
   ;; If non-nil output loading progress in `*Messages*' buffer. (default nil)
   dotspacemacs-verbose-loading nil
   dotspacemacs-startup-banner 'official
   ;; Possible values for list-type are:
   ;; `recents' `bookmarks' `projects' `agenda' `todos'.
   dotspacemacs-startup-lists '((recents . 5) (projects . 7))
   dotspacemacs-startup-buffer-responsive t
   dotspacemacs-loading-progress-bar t
   dotspacemacs-fullscreen-at-startup nil
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native nil ; macOS で使うかも?
   dotspacemacs-maximized-at-startup nil
   dotspacemacs-undecorated-at-startup nil ; macOS で使うかも?

   ;;; Server
   dotspacemacs-enable-server t
   dotspacemacs-server-socket-dir nil   ; 手元の環境以外では変えたほうが良さそう
   ;; If non-nil, advise quit functions to keep server open when quitting.
   dotspacemacs-persistent-server nil

   ;;; Appearance
   dotspacemacs-themes '(spacemacs-dark spacemacs-light)
   ;; Supported themes are
   ;; `spacemacs', `all-the-icons', `custom',
   ;; `doom' (from doom-emacs), `vim-powerline' and `vanilla' (Emacs' default).
   ;; `custom' is a user defined themes. Refer to the DOCUMENTATION.org.
   ;; (default '(spacemacs :separator wave :separator-scale 1.5))
   dotspacemacs-mode-line-theme '(spacemacs :separator wave :separator-scale 1.5)
   ;; If non-nil the cursor color matches the state color in GUI Emacs.
   dotspacemacs-colorize-cursor-according-to-state t
   ;; Default font or prioritized list of fonts.
   dotspacemacs-default-font '(("源ノ角ゴシック Code JP" :size 16 :weight normal :width normal))
   dotspacemacs-active-transparency 90
   dotspacemacs-inactive-transparency 90
   ;; `t', `nil' or `display-graphic-p'
   dotspacemacs-mode-line-unicode-symbols t
   dotspacemacs-smooth-scrolling t      ; no recentering on reaching the top
   ;; Possible values are `any',`current', `all' or `nil'.
   dotspacemacs-highlight-delimiters 'all
   dotspacemacs-frame-title-format "%a @ %t" ; 'file' @ 'project'
   dotspacemacs-icon-title-format nil        ; same as frame-title-format
   ;; Run `spacemacs/prettify-org-buffer' when
   ;; visiting README.org files of Spacemacs.
   dotspacemacs-pretty-docs nil

   ;;; Performance tuning
   ;; Set `gc-cons-threshold' and `gc-cons-percentage' when startup finishes.
   dotspacemacs-gc-cons '(100000000 0.1)
   ;; https://gitter.im/emacs-lsp/lsp-mode?at=5dfe7962cf771f770809683e
   dotspacemacs-read-process-output-max (* 1024 1024 4) ; 4 MB

   ;;; Layout
   dotspacemacs-default-layout-name "Default"
   dotspacemacs-display-default-layout nil
   dotspacemacs-auto-resume-layouts nil      ; Resume with auto-saved layouts?
   dotspacemacs-auto-generate-layout-names t ; Only when "jump by number"

   ;;; Misc
   dotspacemacs-new-empty-buffer-major-mode 'text-mode
   dotspacemacs-scratch-mode 'lisp-interaction-mode
   dotspacemacs-initial-scratch-message nil

   dotspacemacs-large-file-size 1       ; prompt to open literally (MB)

   ;; Possible values are `original', `cache' and `nil'
   dotspacemacs-auto-save-file-location 'cache

   ;; Maximum number of rollback slots to keep in the cache. (default 5)
   dotspacemacs-max-rollback-slots 5

   dotspacemacs-enable-paste-transient-state nil

   dotspacemacs-which-key-delay 0.4     ; Which-key delay in seconds.
   ;; Possible values are `right', `bottom' and `right-then-bottom'.
   dotspacemacs-which-key-position 'bottom

   ;; Control where `switch-to-buffer' displays the buffer. If nil,
   ;; `switch-to-buffer' displays the buffer in the current window even if
   ;; another same-purpose window is available. If non-nil, `switch-to-buffer'
   ;; displays the buffer in a same-purpose window even if the buffer can be
   ;; displayed in the current window. (default nil)
   dotspacemacs-switch-to-buffer-prefers-purpose nil

   dotspacemacs-show-transient-state-title t
   dotspacemacs-show-transient-state-color-guide t

   dotspacemacs-check-for-update nil

   ;; Control line numbers activation.
   dotspacemacs-line-numbers '(:enabled-for-modes prog-mode text-mode conf-mode)

   ;; Possible values are `evil' and `origami'.
   dotspacemacs-folding-method 'evil

   dotspacemacs-smartparens-strict-mode nil
   dotspacemacs-smart-closing-parenthesis nil ; ")" を押したら ")" を挿入

   dotspacemacs-search-tools '("rg" "ag" "pt" "ack" "grep")

   ;; Possible values are `all', `trailing', `changed' or `nil'.
   dotspacemacs-whitespace-cleanup nil
   dotspacemacs-show-trailing-whitespace t

   dotspacemacs-zone-out-when-idle nil  ; in seconds

   ;;; portable dumper (I won't need this)
   dotspacemacs-enable-emacs-pdumper nil
   dotspacemacs-emacs-pdumper-executable-file "emacs"
   dotspacemacs-emacs-dumper-dump-file "spacemacs.pdmp"

   ))

(defun dotspacemacs/user-env ()
  "Environment variables setup.
This function defines the environment variables for your Emacs session. By
default it calls `spacemacs/load-spacemacs-env' which loads the environment
variables declared in `~/.spacemacs.env' or `~/.spacemacs.d/.spacemacs.env'.
See the header of this file for more information."
  (spacemacs/load-spacemacs-env))

(defun dotspacemacs/user-load ()
  "Library to load while dumping.
This function is called only while dumping Spacemacs configuration. You can
`require' or `load' the libraries of your choice that will be included in the
dump."
  )

(load (concat dotspacemacs-directory "user-init.el"))
(load (concat dotspacemacs-directory "user-config.el"))
