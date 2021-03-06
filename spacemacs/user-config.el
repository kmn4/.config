(defun dotspacemacs/user-config ()
  "Configuration for user code:
This function is called at the very end of Spacemacs startup, after layer
configuration.
Put your configuration code here, except for variables that should be set
before packages are loaded."
() ;; 改行時のよけいなインデントを防止

;; variables
(defcustom +dropbox-root (substitute-env-vars "$HOME/Dropbox")
  "Dropbox sync root directory.")
(defun +dropbox-root (path) (s-lex-format "${+dropbox-root}/${path}"))
(defconst dotspacemacs-private-directory (concat dotspacemacs-directory "lisp/"))
;; load-path
(let ((default-directory (concat dotspacemacs-directory "lisp/")))
  (push default-directory load-path)
  (normal-top-level-add-subdirs-to-load-path))

;; functions
(defun +sorted-dotspacemacs-configuration-layers ()
  "Return a copy of dotspacemacs-configuration-layers sorted by lexicological order."
  (labels ((cmp (fst snd)
                (cond ((symbolp fst)
                       (cond
                        ((symbolp snd) (string< fst snd))
                        (t (cmp fst (car snd)))))
                      (t (cmp (car fst) snd)))))
    (let ((seq (copy-tree dotspacemacs-configuration-layers)))
      (prin1-to-string
       (sort seq #'cmp)))))
(defun +disable-annoying-things ()
  "Disable `lsp-ui-mode' and `flycheck-mode'."
  (interactive)
  (lsp-ui-mode -1)
  (flycheck-mode -1))
(defun +enable-debug-assistants ()
  "Enable `lsp-ui-mode' and `flycheck-mode'."
  (interactive)
  (lsp-ui-mode +1)
  (flycheck-mode +1))

(defun +balance-windows ()
  "Close treemacs, balance windows, and reopen treemacs
if it was opened when called."
  (interactive)
  (let* ((treemacs-local-window (treemacs-get-local-window))
         (initial-window (selected-window)))
    (when treemacs-local-window
      (delete-window treemacs-local-window)
      (run-hooks 'treemacs-quit-hook))
    (balance-windows)
    (when treemacs-local-window
      (treemacs-select-window)
      (select-window initial-window))))

;; TODO ライブラリにありそう
(defun +get-string-from-file (file-path)
  "Return FILE-PATH's file content."
  (with-temp-buffer
    (insert-file-contents file-path)
    (buffer-string)))
(defmacro add-hooks (fn &rest hooks) "Add FN to each of HOOKS."
          `(let ((fn ,fn))
             (dolist (hook ',hooks)
               (add-hook hook fn))))

(defvar define-hook-command-history nil
  "History for commands by `define-hook-command'.")
(defmacro define-hook-command (name hook-name docstring &rest action)
  "Define interactive command NAME whose arglist is nil and body is ACTION.
Use `ivy-read' to read a hook which is to be bound to HOOK-NAME."
  `(defun ,name () ,docstring
          (interactive)
          (ivy-read "Choose hook: " obarray
                    :predicate (-andfn #'counsel--variable-p
                                       (lambda (s) (s-ends-with? "-hook" (symbol-name s))))
                    :require-match t
                    :history 'define-hook-command-history
                    :keymap counsel-describe-map
                    :preselect (cadr define-hook-command-history)
                    :action (lambda (x)
                              (let ((,hook-name (intern x)))
                                ,@action))
                    ;; :caller 'counsel-describe-variable
                    ))
  )

;; Key Bindings
(progn (defvar +spacemacs-user-map (make-sparse-keymap))
       (spacemacs/set-leader-keys "o" +spacemacs-user-map)
       (define-key global-map (kbd "<henkan>") +spacemacs-user-map)
       (defun +spacemacs/set-user-map (key def &rest bindings)
         "Add KEY and DEF as key bindings under
`dotspacemacs-leader-key' and `dotspacemacs-emacs-leader-key'
followed by \"o\"."
         (while key
           (define-key +spacemacs-user-map (kbd key) def)
           (setq key (pop bindings) def (pop bindings))))
       (with-eval-after-load 'vterm
         (define-key vterm-mode-map (kbd "C-h") 'vterm-send-backspace))
       (bind-keys :map global-map
                  ("C-h" . nil)
                  ("C-h" . delete-backward-char)
                  ("<f5>" . revert-buffer))
       (defun +open-org-default-note-file ()
         (interactive)
         (find-file org-default-notes-file))
       (+spacemacs/set-user-map
        "g" #'find-grep
        "pu" #'sp-unwrap-sexp
        "pr" #'sp-rewrap-sexp
        "on" #'+open-org-default-note-file)
       (bind-keys :map key-translation-map
                  ("S-TAB" . "<backtab>"))
       (+spacemacs/set-user-map "jl" #'ace-link)
       (with-eval-after-load 'evil
         (evil-define-minor-mode-key 'motion 'visual-line-mode "j" 'evil-next-visual-line)
         (evil-define-minor-mode-key 'motion 'visual-line-mode "k" 'evil-previous-visual-line)
         (evil-define-minor-mode-key 'motion 'visual-line-mode (kbd "<down>") 'evil-next-visual-line)
         (evil-define-minor-mode-key 'motion 'visual-line-mode (kbd "<up>") 'evil-previous-visual-line)))

;; Dired, Treemacs
(setq-default dired-omit-mode t)
(with-eval-after-load 'treemacs
  (defun +treemacs-ignore-latex-predicate (file _)
    (some (lambda (suf) (string-suffix-p suf file))
          '(".aux"
            "synctex.gz"
            "fls"
            "fdb_latexmk"
            ".bbl")))
  (push #'+treemacs-ignore-latex-predicate treemacs-ignored-file-predicates))

;; Hooks
(progn (add-hooks (lambda () (display-line-numbers-mode +1))
                  prog-mode-hook
                  org-mode-hook
                  LaTeX-mode-hook
                  shell-mode-hook)
       (add-hooks (lambda () (hs-minor-mode +1))
                  prog-mode-hook
                  LaTeX-mode-hook)
       (add-hooks (lambda ()
                    (when (derived-mode-p 'prog-mode)
                      (delete-trailing-whitespace)))
                  before-save-hook)
       (add-hooks (lambda () (clean-aindent-mode -1)) go-mode-hook))

;; Fish
(with-eval-after-load 'fish-mode
  (advice-add 'fish_indent :after #'recenter)
  (add-hook 'before-save-hook
            (lambda () (when (eq major-mode 'fish-mode) (fish_indent))))
  )

;; bookmark
;; counsel-bookmark に名前でソートさせたい
(advice-add 'bookmark-all-names :filter-return (lambda (names) (sort names #'string<)))

;; LSP
(with-eval-after-load 'lsp-mode
  ;; lsp-ui-doc-show-with-mouse makes Emacs slow.
  (setq lsp-eldoc-enable-hover t
        lsp-ui-doc-enable t
        lsp-ui-doc-show-with-cursor nil
        lsp-ui-doc-show-with-mouse t
        lsp-signature-auto-activate nil)
  (spacemacs/set-leader-keys-for-minor-mode 'lsp-mode
    "hg" #'lsp-ui-doc-glance
    "hs" #'lsp-ui-doc-show
    "hq" #'lsp-ui-doc-hide)
  ;; パンくずリストを有効化
  (setq lsp-headerline-breadcrumb-enable t
        lsp-headerline-breadcrumb-segments '(symbols)
        lsp-headerline-breadcrumb-enable-symbol-numbers nil)
  ;; Spacemacs が各メジャーモードのフックに lsp を追加する
  ;; lsp サーバの起動は遅いことがあり，少しの変更をするだけなら起動してほしくない
  (define-hook-command remove-lsp-from-hook hook
    "Remove `lsp' and `dap-mode' from HOOK."
    (remove-hook hook #'lsp)
    (remove-hook hook #'dap-mode))
  (define-hook-command add-lsp-to-hook hook
    "Add `lsp' and `dap-mode' to HOOK."
    (add-hook hook #'dap-mode)
    (add-hook hook #'lsp))
)
(with-eval-after-load 'imenu
  (setq imenu-auto-rescan t))

;; Org
(with-eval-after-load 'org
  (setq org-directory (+dropbox-root "org")
        org-default-notes-file (expand-file-name "notes.org" org-directory)
        org-agenda-files (list (+dropbox-root "org"))
        org-agenda-span 'month)
  (add-hooks (lambda () (visual-line-mode +1)) org-mode-hook)

  (spacemacs/set-leader-keys-for-major-mode 'org-mode
    "s." #'spacemacs/org-subtree-transient-state/body)

  (setq org-babel-python-command (or (executable-find "python") "python3")))

;; Scala
(with-eval-after-load 'scala-mode
  (add-hooks (lambda () (when (and (eq major-mode 'scala-mode) (boundp 'lsp-mode) lsp-mode)
                          (lsp-format-buffer)))
             before-save-hook))
(with-eval-after-load 'lsp-metals
  (setq lsp-metals-treeview-show-when-views-received nil
        lsp-metals-show-implicit-arguments nil
        lsp-metals-show-inferred-type nil))

;; Python
(use-package python
  :defer t
  :init
  (use-package blacken
    :defer t
    :ensure t)
  (use-package py-isort
    :defer t
    :ensure t)
  :config
  (add-hook 'python-mode-hook #'blacken-mode)
  (add-hook 'python-mode-hook #'py-isort-before-save))

;; Rust
(with-eval-after-load 'rust-mode
  (setq rust-format-on-save t))

(with-eval-after-load 'info
  (setq Info-directory-list (-union Info-directory-list Info-default-directory-list))
  (push (substitute-env-vars "$XDG_DATA_HOME/info") Info-directory-list) ; TODO
  )

;; Japanese support (migemo)
(progn (set-language-environment "Japanese")
       (prefer-coding-system 'utf-8)
       (use-package migemo
         :defer t
         :init
         (setq migemo-dictionary (cond (unix? "/usr/share/cmigemo/utf-8/migemo-dict"))) ; TODO
         )
       (use-package avy-migemo
         :after counsel
         :config
         (require 'avy-migemo-e.g.ivy)
         (require 'avy-migemo-e.g.counsel)
         (require 'avy-migemo-e.g.swiper)
         (avy-migemo-mode +1))
       )
;; textlint
(with-eval-after-load 'flycheck
  (flycheck-define-checker textlint
    "A linter for Japanese writing."
    :command ("textlint" "--format" "unix" source)
    :error-patterns
    ((warning line-start (file-name) ":" line ":" column ": "
              (id (one-or-more (not (any " "))))
              (message (one-or-more not-newline)
                       (zero-or-more "\n" (any " ") (one-or-more not-newline)))
              line-end))
    :modes (text-mode org-mode markdown-mode gfm-mode)))

;; Use Agda input method
(use-package agda-input :config (setq default-input-method "Agda"))

;; TODO
(defun add-all-to-list (list-var elements &optional append compare-fn)
  "Call `add-to-list' on each element in ELEMENTS."
  (mapc (lambda (elt) (add-to-list list-var elt append compare-fn)) elements))
(with-eval-after-load 'magit
  (add-all-to-list 'magit-repository-directories
                   (list (cons dotspacemacs-directory 0)
                         (cons spacemacs-start-directory 0))))

;; Frame size
(defun +set-frame-size (width height)
  "Set default-frame-width/height to WIDTH/HEIGHT."
  (setf (alist-get 'width default-frame-alist) width)
  (setf (alist-get 'height default-frame-alist) height)
  (setf (alist-get 'width initial-frame-alist) width)
  (setf (alist-get 'height initial-frame-alist) height))
(+set-frame-size 120 36)
(defun push-to-frame-alists (elt)
  "Push ELT to `default-frame-alist' and `initial-frame-alist'."
  (push elt default-frame-alist)
  (push elt initial-frame-alist))
(push-to-frame-alists '(undecorated . t))

;; Company
(with-eval-after-load 'company
  (setq company-idle-delay nil          ; no idle completion
        company-selection-wrap-around t)
  (bind-keys :map company-mode-map
             ("S-SPC" . company-complete)
             ("C-c y" . company-yasnippet))
  (bind-keys :map company-active-map
             ("C-h" . delete-backward-char)
             ([return] . company-complete-selection)
             ("RET" . company-complete-selection)))

(use-package open-junk-file
  :defer t
  :commands (open-junk-file counsel-find-junk counsel-junk-grep)
  :init
  (+spacemacs/set-user-map
   "jn" #'open-junk-file
   "jf" #'counsel-find-junk
   "jg" #'counsel-junk-grep)
  :config
  (require 'counsel-projectile)
  (defvar +junk-root (+dropbox-root "junk/") "Root directory for junk files.")
  (setq open-junk-file-format (s-lex-format "${+junk-root}/%Y/%m/%Y-%m-%d-%H%M%S."))
  (defun counsel-find-junk ()
    (interactive)
    (let ((projectile-project-root +junk-root))
      (counsel-projectile-find-file)))
  (defun counsel-junk-grep ()
    (interactive)
    (let ((projectile-project-root +junk-root))
      (counsel-projectile-grep))))

;; LaTeX
(with-eval-after-load 'tex
  (setf (alist-get 'output-pdf TeX-view-program-selection)
        (cond (kde? '("Okular"))))
  (setq reftex-default-bibliography `(,(+dropbox-root "lab/bib/ref.bib")) ; TODO
        bibtex-files '(bibtex-file-path))
  (spacemacs/set-leader-keys-for-major-mode 'latex-mode
    "gl" #'reftex-goto-label)
  (setq reftex-label-alist
        '(("definition" ?d "def:" "~\\ref{%s}" t ("definition" "def.") -3)
          ("lemma" ?m "lem:" "~\\ref{%s}" t ("lemma" "lem.") -3)
          ("theorem" ?h "thm:" "~\\ref{%s}" t   ("theorem" "th.") -3)))
  (progn
    (setq TeX-command-list (-remove (lambda (l) (string-equal (car l) "LatexMk")) TeX-command-list))
    (push '("LatexMk" "latexmk --synctex=1 %T" TeX-run-command nil t) TeX-command-list))
  )

(use-package pdf-sync
  :defer t
  :config
  (define-key LaTeX-mode-map (kbd "<C-return>") 'pdf-sync-forward-search)
  (define-key pdf-view-mode-map (kbd "<C-mouse-1>") 'pdf-sync-backward-search-mouse))

;; Flycheck
(with-eval-after-load 'flycheck
  (setq-default flycheck-disabled-checkers '(tex-lacheck tex-chktex)))

(push '("\\.gpi\\'" . gnuplot-mode) auto-mode-alist)
(push '("\\.smt2\\'" . lisp-mode) auto-mode-alist)

(setq find-function-C-source-directory
      (concat (getenv "XDG_DATA_HOME") "/emacs/emacs-27.1/src")) ; TODO

(menu-bar-mode +1)
(spacemacs/toggle-debug-on-error-on)
(setf confirm-kill-emacs 'yes-or-no-p)

;; edit-server
(use-package edit-server
  :init
  (edit-server-start))

;; Mail (mu4e)
(setq mu4e-update-interval 60)
(with-eval-after-load 'mu4e-alert
  ;; Enable Desktop notifications
  (mu4e-alert-set-default-style 'notifications)) ; For Linux.
)  ; dotspacemacs/user-config
