(setq user-emacs-directory (expand-file-name (file-name-directory load-file-name)))

(setq custom-file (concat user-emacs-directory "emacs-custom-settings.el"))
(unless (file-exists-p custom-file)
  (with-temp-buffer (write-file custom-file)))
(load custom-file)

(defun visit-init-file ()
  "Visit init.el."
  (interactive)
  (find-file user-init-file))

(let ((default-directory (concat user-emacs-directory "site-lisp")))
  (push default-directory load-path)
  (normal-top-level-add-subdirs-to-load-path))

;; macOS で pakcage-refresh-contents が失敗するのを直す．
;; 出典: https://emacs.stackexchange.com/a/68568
(when (and (equal emacs-version "27.2")
           (eql system-type 'darwin))
  (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("gnu" . "https://elpa.gnu.org/packages/")))
(package-refresh-contents (boundp 'package-selected-packages))

(unless (package-installed-p 'leaf) (package-install 'leaf))
(leaf leaf-tree :ensure t)

(leaf util
  :ensure dash s
  :require dash s)

(leaf envvar
  :config
  (leaf exec-path-from-shell :unless (eq system-type 'windows-nt)
    :ensure t
    :config
    (exec-path-from-shell-initialize))
  (setenv "LANG" "ja_JP.utf-8"))

(defun shell-command-output (command)
  "COMMAND を実行し、その標準出力を文字列として返す。"
  (with-temp-buffer
    (shell-command command (current-buffer))
    (buffer-string)))
(defconst unix? (memq system-type '(gnu/linux darwin)))
(defconst macos? (eq system-type 'darwin))
(defconst wsl? (and unix? (s-contains-p "WSL2" (shell-command-output "uname -a"))))

;; 主に macOS で円記号をバックスラッシュに読み替える．
;; 出典: https://tsuu32.hatenablog.com/entry/2019/08/27/122459
(define-key local-function-key-map (kbd "¥") (kbd "\\"))
(define-key local-function-key-map (kbd "C-¥") (kbd "C-\\"))
(define-key local-function-key-map (kbd "M-¥") (kbd "M-\\"))
(define-key local-function-key-map (kbd "C-M-¥") (kbd "C-M-\\"))

(defcustom +dropbox-root (substitute-env-vars "$HOME/Dropbox")
  "Dropbox sync root directory.")
(defun +dropbox-root (path) (s-lex-format "${+dropbox-root}/${path}"))

(leaf convenience
  :custom
  (confirm-kill-emacs . #'yes-or-no-p))

(defmacro add-hooks (fn &rest hooks) "Add FN to each of HOOKS."
          `(let ((fn ,fn))
             (dolist (hook ',hooks)
               (add-hook hook fn))))

(leaf display-line-numbers-mode
  :hook prog-mode-hook org-mode-hook LaTeX-mode-hook)

(defvar delete-trailing-whitespace-modes
  (list 'prog-mode 'tex-mode)
  "保存前に `delete-trailing-whitespace' を呼び出すモードのリスト")
(add-hook 'before-save-hook
          (lambda ()
            (when (apply #'derived-mode-p delete-trailing-whitespace-modes)
              (delete-trailing-whitespace))))

(leaf font
  :config
  (defconst source-code-pro "Source Han Code JP-13")
  (defcustom default-font-name source-code-pro "Default font name.")
  (defun set-font (name)
    (if-let ((spec (font-spec :name name))
	     (font (find-font spec)))
        (progn (set-frame-font name nil t)
	       (setf (alist-get 'font default-frame-alist) name))
      (message "Font not found: %s" name)))
  (set-font default-font-name))


(leaf info
  :config
  (setq Info-directory-list (-union Info-directory-list Info-default-directory-list))
  (push (substitute-env-vars "$XDG_DATA_HOME/info") Info-directory-list))

(leaf electric-pair-local-mode :hook prog-mode-hook)
(leaf show-paren-mode :hook prog-mode-hook)
(leaf hs-minor-mode :hook prog-mode-hook)

(leaf recentf
  :custom (recentf-max-saved-items . 1000)
  :config (recentf-mode +1))

(leaf tool-bar-mode :config (tool-bar-mode -1))

(leaf undo-tree
  :ensure t
  :config (global-undo-tree-mode +1))

(leaf ivy
  :ensure t swiper counsel
  :bind (("C-s" . swiper))
  :config
  (ivy-mode +1)
  (counsel-mode +1))

(leaf winum
  :ensure t
  :config
  (winum-mode +1)
  (dolist (digit (number-sequence 0 9))
    (define-key global-map (kbd (format "M-%d" digit))
      (intern (format "winum-select-window-%d" digit)))))

(leaf which-key
  :ensure t
  :config (which-key-mode +1))

(leaf git
  :ensure magit git-gutter+
  :config (global-git-gutter+-mode +1))

(defun buffer-list-where (pred)
  "List of all live buffers where PRED holds."
  (-filter (lambda (buf) (with-current-buffer buf (funcall pred))) (buffer-list)))

(defun buffer-list-major-mode (major)
  "List of all live buffers that are in major mode MAJOR."
  (buffer-list-where (lambda () (eq major major-mode))))

(defun buffer-list-minor-mode (minor)
  "List of all live buffers where minor mode MINOR is enabled."
  (buffer-list-where (lambda () (and (boundp minor) (symbol-value minor)))))

(defmacro in-all-buffers-where (pred &rest body)
  "Do BODY in all buffers where PRED evaluates to t."
  `(dolist (buf (buffer-list))
     (with-current-buffer buf
       (when (funcall ,pred) ,@body))))

(defun git-gutter+-refresh-all-buffers ()
  (interactive)
  (in-all-buffers-where (-const git-gutter+-mode) (git-gutter+-refresh)))

(leaf rg :ensure t)

(leaf yasnippet
  :ensure t
  :config (yas-global-mode +1))

(leaf fish-mode :when unix? :ensure t)

(set-language-environment "Japanese")
(prefer-coding-system 'utf-8)
;; Ubuntu  -- apt install cmigemo
;; Windows -- https://www.kaoriya.net/software/cmigemo/
;; M-x customize-variable migemo-dictionary
(leaf migemo :ensure t)
(require 'swiper-migemo)
(add-to-list 'swiper-migemo-enable-command 'counsel-recentf)
(add-to-list 'swiper-migemo-enable-command 'counsel-rg)
(setq migemo-options '("--quiet" "--nonewline" "--emacs"))
(global-swiper-migemo-mode +1)

(leaf input-method
  :require agda-input
  :config
  (unless wsl? (setq default-input-method "Agda"))
  ;; WSL で Emacs を使っているとき、入力メソッドの切換は次のような状態遷移系にする。
  ;; states     : NoIM, Agda, Mozc
  ;; inputs     : C-\, <C-henkan>, <C-muhenkan>
  ;; transitions:
  ;; |      | C-\   | <C-henkan> | <c-muhenkan> |
  ;; |------+-------+------------+--------------|
  ;; | NoIM | Agda  | Mozc       | NoIM         |
  ;; | Agda | NoIME | Mozc       | NoIM         |
  ;; | Mozc | Agda  | Mozc       | NoIM         |
  (leaf mozc :when wsl?
    :ensure t
    :config
    (setq mozc-candidate-style 'echo-area)
    (defun im-mozc-on () (interactive) (set-input-method "japanese-mozc"))
    (defun im-agda-on () (interactive) (set-input-method "Agda"))
    (defun im-off () (interactive) (set-input-method nil))
    (defun im-off? () (null current-input-method))
    (defun im-mozc? () (equal current-input-method "japanese-mozc"))
    (defun im-agda? () (equal current-input-method "Agda"))
    (defun im-C-backslash ()
      (interactive)
      (cond ((im-agda?) (im-off))
            (t (im-agda-on))))
    (defun im-C-henkan () (interactive) (im-mozc-on))
    (defun im-C-muhenkan () (interactive) (im-off))
    :bind
    ((:global-map
      ("C-\\" . im-C-backslash)
      ("<C-henkan>" . im-C-henkan)
      ("<C-muhenkan>" . im-C-muhenkan))
     (:mozc-mode-map
      ("C-\\" . im-C-backslash)))))

;; ターミナルエミュレータの起動
(defcustom terminal-emulator "gnome-terminal" "Terminal enulator.")
(defun open-shell-here ()
  "visit 中のファイルが存在するディレクトリでターミナルを開く。"
  (interactive)
  (if-let ((dir (path-directory buffer-file-name)))
      (open-shell dir)
    (error "バッファがファイルに関連付けられていない？")))
(defun open-shell (dir)
  "DIR でシェルを開く。"
  (just-run-shell-command (s-concat terminal-emulator " " dir)))
(defun just-run-shell-command (command)
  "出力のキャプチャや通信を一切せずに COMMAND を実行する。"
  (call-process-shell-command command))
(defun path-directory (path)
  "PATH のディレクトリを返す。

PATH がファイルを指すなら、それが存在しているディレクトリ。
PATH がディレクトリを指すなら、PATH 自身。
それ以外なら NIL."
  (cond ((file-regular-p path) (file-name-directory path))
        ((file-directory-p path) path)))

(leaf lsp
  :ensure lsp-mode lsp-ui
  :config
  (leaf lsp-metals
    :config
    (when (executable-find "metals-emacs")
      (setq lsp-metals-server-command "metals-emacs"))))

(defun graphical? () (null (eq (framep (selected-frame)) t)))

(leaf tex
  :ensure auctex magic-latex-buffer
  :custom
  (magic-latex-enable-block-align . nil)
  (magic-latex-enable-inline-image . nil)
  (LaTeX-electric-left-right-brace . t)
  :hook
  (LaTeX-mode-hook . (lambda () (when (graphical?) (magic-latex-buffer +1))))
  (LaTeX-mode-hook . (lambda () (reftex-mode +1)))
  :config
  (setq TeX-command-list (-remove (lambda (l) (string-equal (car l) "LaTeX")) TeX-command-list))
  (push '("LaTeX" "latexmk --synctex=1 %T" TeX-run-command nil t) TeX-command-list)
  (leaf reftex
    :ensure t
    :custom
    (reftex-label-alist .
     '(("definition" ?d "def:" "~\\ref{%s}" t ("definition" "def.") -3)
       ("lemma" ?m "lem:" "~\\ref{%s}" t ("lemma" "lem.") -3)
       ("theorem" ?h "thm:" "~\\ref{%s}" t   ("theorem" "th.") -3)))
    (reftex-default-bibliography . `(,(+dropbox-root "lab/bib/ref.bib"))) ; TODO
    )
  (leaf bibtex
    :ensure t
    :custom
    (bibtex-files . '(bibtex-file-path)))
  )

(leaf org
  :custom
  (org-adapt-indentation . nil)
  (org-agenda-files . `(,(+dropbox-root "org")))
  (org-agenda-span . 'month)
  (org-export-use-babel . nil)
  (org-export-with-broken-link . t)
  :config
  (defvar org-indent-mode-on-automatically t)
  :bind
  (:org-mode-map
   ("M-p" . org-move-subtree-up)
   ("M-n" . org-move-subtree-down))
  :hook
  (org-mode-hook . (lambda () (when org-indent-mode-on-automatically (org-indent-mode +1))))
  )

(leaf dockerfile-mode :ensure t)

(defun count-chars-buffer (count?)
  (save-excursion
    (goto-char (point-min))
    (let ((chars 0))
      (while (char-after)
        (forward-char 1)
        (when-let* ((c (char-after))
                    (_ (funcall count? c)))
          (setq chars (1+ chars))))
      chars)))
(defun count-chars ()
  (interactive)
  (save-restriction
    (narrow-to-region (region-beginning) (region-end))
    (let ((chars
           (count-chars-buffer (lambda (c) (null (or (eq c ?\n) (eq c ? ) (eq c ?　)))))))
      (message (int-to-string chars)))
    ))

(defun revisit-with-sudo ()
  "Revisit the file of selected buffer with root priviledge."
  (interactive)
  (find-file (s-concat "/sudo::" buffer-file-name)))

(advice-add 'bookmark-all-names :filter-return (lambda (names) (sort names #'string<)))

(defun replace-japanese-punctuations-interactively ()
  "すべての句読点をカンマとピリオドに置き換える。"
  (interactive)
  (goto-char 0)
  (query-replace "、" "，")
  (goto-char 0)
  (query-replace "。" "．"))

(defun reload-all-dired-buffers ()
  (interactive)
  (let ((dired-buffers (buffer-list-major-mode 'dired-mode))
        (reload (lambda (buf) (with-current-buffer buf (revert-buffer)))))
    (mapc reload dired-buffers)))

(setq ring-bell-function 'ignore)

(defconst leader-map (make-sparse-keymap) "My keymap.")
(defun set-leader-map (key def &rest bindings)
  "Add KEY and DEF to my keymap."
  (while key
    (define-key leader-map (kbd key) def)
    (setq key (pop bindings) def (pop bindings))))

(defcustom leader-key
  (cond ((eq system-type 'gnu/linux) "<henkan>")
        ;; Karabiner-Elements を使って Caps Lock を <help> にリマップして使う．
        ((eq system-type 'darwin) "<help>")
        ((eq system-type 'windows-nt) "<convert>"))
  "自分で好きに使えるプレフィックスキー。")
(setq lsp-keymap-prefix (concat leader-key " l"))
(global-set-key (kbd leader-key) leader-map)
(define-key global-map (kbd "C-h") 'backward-delete-char-untabify)
(define-key global-map (kbd "<f5>") 'revert-buffer)
(define-key global-map (kbd "C-x C-c") #'save-buffers-kill-emacs)
(set-leader-map
 ;; shell
 "!" #'open-shell-here
 ;; visiting [f]ile
 "fr" #'counsel-recentf
 "fi" #'visit-init-file
 "fs" #'revisit-with-sudo
 ;; [g]it
 "gb" #'magit-blame
 "gr" #'git-gutter+-refresh-all-buffers
 "gn" #'git-gutter+-next-hunk
 "gp" #'git-gutter+-previous-hunk
 "gs" #'git-gutter+-show-hunk-inline-at-point
 ;; [s]earch
 "sg" #'counsel-git-grep
 "sr" #'counsel-rg
 ;; [p]roject-aware commands
 "pf" #'project-find-file
 ;; [j]ump
 "jf" #'find-function
 "jl" #'find-library
 ;; [l]sp
 "l" #'lsp
 ;; [o]rg mode
 "oa" #'org-agenda)

(leaf server :unless (server-running-p)
  :config
  (server-start))

;; Local Variables:
;; indent-tabs-mode: nil
;; End:
