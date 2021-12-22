(setq user-emacs-directory (expand-file-name (file-name-directory load-file-name)))

;; init.el は GitHub を使って複数の環境から取得・更新するので、
;; 公開したくない情報や環境特有の設定はカスタマイズ変数にして別ファイルに追い出す。
(setq custom-file (locate-user-emacs-file "emacs-custom-settings.el"))
(load custom-file)

;;;; load-path と leaf.

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
;; leaf
(if (package-installed-p 'leaf) (package-refresh-contents t) ;; 非同期
  (package-refresh-contents)
  (package-install 'leaf))
(leaf leaf-tree :ensure t)

;;;; キーマップ。
;; init.el に問題があってすべてを読み込めないときでも
;; ある程度バインドされていたほうが便利なので初めに設定してしまう。

;; 主に macOS で円記号をバックスラッシュに読み替える．
;; 出典: https://tsuu32.hatenablog.com/entry/2019/08/27/122459
(define-key local-function-key-map (kbd "¥") (kbd "\\"))
(define-key local-function-key-map (kbd "C-¥") (kbd "C-\\"))
(define-key local-function-key-map (kbd "M-¥") (kbd "M-\\"))
(define-key local-function-key-map (kbd "C-M-¥") (kbd "C-M-\\"))

;; 独自のキーマップ。Spacemacs のスペースキーにバインドされたマップみたいに使う。
(defconst leader-map (make-sparse-keymap) "My keymap.")

;; leaf の :bind で leader-map を設定しようして
;;   (leaf NAME :bind (:leader-map BIND...))
;; のように書くと、leaf は leader-map が NAME というパッケージに定義されていると想定して
;; 展開する。しかし実際には独自のキーマップであるため、期待と異なる挙動を示す。
;; そのため、leader-map の設定には次の関数を使うことにする。
(defun set-leader-map (key def &rest bindings)
  "Add KEY and DEF to my keymap."
  (while key
    (define-key leader-map (kbd key) def)
    (setq key (pop bindings) def (pop bindings))))

;; leader-map をバインドするキー。
;; キーボードや OS によって異なるキーを使いたいのでカスタマイズ変数とする。
(defcustom leader-key
  (cond ((eq system-type 'gnu/linux) "<henkan>")
        ;; Karabiner-Elements を使って Caps Lock を <help> にリマップして使う．
        ((eq system-type 'darwin) "<help>")
        ((eq system-type 'windows-nt) "<convert>"))
  "自分で好きに使えるプレフィックスキー。")
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

(leaf util
  :ensure dash s
  :require dash s)

;;;; Emacs や基本的なライブラリにしか依存しない関数など

(defun visit-init-file ()
  "Visit init.el."
  (interactive)
  (find-file user-init-file))

(defun revisit-with-sudo ()
  "Revisit the file of selected buffer with root priviledge."
  (interactive)
  (find-file (s-concat "/sudo::" buffer-file-name)))

(defun replace-in-buffer (from to)
  (save-excursion
    (goto-char 0)
    (while (search-forward from nil t)
      (replace-match to))))

;; 論文とかでは句読点をカンマとピリオドに統一したい。IME 設定は面倒なので勝手にやってほしい。
;; M-x add-file-local-variable RET replace-japanese-punctuations-on-save RET t RET

(defun replace-japanese-punctuations ()
  (replace-in-buffer "、" "，")
  (replace-in-buffer "。" "．"))

(defvar-local replace-japanese-punctuations-on-save nil)

(add-hook 'before-save-hook
          (lambda () (when replace-japanese-punctuations-on-save
                       (replace-japanese-punctuations))))

;; 引用があるときは機械的に置き換えるとまずいのでいちいち判断する。
;; それなら IME で設定したほうが早そうだけど…
(defun replace-japanese-punctuations-interactively ()
  "すべての句読点をカンマとピリオドに置き換える。"
  (interactive)
  (goto-char 0)
  (query-replace "、" "，")
  (goto-char 0)
  (query-replace "。" "．"))

;; シェルとターミナル

(defcustom terminal-emulator "gnome-terminal" "Terminal enulator.")

(defun open-shell-here ()
  "visit 中のファイルが存在するディレクトリでターミナルを開く。"
  (interactive)
  (open-shell default-directory))

(defun open-shell (dir)
  "DIR でシェルを開く。"
  (just-run-shell-command (s-concat terminal-emulator " " dir)))

(defun just-run-shell-command (command)
  "出力のキャプチャや通信を一切せずに COMMAND を実行する。"
  (call-process-shell-command command nil 0))

(defun shell-command-output (command)
  "COMMAND を実行し、その標準出力を文字列として返す。"
  (with-temp-buffer
    (shell-command command (current-buffer))
    (buffer-string)))

;; バッファリスト

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

(defun reload-all-dired-buffers ()
  (interactive)
  (let ((dired-buffers (buffer-list-major-mode 'dired-mode))
        (reload (lambda (buf) (with-current-buffer buf (revert-buffer)))))
    (mapc reload dired-buffers)))

;; 環境判定系

(defconst unix? (memq system-type '(gnu/linux darwin)))
(defconst macos? (eq system-type 'darwin))
(defconst wsl? (and unix? (s-contains-p "WSL2" (shell-command-output "uname -a"))))

(defun graphical? () (null (eq (framep (selected-frame)) t)))

(leaf envvar
  :config
  (leaf exec-path-from-shell :unless (eq system-type 'windows-nt)
    :ensure t
    :config
    (exec-path-from-shell-initialize))
  (setenv "LANG" "ja_JP.utf-8"))

(defcustom +dropbox-root (substitute-env-vars "$HOME/Dropbox")
  "Dropbox sync root directory.")
(defun +dropbox-root (path) (s-lex-format "${+dropbox-root}/${path}"))

(leaf savehist :config (savehist-mode +1))

(leaf convenience
  :custom
  (confirm-kill-emacs . #'yes-or-no-p))

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

(leaf winner :config (winner-mode +1))

(leaf which-key
  :ensure t
  :config (which-key-mode +1))

(leaf git
  :config
  (leaf magit :ensure t)
  (leaf git-gutter+ :ensure t :config (global-git-gutter+-mode +1))
  (leaf git-modes :ensure t))

(defun git-gutter+-refresh-all-buffers ()
  (interactive)
  (in-all-buffers-where (-const git-gutter+-mode) (git-gutter+-refresh)))

(leaf rg :ensure t)

(leaf dockerfile-mode :ensure t)

;; counsel-bookmark を名前順で表示したい。
(advice-add 'bookmark-all-names :filter-return (lambda (names) (sort names #'string<)))

(setq ring-bell-function 'ignore)

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
(leaf swiper-migemo
  :require t
  :config
  (add-to-list 'swiper-migemo-enable-command 'counsel-recentf)
  (add-to-list 'swiper-migemo-enable-command 'counsel-rg)
  (setq migemo-options '("--quiet" "--nonewline" "--emacs"))
  (global-swiper-migemo-mode +1))

;; 入力メソッド
;; |      | C-\   | <C-henkan> | <c-muhenkan> |
;; |------+-------+------------+--------------|
;; | NoIM | Agda  | Mozc       | NoIM         |
;; | Agda | NoIME | Mozc       | NoIM         |
;; | Mozc | Agda  | Mozc       | NoIM         |
(leaf input-method
  :config
  (leaf agda-input :require t
    :custom (default-input-method . "Agda"))
  (leaf mozc :when wsl?
    :ensure t
    :custom (mozc-candidate-style . 'echo-area))
  (when wsl?
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
    (global-set-key (kbd "C-\\") #'im-C-backslash)
    (define-key mozc-mode-map (kbd "C-\\") #'im-C-backslash)
    (global-set-key (kbd "<C-henkan>") #'im-C-henkan)
    (global-set-key (kbd "<C-muhenkan>") #'im-C-muhenkan)))

(leaf lsp
  :ensure lsp-mode lsp-ui
  :custom
  `(lsp-keymap-prefix . ,(concat leader-key " l"))
  :config
  )

;; Scala

(leaf lsp-metals
  :config
  (when (executable-find "metals-emacs")
    (setq lsp-metals-server-command "metals-emacs")))

;; SMT-LIB

(leaf smtlib-mode :require t)

;; TeX

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

;; Org

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

(leaf server :unless (server-running-p)
  :config
  (server-start))

;; Local Variables:
;; indent-tabs-mode: nil
;; End:
