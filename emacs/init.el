;;; init.el --- Emacs initialization script

;;; Commentary:

;;; Code:

;; init.el は GitHub を使って複数の環境から取得・更新するので、
;; 公開したくない情報や環境特有の設定はカスタマイズ変数にして別ファイルに追い出す。
(eval-and-compile
  (setq custom-file (locate-user-emacs-file "emacs-custom-settings.el"))
  (unless (file-exists-p custom-file) (with-temp-buffer (write-file custom-file)))
  (load custom-file))

;;;; load-path と leaf.

(eval-and-compile
  (let* ((site-lisp-directory (concat user-emacs-directory "site-lisp/"))
         (default-directory site-lisp-directory))
    (add-to-list 'load-path default-directory)
    (normal-top-level-add-subdirs-to-load-path)))

;; macOS で pakcage-refresh-contents が失敗するのを直す．
;; 出典: https://emacs.stackexchange.com/a/68568
(eval-and-compile
  (when (and (equal emacs-version "27.2")
             (eql system-type 'darwin))
    (custom-set-variables 'gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")))

;; leaf
(eval-and-compile
  (require 'package)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
  (if (package-installed-p 'leaf) (package-refresh-contents t) ;; 非同期
    (package-refresh-contents)
    (package-install 'leaf))
  (leaf leaf-keywords :ensure t diminish hydra :config (leaf-keywords-init))
  (leaf leaf-tree :ensure t :diminish t))

;;;; キーマップ。
;; init.el に問題があってすべてを読み込めないときでも
;; ある程度バインドされていたほうが便利なので初めに設定してしまう。

(eval-and-compile
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
    "Add KEY and DEF to my keymap.

BINDINGS should be of the form [KEY DEF]..."
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
    "自分で好きに使えるプレフィックスキー。"
    :type 'string :group 'init)
  (global-set-key (kbd leader-key) leader-map)
  (global-set-key (kbd "C-S-k") #'kill-whole-line)
  (define-key global-map (kbd "C-h") 'backward-delete-char-untabify)
  (define-key global-map (kbd "<f5>") 'revert-buffer)
  (define-key global-map (kbd "C-x C-c") #'save-buffers-kill-emacs)
  (define-key global-map [remap list-buffers] #'ibuffer)
  (set-leader-map
   ;; shell
   "!" #'open-shell-here
   ;; [c]ustomize
   "cv" #'customize-variable-other-window
   "cg" #'customize-group-other-window
   ;; visiting [f]ile
   "fr" #'counsel-recentf
   "fi" #'visit-init-file
   "fs" #'revisit-with-sudo
   ;; [s]earch
   "sg" #'counsel-git-grep
   "sr" #'counsel-rg
   ;; [i]nsert
   "i0" #'insert-at-beginnings-in-region
   ;; [j]ump
   "jf" #'find-function
   "jl" #'find-library
   "jm" #'pop-to-mark-command
   ;; [l]sp
   "l" #'lsp
   ;; [o]rg mode
   "oa" #'org-agenda
   ;; [w]indow
   "wt" #'toggle-window-split
   ;; mizc
   "M-w" #'win-clip))

(eval-and-compile
  (leaf dash :ensure t :require t)
  (leaf s :ensure t :require t)
  (leaf f :ensure t :require t))

;;;; Emacs や基本的なライブラリにしか依存しない関数など

(defun visit-init-file ()
  "Visit init.el."
  (interactive)
  (find-file user-init-file)
  (when (fboundp #'leaf-tree-mode) (leaf-tree-mode)))

(defun revisit-with-sudo ()
  "Revisit the file of selected buffer with root priviledge."
  (interactive)
  (find-file (s-concat "/sudo::" buffer-file-name)))

(defun ssh-find-home (userhost)
  "USERHOST として与えられる user@host のホームディレクトリを SSH 接続して開く．"
  (interactive "suser@host: ")
  (find-file (format "/ssh:%s:~" userhost)))

;; 出典: https://stackoverflow.com/a/33456622
(defun toggle-window-split ()
  "フレームにウィンドウが2つだけのとき、分割の方向を変える。"
  (interactive)
  (when (= (count-windows) 2)
    (let* ((1st (frame-first-window))
           (2nd (window-next-sibling 1st))
           (1st-buf (window-buffer 1st))
           (2nd-buf (window-buffer 2nd))
           (splitter (if (= (car (window-edges 1st))
                            (car (window-edges 2nd)))
                         #'split-window-horizontally
                       #'split-window-vertically)))
      (delete-other-windows)
      (funcall splitter)
      (set-window-buffer (selected-window) 1st-buf)
      (set-window-buffer (next-window) 2nd-buf))))

;; 出典: https://twitter.com/mkyutani/status/1503945681094201346
(defun win-clip ()
  "現在の選択範囲を clip.exe でコピーする。"
  (interactive)
  (when (region-active-p)
    (shell-command-on-region (region-beginning) (region-end) "clip.exe" nil nil)))

(defun insert-at-beginnings-in-region (word)
  "リージョンの各行について、その先頭に WORD を挿入する。"
  (interactive "sWord to insert: ")
  (when (use-region-p)
    (save-excursion
      (let ((cur (region-beginning))
            (end (region-end))
            col)
        (goto-char cur)
        (setq col (current-column))
        (while (<= (point) end)
          (insert word)
          (forward-line +1)
          (move-to-column col))))))

(defun forward-line-point (&optional n)
  "次の行に移動したときのポイントを返す。
オプション引数 N が与えられた時は、N 行先のポイントを返す。"
  (save-excursion (forward-line n) (point)))

;; TODO もとからある空白はそのままにする？
(defun join-line-without-spaces ()
  "空白文字を挟むことなく複数行を1行にまとめる。"
  (interactive)
  (save-excursion
    (call-interactively #'join-line)
    (while (search-forward " " (forward-line-point) t) (replace-match ""))))

(defun replace-in-buffer (from to)
  "現在のバッファ内で全ての FROM を TO に置き換える。"
  (save-excursion
    (goto-char 0)
    (while (search-forward from nil t)
      (replace-match to))))

;; 論文とかでは句読点をカンマとピリオドに統一したい。IME 設定は面倒なので勝手にやってほしい。
;; M-x add-file-local-variable RET replace-japanese-punctuations-on-save RET t RET

(defun replace-japanese-punctuations ()
  "すべての句読点をカンマとピリオドに置き換える。"
  (replace-in-buffer "、" "，")
  (replace-in-buffer "。" "．"))

(defcustom replace-japanese-punctuations-on-save nil
  "ファイルの保存時に日本語の句読点を全角のカンマとピリオドに置き換える．"
  :safe t :type 'bool :group 'init)

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

(defun count-without-spaces ()
  "空白文字を無視して文字数を数える。"
  (interactive)
  (how-many "\\S-" nil nil t))

(defun buffer-major-mode (buf)
  "BUF のメジャーモードを返す。"
  (with-current-buffer buf major-mode))

(defun duplicate-line-stay ()
  "カーソルのある行を下に複製する。カーソルは動かさない。"
  (interactive)
  ;; save-excursion だとなぜか行頭に移動してしまう
  (let ((pos (point)))
    (kill-whole-line) (yank) (yank)
    (goto-char pos)))

(defun forward-line-same-col (&optional n)
  "次の行の同じ列にカーソルを移動する。
オプション引数 N が与えられた時は N 行先に移動する。"
  (let ((col (current-column)))
    (forward-line n)
    (move-to-column col)))

(defun duplicate-line-down ()
  "カーソルのある行を下に複製し、カーソルを下へ移動する。"
  (interactive)
  (duplicate-line-stay)
  (forward-line-same-col +1))

(global-set-key (kbd "M-S-<down>") #'duplicate-line-down)
(global-set-key (kbd "M-S-<up>") #'duplicate-line-stay)

;; シェルとターミナル

(defcustom terminal-emulator "gnome-terminal" "Terminal enulator."
  :type 'string :group 'init)

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
  "全ての Dired バッファをリバートする。"
  (interactive)
  (let ((dired-buffers (buffer-list-major-mode 'dired-mode))
        (reload (lambda (buf) (with-current-buffer buf (revert-buffer)))))
    (mapc reload dired-buffers)))

;; 環境判定系

(defconst *linux? (eq system-type 'gnu/linux))
(defconst *macos? (eq system-type 'darwin))
(defconst *unix? (or *linux? *macos?))
(defconst *wsl? (and *unix? (s-contains-p "WSL2" (shell-command-output "uname -a"))))

(defconst dev-null
  (cond (*unix? "/dev/null")
        (t nil))
  "/dev/null のようなファイルへのパス。")

(defun major-mode-window (mode &optional frame)
  "メジャーモードが MODE であるようなウィンドウが FRAME に存在するならそれを返す。"
  (let ((windows (window-list frame)))
    (-find (lambda (win)
             (eq (buffer-major-mode (window-buffer win)) mode))
           windows)))

(leaf imenu
  :defun imenu-list-toggle
  :init
  (defun imenu-list-toggle ()
    (interactive)
    (save-selected-window
      (if-let ((window (major-mode-window 'imenu-list-major-mode)))
          (delete-window window)
        (imenu-list))))
  (set-leader-map "ti" #'imenu-list-toggle))

;; 以下は "NOT part of Emacs" なパッケージも使う

(leaf *envvar
  :config
  (leaf exec-path-from-shell :unless (eq system-type 'windows-nt)
    :ensure t
    :config
    (exec-path-from-shell-initialize))
  (setenv "LANG" "ja_JP.utf-8"))

(defcustom +dropbox-root (substitute-env-vars "$HOME/Dropbox")
  "Dropbox sync root directory."
  :type 'string :group 'init)
(defun +dropbox-root (path)
  "PATH を Dropbox ディレクトリからの相対パスとみなして絶対パスを作る。"
  (s-lex-format "${+dropbox-root}/${path}"))

(leaf savehist :global-minor-mode t)

(leaf convenience
  :custom
  (confirm-kill-emacs . #'yes-or-no-p))

(leaf restart-emacs :ensure t)

(leaf paradox :ensure t async
  :custom (paradox-github-token . t)
  :config (paradox-enable))

(leaf google-translate :ensure t popup
  :config
  ;; 出典: https://github.com/atykhonov/google-translate/issues/137#issuecomment-723938431
  (defun google-translate--search-tkk () "Search TKK." (list 430675 2721866130)))

(leaf move-text :ensure t
  :config (move-text-default-bindings))

(leaf display-line-numbers
  :hook ((prog-mode-hook org-mode-hook LaTeX-mode-hook) . display-line-numbers-mode))

(defvar delete-trailing-whitespace-modes
  (list 'prog-mode 'tex-mode)
  "保存前に `delete-trailing-whitespace' を呼び出すモードのリスト。")
(add-hook 'before-save-hook
          (lambda ()
            (when (apply #'derived-mode-p delete-trailing-whitespace-modes)
              (delete-trailing-whitespace))))

(defun push-list (list-var elts)
  "ELTS を LIST-VAR にプッシュする。
ELTS の要素の順序は保たれる。"
  (mapc (lambda (elt) (push elt (symbol-value list-var))) (seq-reverse elts)))

(leaf xdg :require t
  :defun ivy-find-xdg-user-dirs xdg-user-dir xdg-data-home
  :config
  (set-leader-map "jd" #'ivy-find-xdg-user-dirs)
  ;; Documents, Downloads, Desktop をすぐに開けるようにする
  (defvar xdg-user-dirs-of-interest
    (mapcar (-compose (lambda (s) (s-concat s "/")) #'xdg-user-dir)
            '("DOCUMENTS" "DOWNLOAD" "DESKTOP")))
  (defun ivy-find-xdg-user-dirs ()
    (interactive)
    (ivy-read "Find XDG User Dir: " xdg-user-dirs-of-interest
              :require-match t
              :action #'find-file)))

(leaf info
  :config
  (setq Info-directory-list (-union Info-directory-list Info-default-directory-list))
  (add-to-list 'Info-directory-list (concat (xdg-data-home) "/info")))

(leaf electric-pair-local-mode :hook prog-mode-hook)
(leaf show-paren-mode :hook prog-mode-hook)
(leaf hideshow :hook (prog-mode-hook . hs-minor-mode) :diminish hs-minor-mode)

(leaf recentf
  :custom (recentf-max-saved-items . 1000)
  :global-minor-mode t)

(leaf project
  :defun project-root
  :custom (project-vc-merge-submodules . nil)  ; サブモジュールは別プロジェクト扱い
  :config
  (defun project-counsel-rg ()
    (interactive)
    (counsel-rg nil (project-root (project-current t))))
  :bind (project-prefix-map ("C-s" . project-counsel-rg)))

(leaf projectile :ensure t :global-minor-mode t)

(leaf cus-start
  :custom
  (tool-bar-mode . nil)
  (scroll-conservatively . 1) ; C-n, C-p でポイントが画面外に出たとき一行だけスクロール
  (scroll-preserve-screen-position . t) ; C-v, M-v でポイントのウィンドウ内相対位置を変えない
  )

;; 目で追いやすくするために、マウススクロールを遅く保つ
(leaf mwheel :custom (mouse-wheel-progressive-speed . nil))

(leaf minimap :ensure t :require t
  :custom (minimap-window-location . 'right)
  :config (set-leader-map "tM" #'minimap-mode))

(leaf menu-bar
  :config
  (menu-bar-mode -1)
  (set-leader-map "tm" #'menu-bar-mode))

(leaf undo-tree
  :ensure t
  :global-minor-mode global-undo-tree-mode
  :custom
  (undo-tree-history-directory-alist . `((".*\.gpg" . ,dev-null)
                                         (".*" . ,(concat user-emacs-directory ".cache/undo-tree"))))
  :diminish undo-tree-mode)

(leaf eldoc :diminish eldoc-mode :custom (eldoc-idle-delay . 0.2))

;; auto-revert-mode is enabled on all Git-managed files due to magit-auto-revert-mode
(leaf autorevert :diminish auto-revert-mode)

(leaf hl-todo :ensure t
  :hook prog-mode-hook org-mode-hook
  :defvar hl-todo-keyword-faces
  :config
  (add-to-list 'hl-todo-keyword-faces '("WARN" . "#ff0000"))
  (add-to-list 'hl-todo-keyword-faces '("\?\?\?" . "#cc9393")))

(leaf ivy
  :ensure t swiper counsel ivy-hydra ivy-rich
  :global-minor-mode t counsel-mode ivy-rich-mode
  :bind (("C-s" . swiper))
  :diminish ivy-mode counsel-mode)

(leaf winum
  :ensure t
  :global-minor-mode t
  :config
  (dolist (digit (number-sequence 0 9))
    (define-key global-map (kbd (format "M-%d" digit))
      (intern (format "winum-select-window-%d" digit)))))

(leaf winner :config (winner-mode +1))

(leaf which-key :ensure t :global-minor-mode t :diminish which-key-mode)

(leaf *git
  :defun hydra-git/body
  :config
  (leaf magit :ensure t :require t)
  (leaf git-gutter+ :ensure t :global-minor-mode global-git-gutter+-mode
    :diminish git-gutter+-mode
    :defvar git-gutter+-mode
    :defun git-gutter+-refresh
    :init
    (defun git-gutter+-refresh-all-buffers ()
      (interactive)
      ;; TODO: .gitignore されているファイルなど、Git 管理外のファイルでもリフレッシュが
      ;;       試行される場合がある。エラーメッセージがエコーエリアに出てしまうので上手く回避したい。
      (in-all-buffers-where (-const git-gutter+-mode) (git-gutter+-refresh))))
  (leaf git-modes :ensure t)
  (set-leader-map "g" #'hydra-git/body)
  :hydra (hydra-git
          (:hint nil)
          "
^Move^^^              ^Action^            ^Magit
^^^--------------------------------------------------
_n_: next^^           _a_: stage          _g_: magit
_p_: prev^^           _r_: revert         _b_: blame
_s_, _<tab>_: show    _U_: unstage all    _c_: commit
"
          ("n" git-gutter+-next-hunk)
          ("p" git-gutter+-previous-hunk)
          ("s" git-gutter+-show-hunk-inline-at-point)
          ("<tab>" git-gutter+-show-hunk-inline-at-point)
          ("a" git-gutter+-stage-hunks)
          ("r" git-gutter+-revert-hunks)
          ("U" git-gutter+-unstage-whole-buffer)
          ("g" magit-status :exit t)
          ("b" magit-blame :exit t)
          ("c" magit-commit :exit t)
          ("R" git-gutter+-refresh-all-buffers "refresh gutter in all buffers" :exit t)))

(leaf rg :ensure t)

(leaf dockerfile-mode :ensure t)

(leaf yaml-mode :ensure t)

(leaf systemd :when (executable-find "systemctl") :ensure t)

;; counsel-bookmark を名前順で表示したい。
(advice-add 'bookmark-all-names :filter-return (lambda (names) (sort names #'string<)))

(setq ring-bell-function 'ignore)

(leaf yasnippet :ensure t :global-minor-mode yas-global-mode :diminish yas-minor-mode)

;; `completion-at-point' を直接利用する場合と比べて、補完中にドキュメントを読めることが company の利点。
;; 独自 UI よりも `counsel-company' ほうが候補の絞り込みに便利だが、後者ではドキュメント表示ができないので我慢。
(leaf company :ensure t :global-minor-mode global-company-mode
  :diminish company-mode
  :custom
  ;; `company-posframe' があれば `company-echo-metadata-frontend' は不要
  (company-frontends . '(company-pseudo-tooltip-unless-just-one-frontend company-preview-if-just-one-frontend))
  (company-idle-delay . 0)
  :bind
  (company-mode-map ("C-M-i" . company-complete))
  (company-active-map ("C-h" . backward-delete-char-untabify)
                      ("M-<" . #'company-select-first)
                      ("M->" . #'company-select-last))
  :config
  (leaf company-posframe :ensure t :global-minor-mode t
    ;; *Help* が汚染されるのでドキュメントは手動 (<f1>キー) で開く
    :custom (company-posframe-quickhelp-delay . nil)
    :diminish company-posframe-mode))

;; Emoji ✨
(leaf emojify :ensure t :global-minor-mode 'global-emojify-mode
  :custom
  (emojify-display-style . 'image)
  (emojify-emoji-styles . '(unicode))
  :config
  (set-leader-map "ie" #'emojify-insert-emoji))

(leaf smartparens :ensure t
  :require t smartparens-config
  :hook prog-mode-hook TeX-mode-hook
  :diminish smartparens-mode
  :defun sp-rewrap-sexp sp-unwrap-sexp hydra-parens/body
  :hydra (hydra-parens
          (:hint nil)
          "
^Edit^         ^Move^           ^Mark/Kill
^^^^^-------------------------------------
_s_: slurp     _f_: forward     _k_: kill
_|_: split     _b_: backward    _y_: yank
_/_: undo      _d_: down        ^ ^
^ ^            _u_: up          ^ ^
^ ^            ^ ^              ^ ^
"
          ("s" sp-slurp-hybrid-sexp)
          ("|" sp-split-sexp)
          ("/" undo)

          ("f" forward-sexp)
          ("b" backward-sexp)
          ("d" down-list)
          ("u" backward-up-list)

          ("k" sp-kill-sexp)
          ("y" yank))
  :config (set-leader-map "pr" #'sp-rewrap-sexp "pu" #'sp-unwrap-sexp "pp" #'hydra-parens/body))

(leaf multiple-cursors :ensure t
  :bind
  ("C-M-<down>" . mc/mark-next-lines)
  ("C-M-<up>" . mc/mark-previous-lines)
  ("M-N" . mc/mark-next-like-this-symbol)
  ("M-P" . mc/mark-previous-like-this-symbol))

(leaf expand-region :ensure t
  :custom
  (expand-region-contract-fast-key . "c")
  (expand-region-show-usage-message . nil)
  :config (set-leader-map "x" #'er/expand-region))

;;;; 外部ツールインテグレーション

;; シェル周り

;; vterm (https://github.com/akermu/emacs-libvterm)
;; Ubuntu では `libtool', `libtool-bin', `cmake', `libvterm-dev' が必要。
;; シェル側の設定も必要なので注意 (https://github.com/akermu/emacs-libvterm#shell-side-configuration)
(leaf vterm :ensure t
  :bind
  (:vterm-mode-map
   ("C-h" . vterm-send-C-h)
   ("C-v" . scroll-up-command)
   ("M-v" . scroll-down-command))
  :init
  (defun vterm-other-window (&optional arg)
    "Open vterm in other window.

ARG is passed to `vterm', so refer to its docstring for exaplanation."
    (interactive "P")
    (pop-to-buffer nil)
    (vterm arg))
  (set-leader-map "4!" #'vterm-other-window))

(leaf fish-mode :when *unix? :ensure t)

;; 言語設定とMigemo

(set-language-environment "Japanese")
(prefer-coding-system 'utf-8)
;; Ubuntu  -- apt install cmigemo
;; Windows -- https://www.kaoriya.net/software/cmigemo/
;; M-x customize-variable migemo-dictionary
(leaf migemo :ensure t)
(leaf swiper-migemo
  :require t
  :defvar swiper-migemo-enable-command
  :defun global-swiper-migemo-mode
  :config
  (set-leader-map "sm" #'global-swiper-migemo-mode)
  (add-to-list 'swiper-migemo-enable-command 'counsel-recentf)
  (add-to-list 'swiper-migemo-enable-command 'counsel-rg)
  :custom (migemo-options . '("--quiet" "--nonewline" "--emacs")))

;; 入力メソッド
;; |      | C-\   | <C-henkan> | <c-muhenkan> |
;; |------+-------+------------+--------------|
;; | NoIM | Agda  | Mozc       | NoIM         |
;; | Agda | NoIME | Mozc       | NoIM         |
;; | Mozc | Agda  | Mozc       | NoIM         |
(leaf *input-method
  :init (defcustom use-mozc *wsl? "Mozc" :type 'bool :group 'init)
  :defvar use-mozc
  :config
  (leaf agda-input :require t
    :custom (default-input-method . "Agda"))
  (leaf mozc :when use-mozc
    :ensure t
    :custom (mozc-candidate-style . 'echo-area))
  (when use-mozc
    (eval-and-compile
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
      (global-set-key (kbd "<C-muhenkan>") #'im-C-muhenkan))))

;; PDF

;; https://github.com/politza/pdf-tools#server-prerequisites
(leaf pdf-tools :when *unix? :ensure t
  :bind (pdf-view-mode-map ("C-s" . pdf-occur))
  :init
  (if (daemonp) (add-hook 'server-after-make-frame-hook #'pdf-tools-install)
    (pdf-tools-install))
  :hook (pdf-view-mode-hook . auto-revert-mode))

;; Markdown

(leaf markdown-mode :when *unix? :ensure t
  :custom (markdown-command . "marked")
  :mode ("\\.md$" . gfm-mode))

(leaf writeroom-mode :ensure t)

;;;; プログラミング

(leaf flycheck :ensure t :hook prog-mode-hook LaTeX-mode-hook
  :defun flycheck-list-errors hydra-flycheck/body
  :config (set-leader-map "e" #'hydra-flycheck/body)
  :custom (flycheck-display-errors-delay . 0)  ; HACK: あえて即表示させると ElDoc が上書きできる
  :init
  (defun flycheck-toggle-error-list (&optional frame)
    (interactive)
    (if-let ((window (major-mode-window 'flycheck-error-list-mode frame)))
        (delete-window window)
      (flycheck-list-errors)))
  :hydra (hydra-flycheck nil "Flycheck"
          ("e" flycheck-display-error-at-point "explain")
          ("n" flycheck-next-error "next")
          ("p" flycheck-previous-error "prev")
          ("l" flycheck-toggle-error-list "list")
          ("?" flycheck-describe-checker "help" :exit t)))

(leaf lsp
  :ensure lsp-mode lsp-ui
  :defun lsp-format-buffer
  :init
  ;; 参考: https://github.com/ncaq/.emacs.d/blob/f1612eeb346974254e893c091901c987003d5e53/init.el#L971-L973
  (eval-and-compile
    (defvar lsp-format-before-save t "LSPモードが入っているバッファを保存するときにフォーマットするかどうか。")
    (defun lsp-toggle-format-before-save ()
      "`lsp-format-before-save' をトグルする。"
      (interactive)
      (setq lsp-format-before-save (null lsp-format-before-save)))
    (defun lsp-format-buffer-no-error ()
      ;; エラーを握りつぶす
      (condition-case err (lsp-format-buffer)
        (lsp-capability-not-supported nil)))
    (defun lsp-format-before-save ()
      "LSPモードが有効かつ `lsp-format-before-save' が非 nil なら、`lsp-format-buffer' を呼び出す。"
      (when (and (boundp 'lsp-mode) lsp-mode lsp-format-before-save)
        (lsp-format-buffer-no-error)))
    (add-hook 'before-save-hook #'lsp-format-before-save))
  ;; 既に LSP サーバが既に走っているプロジェクトのファイルを開くときは自動的に接続。
  ;; ただし少しだけファイルを開きたいときにいちいち LSP サーバが起動すると鬱陶しいので、
  ;; LSP を使いたいプロジェクトでは初めに明示的に `lsp' を呼び出す。
  ;; 出典: https://github.com/kurnevsky/dotfiles/blob/c0049a655a502cd81f1aba7321ff65d178a557c9/.emacs.d/init.el#L1231-L1237
  (eval-and-compile
    (defun lsp-activate-if-already-activated (server-id)
      (when (and (functionp 'lsp-find-workspace)
                 (lsp-find-workspace server-id (buffer-file-name)))
        (lsp)))
    (add-hook 'scala-mode-hook
              (lambda () (lsp-activate-if-already-activated 'metals))))
  :custom
  `(lsp-keymap-prefix . ,(concat leader-key " l"))
  (lsp-idle-delay . 0.2)
  :custom-face
  ;; TODO: ピーク時にはむしろ背景をグレーにしたい
  (lsp-ui-peek-peek . '((t (:background "dim gray"))))
  :config
  (leaf lsp-lens :diminish lsp-lens-mode))

;; Scala

(leaf lsp-metals :when (executable-find "scala") :ensure t)

;; SMT-LIB

(leaf smtlib-mode :require t)

;; CUDA
(leaf *cuda :mode ("\\.cu$" . c-mode))

;; Rust

(leaf rust-mode :when (executable-find "cargo") :ensure t)

;; Racket

(leaf racket-mode :when (executable-find "raco")
  :ensure t
  :config
  (leaf racket-xp
    :require t
    :hook (racket-mode-hook . racket-xp-mode)))

;; Haskell

(leaf *haskell :when (executable-find "ghc")
  :ensure haskell-mode lsp-haskell)

;; TeX

(defun tex-installed-p () "TeX がインストールされているか。" (executable-find "tex"))

(leaf latex :when (tex-installed-p)
  :ensure auctex
  :defvar TeX-command-list
  :custom
  (LaTeX-electric-left-right-brace . t)
  :hook
  (LaTeX-mode-hook . (lambda () (reftex-mode +1)))
  :config
  (setq TeX-command-list (-remove (lambda (l) (string-equal (car l) "LaTeX")) TeX-command-list))
  (push '("LaTeX" "latexmk --synctex=1 %T" TeX-run-command nil t) TeX-command-list)

  (leaf magic-latex-buffer :ensure t :diminish t
    :custom
    (magic-latex-enable-block-align . nil)
    (magic-latex-enable-inline-image . nil)
    :hook (LaTeX-mode-hook . (lambda () (when (display-graphic-p) (magic-latex-buffer +1)))))
  (leaf reftex :diminish t
    :custom
    (reftex-label-alist .
                        '(("definition" ?d "def:" "~\\ref{%s}" t ("definition" "def.") -3)
                          ("lemma" ?m "lem:" "~\\ref{%s}" t ("lemma" "lem.") -3)
                          ("theorem" ?h "thm:" "~\\ref{%s}" t   ("theorem" "th.") -3))))
  (leaf bibtex
    :custom
    (bibtex-files . '(bibtex-file-path)))
  )

(leaf satysfi :when (executable-find "satysfi") :require t
  :mode (("\\.saty$" "\\.satyh$" "\\.satyg$") . satysfi-mode)
  :hook
  (satysfi-mode-hook . (lambda () (indent-tabs-mode -1)))
  (satysfi-mode-hook . (lambda () (display-line-numbers-mode +1)))
  :config
  (eval-and-compile
    (defun satysfi-find-pdf-other-window ()
      "Visit PDF for visiting SATySFi source in other window."
      (interactive)
      (let ((pdf-name (s-replace-regexp "\\.saty$" ".pdf" (buffer-file-name))))
        (let ((buf (current-buffer)))
          (find-file-other-window pdf-name))))
    (defalias 'satysfi-mode/open-pdf #'satysfi-find-pdf-other-window)))

;; Org

(leaf org
  :custom
  (org-adapt-indentation . nil)
  (org-agenda-files . `(,(+dropbox-root "org")))
  (org-agenda-span . 'month)
  (org-export-use-babel . nil)
  (org-export-with-broken-link . t)
  :init (defvar org-indent-mode-on-automatically t)
  :defvar org-indent-mode-on-automatically
  :defun org-indent-mode
  :bind
  (:org-mode-map
   ("M-p" . org-move-subtree-up)
   ("M-n" . org-move-subtree-down))
  :hook
  (org-mode-hook . (lambda () (when org-indent-mode-on-automatically (org-indent-mode +1))))
  )

;; Go

(leaf go-mode :ensure t)

;;;; 見た目

(leaf treemacs :ensure t
  :bind
  ("M-0" . #'treemacs)
  (treemacs-mode-map
   ("k" . #'treemacs-previous-line)
   ("j" . #'treemacs-next-line))
  :config (set-leader-map "0" #'treemacs))

(leaf all-the-icons :ensure t
  :config
  (leaf all-the-icons-dired :ensure t :hook dired-mode-hook
    :custom (all-the-icons-dired-monochrome . nil))
  (leaf all-the-icons-ivy-rich :ensure t :require t :after ivy ivy-rich
    :config
    ;; XDG User Dir は D から始まるディレクトリが多くて識別しづらいので
    ;; アイコンを表示してわかりやすくする。
    (push-list
     'all-the-icons-ivy-rich-display-transformers-list
     '(ivy-find-xdg-user-dirs
       (:columns
        ((all-the-icons-ivy-rich-file-icon)
         (all-the-icons-ivy-rich-project-name))
        :delimiter "\t")))
    ;; push-list したあとで有効化
    (all-the-icons-ivy-rich-mode +1))
  (leaf all-the-icons-ibuffer :ensure t :hook ibuffer-mode-hook))

(leaf *mode-line
  ;; https://ayatakesi.github.io/emacs/28.1/html/Optional-Mode-Line.html#Optional-Mode-Line
  :global-minor-mode line-number-mode column-number-mode
  :custom
  (column-number-indicator-zero-based . nil)
  (mode-line-percent-position . nil)
  (mode-line-compact . t))

;; フォント設定
(eval-and-compile
  (defconst source-code-pro "Source Han Code JP-13")
  (defcustom default-font-name source-code-pro "Default font name."
    :type 'string :group 'init)
  (defun set-font (&optional new-default)
    "フォントを `default-font-name' に設定する。

NEW-DEFAULT が非 nil のときは、現在のセッションに限りこれを新たなデフォルトとする。"
    (interactive (list (read-string "font: " default-font-name)))
    (when new-default (customize-set-variable 'default-font-name new-default))
    (set-frame-font default-font-name nil t))
  (if (daemonp) (add-hook 'server-after-make-frame-hook #'set-font)
    (add-hook 'after-init-hook #'set-font)))

(leaf centaur-tabs :ensure t :global-minor-mode centaur-tabs-mode
  :defun centaur-tabs-headline-match
  :bind
  (centaur-tabs-mode-map
   ("C-<next>" . centaur-tabs-forward)
   ("C-<prior>" . centaur-tabs-backward))
  :hook
  ((imenu-list-major-mode-hook) . centaur-tabs-local-mode)
  :custom
  (centaur-tabs-set-icons . t)
  (centaur-tabs-set-modified-marker . t)
  :config
  (set-leader-map "tt" #'centaur-tabs-mode)
  :defer-config
  (centaur-tabs-headline-match))

(leaf rainbow-delimiters :ensure t :hook prog-mode-hook TeX-mode-hook)
(leaf highlight-thing :ensure t :hook (prog-mode-hook . highlight-thing-mode)
  :diminish highlight-thing-mode
  :custom (highlight-thing-delay-seconds . 0.2)
  :custom-face (highlight-thing . '((t (:inherit 'highlight))))
  :init
  (defun toggle-hlt () (highlight-thing-mode 'toggle))
  :config
  ;; LSP モードでは無効化する
  (leaf lsp :hook (lsp-mode-hook . toggle-hlt)))
(leaf volatile-highlights :ensure t :global-minor-mode volatile-highlights-mode
  :diminish volatile-highlights-mode)
(leaf hi-lock :diminish hi-lock-mode)

(leaf *theme
  :ensure vscode-dark-plus-theme spacemacs-theme
  :config
  (eval-and-compile
    (defun toggle-theme ()
      "ライトテーマとダークテーマを切り替える。"
      (interactive)
      (let ((in-light-theme-tmp (in-light-theme)))
        (mapc #'disable-theme custom-enabled-themes)
        (if in-light-theme-tmp
            (load-theme default-dark-theme)
          (load-theme default-light-theme))))
    (defcustom default-light-theme 'spacemacs-light "デフォルトのライトテーマ"
      :type 'symbol :group 'init)
    (defcustom default-dark-theme 'vscode-dark-plus "デフォルトのダークテーマ"
      :type 'symbol :group 'init)
    (defun in-light-theme ()
      "現在、ライトテーマが設定されている。"
      (eq (car custom-enabled-themes) default-light-theme))
    (load-theme default-dark-theme)))

(leaf dashboard :ensure t
  :config (dashboard-setup-startup-hook)
  :custom
  (dashboard-startup-banner . 'logo)
  (dashboard-center-content . t)
  (dashboard-items . '((bookmarks . 5) (recents . 3)))
  (dashboard-footer-messages . '("Happy Coding!"))
  (initial-buffer-choice . (lambda () (get-buffer-create "*dashboard*"))))

;; TODO モードライン
;; - SWM (swiper-migemo-mode) を目立たせる

(put 'narrow-to-region 'disabled nil)

(provide 'init)

;; Local Variables:
;; indent-tabs-mode: nil
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:

;;; init.el ends here
