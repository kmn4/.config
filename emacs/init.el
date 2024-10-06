;;; init.el --- Emacs initialization script  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(defun report-time (&optional msg)
  (message "%s\t%s" (format-time-string "%H:%M:%S.%3N") msg))
(defun advice-leaf-report-time (&rest arg)
  (report-time (car arg)))
(advice-add 'leaf :before #'advice-leaf-report-time)

(report-time "init.el 読み込み開始")

;; init.el は GitHub を使って複数の環境から取得・更新するので、
;; 公開したくない情報や環境特有の設定はカスタマイズ変数にして別ファイルに追い出す。
(prog1 "カスタムファイルを設定"
  (setq custom-file (locate-user-emacs-file "emacs-custom-settings.el"))
  (unless (file-exists-p custom-file) (with-temp-buffer (write-file custom-file)))
  (load custom-file))

;; ネイティブコンパイル
(defun number-of-cpu-cores ()
  (or (and (executable-find "grep") (file-exists-p "/proc/cpuinfo")
           (string-to-number
            (shell-command-to-string
             "grep -c ^processor /proc/cpuinfo")))
      0))
(require 'comp)
(setq package-native-compile t)
(setq native-comp-async-jobs-number (/ (number-of-cpu-cores) 2))
(defun native-comp-async-elpa ()
  "/elpa 以下のファイルをネイティブコンパイルする。"
  (interactive)
  (native-compile-async (concat user-emacs-directory "elpa") 'recursively))

;;;; load-path と leaf.

(prog1 "site-lisp/ 以下をロードパスに追加"
  (let ((default-directory (concat user-emacs-directory "site-lisp/")))
    (unless (file-exists-p default-directory)
      (make-directory default-directory))
    (add-to-list 'load-path default-directory)
    (normal-top-level-add-subdirs-to-load-path)))

;; leaf
(eval-and-compile
  (require 'package)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
  (unless (package-installed-p 'leaf)
    (package-refresh-contents)
    (package-install 'leaf))
  (leaf leaf-keywords :ensure t diminish hydra :config (leaf-keywords-init)))

(leaf el-get :ensure t :custom (el-get-notify-type . 'message))

;;;; キーマップ。
;; init.el に問題があってすべてを読み込めないときでも
;; ある程度バインドされていたほうが便利なので初めに設定してしまう。

(prog1 "キーマップ"
  ;; 主に macOS で円記号をバックスラッシュに読み替える．
  ;; 出典: https://tsuu32.hatenablog.com/entry/2019/08/27/122459
  (define-key local-function-key-map (kbd "¥") (kbd "\\"))
  (define-key local-function-key-map (kbd "C-¥") (kbd "C-\\"))
  (define-key local-function-key-map (kbd "M-¥") (kbd "M-\\"))
  (define-key local-function-key-map (kbd "C-M-¥") (kbd "C-M-\\"))

  ;; 独自のキーマップ。Spacemacs のスペースキーにバインドされたマップみたいに使う。
  (defconst leader-map (make-sparse-keymap) "My keymap.")

  (defun set-leader-map (key def &rest bindings)
    "Add KEY and DEF to my keymap.

BINDINGS should be of the form [KEY DEF]..."
    (while key
      (define-key leader-map (kbd key) def)
      (setq key (pop bindings) def (pop bindings))))

  ;; leader-map をバインドするキー。
  ;; キーボードや OS によって異なるキーを使いたいのでカスタマイズ変数とする。
  (defcustom leader-key
    (cond ((eq system-type 'gnu/linux) "C-<henkan>")
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
   "cs" #'customize-face-other-window
   ;; visiting [f]ile
   "fr" #'counsel-recentf
   "fi" #'visit-init-file
   "fs" #'revisit-with-sudo
   ;; [h]elp
   "h" help-map
   ;; [s]earch
   "sg" #'counsel-git-grep
   "sr" #'counsel-rg
   ;; [i]nsert
   "i0" #'insert-at-beginnings-in-region
   ;; [j]ump
   "jv" #'find-variable
   "jf" #'find-function
   "jl" #'find-library
   "js" #'find-face-definition ; ?s for "style"
   "jm" #'pop-to-mark-command
   ;; [l]sp
   "l" #'lsp
   ;; [o]rg mode
   "oa" #'org-agenda
   ;; [w]indow
   "wt" #'toggle-window-split
   ;; mizc
   "M-w" #'win-clip))

(leaf dash :ensure t)
(leaf s :ensure t)
(leaf f :ensure t)
(require 'dash)
(require 's)
(require 'f)

;;;; Emacs や基本的なライブラリにしか依存しない関数など

(defun visit-init-file ()
  "Visit init.el."
  (interactive)
  (find-file user-init-file))

(defun revisit-with-sudo ()
  "Revisit the file of selected buffer with root priviledge."
  (interactive)
  (find-file (s-concat "/sudo::" buffer-file-name)))

(defun ssh-find-home (userhost)
  "USERHOST として与えられる user@host のホームディレクトリを SSH 接続して開く．"
  (interactive "suser@host: ")
  (find-file (format "/ssh:%s:~" userhost)))

(defcustom ssh-hosts '() "SSH hosts and users."
  :type '(repeat string))

(defun ssh-connect ()
  (interactive)
  (ivy-read "connect to: " ssh-hosts
            :action #'ssh-find-home))

(defun map-frame-parameter (fn param &optional frame)
  (unless frame (setq frame (selected-frame)))
  (set-frame-parameter
   frame param
   (funcall fn (frame-parameter frame param))))

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

(leaf duplicate-line :el-get manateelazycat/duplicate-line
  :require t
  :defun
  duplicate-line-or-region-above
  duplicate-line-or-region-below
  duplicate-line-above-comment
  duplicate-line-below-comment
  :bind
  ("M-S-<up>" . duplicate-line-above)
  ("M-S-<down>" . duplicate-line-below)
  :init
  (defun duplicate-line-above (comment)
    (interactive "P")
    (if comment
        (duplicate-line-above-comment)
      (duplicate-line-or-region-above)))
  (defun duplicate-line-below (comment)
    (interactive "P")
    (if comment
        (duplicate-line-below-comment)
      (duplicate-line-or-region-below))))

;; `add-to-list' と grugru したい
(defun delete-from-list (list-var element)
  "`add-to-list' の逆操作のつもり。"
  (set list-var (delete element (symbol-value list-var))))

(defun version>= (v1 v2) (version<= v2 v1))

(defun version> (v1 v2) (version< v2 v1))

;; シェルとターミナル

(defun start-shell-command (command)
  "出力のキャプチャや通信を一切せずに COMMAND を実行する。"
  (call-process-shell-command command nil 0))

(defun run-shell-command (command)
  (call-process-shell-command command nil nil))

(defun test-shell-command (command)
  (eq (run-shell-command command) 0))

(defalias 'shell-command-output #'shell-command-to-string)

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

;; マウス
(defmacro defun-with-mouse-posn-set (name &optional docstring &rest body)
  "クリック位置にポイントをセットしてから BODY を評価するコマンドを定義する。

DOCSTRING は必須。これがないと意図通りに展開されない。"
  (declare (doc-string 2) (indent 1))
  (let ((ev (gensym)))
    `(defun ,name (,ev)
       ,docstring
       (interactive "e")
       (save-excursion
         (posn-set-point (event-start ,ev))
         ,@body))))

(defun-with-mouse-posn-set quit-window-mouse
  "マウスクリックで呼び出され、クリックイベント位置のウィンドウを閉じる。"
  (quit-window))

;; 環境判定系

(defconst *linux? (eq system-type 'gnu/linux))
(defconst *lsb-release-id
  (when *linux? (s-trim (shell-command-to-string "lsb_release -is"))))
(defconst *ubuntu? (and *linux? (equal *lsb-release-id "Ubuntu")))
(defconst *macos? (eq system-type 'darwin))
(defconst *unix? (or *linux? *macos?))
(defconst *wsl? (and *unix? (s-contains-p "WSL2" (shell-command-output "uname -a"))))

(prog1 "check requirements"
  (defun check-executable (bin)
    (unless (executable-find bin)
      (warn "!!! REQUIREMENT NOT INSTALLED !!! %s" bin)))
  (when *wsl?
    (check-executable "mozc_emacs_helper")
    (check-executable "cmigemo")))

(defconst dev-null
  (cond (*unix? "/dev/null")
        (t nil))
  "/dev/null のようなファイルへのパス。")

(defcustom terminal-emulator
  (cond (*wsl? "wt.exe nt -d ")
        (t "gnome-terminal"))
  "Terminal enulator."
  :type 'string :group 'init)

(defun open-shell-here ()
  "visit 中のファイルが存在するディレクトリでターミナルを開く。"
  (interactive)
  (open-shell default-directory))

(defun open-shell (dir)
  "DIR でシェルを開く。"
  (start-shell-command (s-concat terminal-emulator " " dir)))

(defun major-mode-window (mode &optional frame)
  "メジャーモードが MODE であるようなウィンドウが FRAME に存在するならそれを返す。"
  (let ((windows (window-list frame)))
    (-find (lambda (win)
             (eq (buffer-major-mode (window-buffer win)) mode))
           windows)))

(leaf imenu-list :ensure t
  :bind (leader-map :package init ("ti" . imenu-list-toggle))
  :init
  (defun imenu-list-toggle ()
    (interactive)
    (save-selected-window
      (if-let ((window (major-mode-window 'imenu-list-major-mode)))
          (delete-window window)
        (imenu-list)))))

;; `view-lossage' 用に保存されるキーの個数を増やす
(leaf help :config (lossage-size 1000))
(leaf help-fns
  :bind (help-map ("W" . describe-widget)
                  ("B" . button-describe)))

(leaf elisp-mode :bind (emacs-lisp-mode-map ("C-c x" . emacs-lisp-macroexpand)))
(leaf macrostep :ensure t
  :bind (emacs-lisp-mode-map :package init ("C-c e" . macrostep-expand)))
(leaf simple :custom (eval-expression-print-length . nil))

(leaf ibuffer :custom (ibuffer-default-sorting-mode . 'filename/process))

(defun kill-buffer-dwim (other-buffer)
  (interactive "P")
  (if other-buffer
      (call-interactively #'kill-buffer)
    (kill-this-buffer)))

(global-set-key (kbd "C-x k") #'kill-buffer-dwim)

(defun shell-like-C-w ()
  (interactive)
  (call-interactively
   (if (region-active-p)
       #'kill-region
     #'backward-kill-word)))

(global-set-key (kbd "C-w") #'shell-like-C-w)

;; 以下は "NOT part of Emacs" なパッケージも使う

(leaf exec-path-from-shell :when *unix?
  :ensure t
  :custom (exec-path-from-shell-variables . '("PATH" "MANPATH" "INFOPATH" "LANG"))
  :config (exec-path-from-shell-initialize))

(leaf woman :when *unix?
  :after exec-path-from-shell
  :custom `(woman-locale . ,(getenv "LANG")))

(leaf savehist :global-minor-mode t
  :custom (history-length . 2000))

(leaf convenience
  :custom
  (confirm-kill-emacs . #'yes-or-no-p))

(leaf restart-emacs :ensure t)

(defmacro with-logging-messages (file &rest form)
  "FORM を評価し、その間のメッセージを FILE に書き込む。"
  (declare (indent 1))
  ;; 一時バッファにメッセージバッファの内容を退避し、後で復元する
  `(with-temp-buffer
     (let ((tmp-buf (current-buffer)))
       (buffer-swap-text (messages-buffer))
       (switch-to-buffer (messages-buffer))
       (unwind-protect (let ((message-log-max t)) ,@form)
         ;; write new messages to file and take old message back to msg-buf
         (write-file ,file)
         (switch-to-buffer (messages-buffer))
         (buffer-swap-text tmp-buf)))))

(defcustom package-upgrade-log-directory (concat user-emacs-directory "package-logs/")
  "`package-upgrade-packages-with-logging' のログの保存先"
  :type 'string :group 'init)

(defun package-upgrade-packages ()
  (interactive)
  (save-window-excursion
    (package-refresh-contents)
    (list-packages 'no-fetch)
    (package-menu-mark-upgrades)
    (package-menu-execute t)))

(defun package-upgrade-packages-with-logging ()
  "`package-upgrade-packages' を呼び出し、その間のメッセージをログファイルに書き込む。

ログファイルは `package-upgrade-log-directory' の下に呼び出し時の時刻を含む名前で保存される。"
  (interactive)
  (unless (file-exists-p package-upgrade-log-directory)
    (make-directory package-upgrade-log-directory t))
  (let ((start-time-string (format-time-string "%Y%m%dT%H%M%S")))
    (with-logging-messages
        (concat package-upgrade-log-directory start-time-string "_messages.log")
      (package-upgrade-packages))))

(leaf google-translate :ensure t popup)

(leaf move-text :ensure t
  :config (move-text-default-bindings))

(leaf display-line-numbers :hook prog-mode-hook text-mode-hook)

(defvar delete-trailing-whitespace-modes
  (list 'prog-mode 'tex-mode 'conf-mode)
  "保存前に `delete-trailing-whitespace' を呼び出すモードのリスト。")
(add-hook 'before-save-hook
          (lambda ()
            (when (apply #'derived-mode-p delete-trailing-whitespace-modes)
              (delete-trailing-whitespace))))

(defun push-list (list-var elts)
  "ELTS を LIST-VAR にプッシュする。
ELTS の要素の順序は保たれる。"
  (mapc (lambda (elt) (push elt (symbol-value list-var))) (seq-reverse elts)))

(leaf xdg :when *unix? :require t
  :defun xdg-user-dir xdg-data-home
  :bind (leader-map :package init ("jd" . ivy-find-xdg-user-dirs))
  :config
  ;; Documents, Downloads, Desktop をすぐに開けるようにする
  (defvar xdg-user-dirs-of-interest
    (mapcar (-compose (lambda (s) (s-concat s "/")) #'xdg-user-dir)
            '("DOCUMENTS" "DOWNLOAD" "DESKTOP")))
  (defun ivy-find-xdg-user-dirs ()
    (interactive)
    (ivy-read "Find XDG User Dir: " xdg-user-dirs-of-interest
              :require-match t
              :action #'find-file)))

(leaf image-dired
  :defun
  ;; defined in image-dired
  image-dired-track-original-file
  image-dired-thumbnail-display-external
  image-dired-associated-dired-buffer
  image-dired--with-marked
  image-dired-thumb-update-marks
  image-dired-mark-thumb-original-file
  ;; defined within this block
  image-dired-thumbnail-num-marked
  :bind
  (image-dired-thumbnail-mode-map
   ("n" . image-dired-next-line)
   ("p" . image-dired-previous-line)
   ("f" . image-dired-forward-image)
   ("b" . image-dired-backward-image)
   ("U" . image-dired-thumbnail-unmark-all)
   ([double-mouse-1] . image-dired-mouse-display-image)
   ([mouse-3] . image-dired-thumbnail-display-external-mouse)
   ([double-mouse-3] . image-dired-thumbnail-display-marked-external))
  (image-dired-display-image-mode-map
   ([double-mouse-1] . image-dired-display-current-image-sized)
   ([mouse-3] . quit-window-mouse))
  :init
  (defun image-dired-thumbnail-mark-all ()
    "全てのサムネイルをマークする。"
    (interactive)
    (save-excursion
      (goto-char (point-min))
      (let (lastpos (currpos (point)))
        (while (null (eq lastpos currpos))
          (setq lastpos currpos)
          (image-dired-mark-thumb-original-file)
          (setq currpos (point))))))
  (defun image-dired-thumbnail-narrow ()
    (interactive)
    (with-current-buffer (image-dired-associated-dired-buffer)
      (image-dired-display-thumbs)))
  (defun image-dired-thumbnail-unmark-all ()
    "全てのマークを解除する。"
    (interactive)
    (with-current-buffer (image-dired-associated-dired-buffer)
      (dired-unmark-all-marks))
    (image-dired-thumb-update-marks))
  (defun-with-mouse-posn-set image-dired-thumbnail-display-external-mouse
    "クリックされた画像を外部プログラムで開く。"
    (image-dired-track-original-file)
    (image-dired-thumbnail-display-external))
  (defun image-dired-thumbnail-num-marked ()
    0) ; TODO
  (defun-with-mouse-posn-set image-dired-thumbnail-display-marked-external
    "マークされた画像を全て外部プログラムで開く。

マークされた画像が多い時は確認する。"
    (let ((marked (image-dired-thumbnail-num-marked)))
      (when (or (< marked 5) (y-or-n-p (format "display %s files?" marked)))
        (image-dired--with-marked (image-dired-thumbnail-display-external))))))

(leaf info :config (nconc Info-directory-list Info-default-directory-list))

(leaf show-paren-mode :hook prog-mode-hook)
(leaf hideshow :hook (prog-mode-hook . hs-minor-mode) :diminish hs-minor-mode
  :defun hs-toggle-hiding
  :init
  (defun *hs-toggle-hiding (ev)
    (interactive "e")
    (posn-set-point (event-start ev))
    (hs-toggle-hiding))
  :bind
  ;; down-mouse-1 は lsp-mode で `lsp-find-definition-mouse' と被る
  ([(control down-mouse-3)] . nil)
  (hs-minor-mode-map ([(control mouse-3)] . *hs-toggle-hiding)))

(leaf recentf
  :custom (recentf-max-saved-items . 1000)
  :global-minor-mode t)

(leaf project
  :defun project-root
  :custom (project-vc-merge-submodules . nil)  ; サブモジュールは別プロジェクト扱い
  :config

  :bind (project-prefix-map ("C-s" . project-counsel-rg)))

(leaf projectile
  :ensure t
  :custom
  (projectile-globally-ignored-directories . nil) ; quick fix for bbatsov/projectile#1777
  (projectile-use-git-grep . t)
  :global-minor-mode t
  ;; If :bind-keymap is used, then FlyC complains that
  ;; "Symbol's value as variable is void: projectile-command-map"
  :config (global-set-key (kbd "C-c p") 'projectile-command-map)
  (leaf counsel-projectile :ensure t
    :bind
    (projectile-command-map :package projectile
     ("C-s" . counsel-projectile-rg))))

(prog1 "プロジェクトルートで TODO の一覧を表示する関数を定義。"
  (defcustom projectile-rg-todo-regex-list
    (list "\\?{3}" "TODO")
    "`projectile-rg-todo-keywords' でマッチすべき正規表現のリスト。"
    :type '(repeat regexp)
    :group 'init)
  (defun projectile-rg-todo-keywords ()
    (interactive)
    (projectile-ripgrep (s-join "|" projectile-rg-todo-regex-list) t))
  (define-key projectile-command-map (kbd "st") #'projectile-rg-todo-keywords))

(leaf *scroll :custom
  (scroll-conservatively . 1)           ; C-n, C-p でポイントが画面外に出たとき一行だけスクロール
  (scroll-preserve-screen-position . t) ; C-v, M-v でポイントのウィンドウ内相対位置を変えない
  (mouse-wheel-progressive-speed . nil) ; 目で追いやすくするために、マウススクロールを遅く保つ
  )

(leaf minimap :ensure t :require t
  :custom (minimap-window-location . 'right)
  :bind (leader-map :package init ("tM" . minimap-mode)))

(leaf undo-tree :ensure t :global-minor-mode global-undo-tree-mode
  :diminish undo-tree-mode
  :custom
  (undo-tree-auto-save-history . nil)
  (undo-tree-history-directory-alist
   . `((".*" . ,(concat user-emacs-directory ".cache/undo-tree")))))

(leaf eldoc :diminish eldoc-mode :custom (eldoc-idle-delay . 0.2))

(leaf files :custom
  (backup-directory-alist
   . `((,tramp-file-name-regexp . nil) ; https://www.gnu.org/software/emacs/manual/html_node/tramp/Auto_002dsave-File-Lock-and-Backup.html
       ("." . ,(concat user-emacs-directory ".cache/backup")))))

;; auto-revert-mode is enabled on all Git-managed files due to magit-auto-revert-mode
(leaf autorevert :diminish auto-revert-mode)

(leaf hl-todo :ensure t
  :hook prog-mode-hook org-mode-hook LaTeX-mode-hook
  :defvar hl-todo-keyword-faces
  :config
  (add-to-list 'hl-todo-keyword-faces '("WARN" . "#ff0000")))

(leaf ivy
  :ensure t swiper counsel ivy-hydra ivy-rich
  :global-minor-mode t counsel-mode ; ivy-rich-mode ; ivy-rich slow on NTFS on WSL
  :bind
  ("C-s" . swiper)
  ("C-x b" . counsel-switch-buffer)
  :diminish ivy-mode counsel-mode)

(leaf winum
  :ensure t
  :global-minor-mode t
  :config
  (dolist (digit (number-sequence 0 9))
    (define-key global-map (kbd (format "M-%d" digit))
      (intern (format "winum-select-window-%d" digit)))))

(leaf winner :config (winner-mode +1))

(leaf *window :after winum winner
  :bind (leader-map :package init ("ww" . hydra-window/body))
  :hydra
  (hydra-window
   (:hint nil)
   "
^ ^Mov^e^ to^ ^    ^ ^ Si^z^e  ^ ^    ^A^ction
^ ^   _k_   ^ ^    ^ ^   _K_   ^ ^    _v_ertical split    _z_oom
^ ^   ^|^   ^ ^    ^ ^   ^|^   ^ ^    _s_tack split
_h_ --^ ^-- _l_    _H_ --^ ^-- _L_    _b_alance
^ ^   ^|^   ^ ^    ^ ^   ^|^   ^ ^    _u_ndo layout
^ ^   _j_   ^ ^    ^ ^   _J_   ^ ^    _r_edo layout
^ ^   ^ ^   ^ ^    ^ ^   ^ ^   ^ ^    _o_ther window
"
   ("b" balance-windows)
   ("H" shrink-window-horizontally)
   ("h" windmove-left)
   ("J" shrink-window)
   ("j" windmove-down)
   ("K" enlarge-window)
   ("k" windmove-up)
   ("L" enlarge-window-horizontally)
   ("l" windmove-right)
   ("r" winner-redo)
   ("s" split-window-vertically)
   ("u" winner-undo)
   ("v" split-window-horizontally)
   ("o" other-window)
   ("z" delete-other-windows)))

(leaf which-key :ensure t :global-minor-mode t :diminish which-key-mode)

(leaf *git
  :init
  (leaf magit :ensure t :require t)
  ;; TODO TRAMP でファイルを開くとエラーになる。
  ;; これは git-gutter+ の実装が悪いためなので、git-gutter+ の使用をやめるか、
  ;; バッファ作成時にそのバッファが TRAMP モードになるかどうか判定して git-gutter+
  ;; の有効化を決定するように変更する。
  (leaf git-gutter+ :ensure t :global-minor-mode global-git-gutter+-mode :disabled t
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
  (leaf git-link :ensure t)
  :bind (leader-map :package init ("g" . hydra-git/body))
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
(leaf visual-regexp :ensure t :bind ("C-M-%" . vr/query-replace))
(leaf color-rg :el-get manateelazycat/color-rg :require t
  :custom (color-rg-search-no-ignore-file . nil)
  :bind
  ("C-c '." . color-rg-search-symbol-in-project)
  ("C-c ''" . color-rg-search-input-in-project))

(leaf dockerfile-mode :ensure t)
(leaf docker :ensure t :when (executable-find "docker")
  :bind (leader-map :package init ("d" . docker))
  :custom
  (docker-run-async-with-buffer-function . 'docker-run-async-with-buffer-vterm)
  (docker-compose-command . "docker compose")
  )
(leaf docker-tramp :ensure t)
(leaf counsel-tramp :ensure t) ; Docker コンテナを簡単に選択

(leaf yaml-mode :ensure t)

(leaf ansible :ensure t)

(leaf systemd :when (executable-find "systemctl") :ensure t)

;; counsel-bookmark を名前順で表示したい。
(advice-add 'bookmark-all-names :filter-return (lambda (names) (sort names #'string<)))

(setq ring-bell-function 'ignore)

(leaf yasnippet :ensure t yasnippet-snippets
  :global-minor-mode yas-global-mode
  :diminish yas-minor-mode)

;; `completion-at-point' を直接利用する場合と比べて、補完中にドキュメントを読めることが company の利点。
;; 独自 UI よりも `counsel-company' ほうが候補の絞り込みに便利だが、後者ではドキュメント表示ができないので我慢。
(leaf company :ensure t :global-minor-mode global-company-mode
  :diminish company-mode
  :defvar company-backends
  :custom
  ;; `company-posframe' があれば `company-echo-metadata-frontend' は不要
  (company-frontends . '(company-pseudo-tooltip-unless-just-one-frontend company-preview-if-just-one-frontend))
  (company-idle-delay . 0)
  (company-tooltip-maximum-width . 120)
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

(leaf dumb-jump :ensure t
  :config
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  )

(leaf emojify :ensure t :global-minor-mode 'global-emojify-mode
  :bind (leader-map :package init ("ie" . emojify-insert-emoji))
  :custom
  (emojify-display-style . 'image)
  (emojify-download-emojis-p . t)
  (emojify-emoji-styles . '(unicode)))

(leaf smartparens :ensure t
  :require t smartparens-config
  :hook prog-mode-hook TeX-mode-hook
  :diminish smartparens-mode
  :defun sp-rewrap-sexp sp-unwrap-sexp
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
  :bind (leader-map
         :package init
         ("pr" . sp-rewrap-sexp)
         ("pu" . sp-unwrap-sexp)
         ("pp" . hydra-parens/body)))

(leaf multiple-cursors :ensure t
  :bind
  ("C-M-<down>" . mc/mark-next-lines)
  ("C-M-<up>" . mc/mark-previous-lines)
  ("M-N" . mc/mark-next-like-this-symbol)
  ("M-P" . mc/mark-previous-like-this-symbol)
  ("M-<down-mouse-1>" . nil) ; https://github.com/magnars/multiple-cursors.el/blob/c870c18462461df19382ecd2f9374c8b902cd804/README.md?plain=1#L168
  ("M-<mouse-1>" . mc/add-cursor-on-click)
  )

(leaf grugru :ensure t
  :bind ("C-;" . grugru)
  :defun grugru-default-setup
  :config
  (grugru-define-on-major-mode 'emacs-lisp-mode 'symbol '("add-to-list" "delete-from-list"))
  (grugru-default-setup))

(leaf expand-region :ensure t
  :bind (leader-map :package init ("x" . er/expand-region))
  :custom
  (expand-region-contract-fast-key . "c")
  (expand-region-show-usage-message . nil))

;;;; 外部ツールインテグレーション

;; シェル周り

;; vterm (https://github.com/akermu/emacs-libvterm)
;; Ubuntu では `libtool', `libtool-bin', `cmake', `libvterm-dev' が必要。
;; シェル側の設定も必要なので注意 (https://github.com/akermu/emacs-libvterm#shell-side-configuration)
(leaf vterm :ensure t
  :bind
  ("C-z" . vterm-or-suspend)
  `(vterm-mode-map
    ,@(mapcar (lambda (c) (cons (concat "C-c C-" (string c)) 'vterm--self-insert))
              (seq-difference "abcdefghijklmnopqrstuvwxyz" "t"))
    ("C-c RET" . vterm-send-next-key)
    ("C-h" . vterm--self-insert)
    ("C-v" . scroll-up-command)
    ("M-v" . scroll-down-command)
    ("C-z" . previous-buffer))
  :defvar vterm-eval-cmds
  :push
  ((vterm-eval-cmds . '("dired" dired))
   (vterm-eval-cmds . '("ediff-files" ediff-files)))
  :custom
  (vterm-exit-functions . '((lambda (_ _) (previous-buffer))))
  (vterm-tramp-shells . '(("ssh" "/bin/bash")))
  :init
  (defun vterm-or-suspend ()
    "GUI フレームでは `projectile-run-vterm' を、ターミナルでは `suspend-frame' を呼ぶ。"
    (interactive)
    (if (window-system) (projectile-run-vterm) (suspend-frame))))

;; company が有効だとなぜか `sh-completion-at-point' が異常に遅い。
;; 無効にして、普通に `completion-at-point' を呼び出す。
(leaf sh-mode :hook (sh-mode-hook . (lambda () (company-mode -1))))

(leaf fish-mode :when *unix? :ensure t)

;; 言語設定とMigemo

(leaf untitled-new-buffer :ensure t
  :bind (leader-map
         :package init
         ("fn" . *untitled-new-buffer))
  :init
  (defun *untitled-new-buffer (arg)
    (interactive "P")
    (if arg
        (call-interactively #'untitled-new-buffer-with-select-major-mode)
      (untitled-new-buffer))))

(prog1 "言語、文字コード、入力メソッド"
  (set-language-environment "Japanese")
  (prefer-coding-system 'utf-8)
  (leaf mozc :when *wsl? :ensure t
    :defvar mozc-mode-map
    :custom (mozc-candidate-style . 'echo-area)
    :config
    (cl-flet* ((im-mozc-on () (interactive) (set-input-method "japanese-mozc"))
               (im-off () (interactive) (set-input-method nil)))
      (global-set-key (kbd "<henkan>") #'im-mozc-on)
      (global-set-key (kbd "<muhenkan>") #'im-off)
      (define-key mozc-mode-map (kbd "<muhenkan>") #'im-off))))

(leaf migemo :when (executable-find "cmigemo") :ensure t
  ;; Ubuntu  -- apt install cmigemo
  ;; Windows -- https://www.kaoriya.net/software/cmigemo/
  :custom (migemo-options . '("--quiet" "--nonewline" "--emacs"))
  :config
  (leaf swiper-migemo
    :el-get tam17aki/swiper-migemo
    :require t
    :defvar swiper-migemo-enable-command
    :defun global-swiper-migemo-mode
    :bind (leader-map :package init ("sm" . global-swiper-migemo-mode))
    :config
    (add-to-list 'swiper-migemo-enable-command 'counsel-recentf)
    (add-to-list 'swiper-migemo-enable-command 'counsel-rg))
  ;; :custom だと `*ubuntu?' が定義されていないと FlyC に言われる
  (customize-set-variable
   'migemo-dictionary
   (cond (*ubuntu? "/usr/share/cmigemo/utf-8/migemo-dict")
         (*macos?  "/usr/local/share/migemo/utf-8/migemo-dict")
         (t        ""))))

(defun add-init-hook (function)
  (let ((hook (if (daemonp) 'server-after-make-frame-hook 'after-init-hook)))
    (add-hook hook function)))

;; https://github.com/politza/pdf-tools#server-prerequisites
(leaf pdf-tools :when *unix? :ensure t
  :bind (pdf-view-mode-map ("C-s" . pdf-occur))
  :init (add-init-hook #'pdf-tools-install)
  :hook (pdf-view-mode-hook . auto-revert-mode))

(leaf org
  :custom
  (org-adapt-indentation . nil)
  (org-agenda-span . 'month)
  (org-export-use-babel . nil)
  (org-export-with-broken-link . t)
  :hook (org-mode-hook . org-indent-mode))

(leaf markdown-mode :when *unix? :ensure t
  :custom
  (markdown-command . "marked")
  (markdown-fontify-code-blocks-natively . t)
  :custom-face (markdown-code-face . '((t (:inherit 'default))))
  :mode ("\\.md$" . gfm-mode))

(leaf easy-hugo :when (executable-find "hugo") :ensure t
  :custom
  (easy-hugo-default-picture-directory . "~/Pictures")
  (easy-hugo-server-flags . "-D")
  (easy-hugo-url . ""))

(leaf writeroom-mode :ensure t)

(leaf csv-mode :ensure t)

;;;; プログラミング

(leaf indent :custom (indent-tabs-mode . nil))

(leaf flycheck :ensure t :hook prog-mode-hook LaTeX-mode-hook
  :defun flycheck-list-errors
  :bind (leader-map :package init ("e" . hydra-flycheck/body))
  :custom
  (flycheck-display-errors-delay . 0)  ; HACK: あえて即表示させると ElDoc が上書きできる
  (flycheck-disabled-checkers . '(scala))
  :hook
  (flycheck-error-list-mode-hook . (lambda () (toggle-truncate-lines -1)))
  :init
  (defun flycheck-toggle-error-list (&optional frame)
    (interactive)
    (if-let ((window (major-mode-window 'flycheck-error-list-mode frame)))
        (with-selected-window window
          (if (window-prev-buffers)
              (previous-buffer)
            (delete-window)))
      (flycheck-list-errors)))
  :hydra (hydra-flycheck nil "Flycheck"
          ("e" flycheck-display-error-at-point "explain")
          ("n" flycheck-next-error "next")
          ("p" flycheck-previous-error "prev")
          ("l" flycheck-toggle-error-list "list")
          ("?" flycheck-describe-checker "help" :exit t)))

(leaf quickrun :ensure t :bind (prog-mode-map ("C-c RET" . quickrun)))

(leaf lsp
  :ensure lsp-mode lsp-ui
  :defun
  lsp-format-buffer ; defined in lsp-mode
  ;; defined within this block
  lsp-format-buffer-no-error
  lsp-format-before-save
  lsp-hook-activation-in-activated-workspace
  :custom
  (lsp-idle-delay . 0.2)
  (lsp-auto-execute-action . nil)
  (lsp-modeline-code-actions-segments . '(count icon name))
  (lsp-disabled-clients . '(perl-language-server)) ; prefer PerlNavigator
  :custom-face
  ;; TODO: ピーク時にはむしろ背景をグレーにしたい
  (lsp-ui-peek-peek . '((t (:background "dim gray"))))
  :setq
  ;; 参考: https://emacs-lsp.github.io/lsp-mode/page/performance/
  (gc-cons-threshold . 100000000)               ; 100 MB
  `(read-process-output-max . ,(* 4 1024 1024)) ; 4 MiB
  :init
  (customize-set-variable 'lsp-keymap-prefix (concat leader-key " l"))
  (prog1 "lsp-mode バッファ保存時の自動フォーマット"
    ;; 参考: https://github.com/ncaq/.emacs.d/blob/f1612eeb346974254e893c091901c987003d5e53/init.el#L971-L973
    (defvar lsp-format-before-save t "LSPモードが入っているバッファを保存するときにフォーマットするかどうか。")
    (defun lsp-toggle-format-before-save ()
      "`lsp-format-before-save' をトグルする。"
      (interactive)
      (setq lsp-format-before-save (null lsp-format-before-save)))
    (defun lsp-format-buffer-no-error ()
      ;; エラーを握りつぶす
      (condition-case _ (lsp-format-buffer)
        (lsp-capability-not-supported nil)))
    (defun lsp-format-before-save ()
      "LSPモードが有効かつ `lsp-format-before-save' が非 nil なら、`lsp-format-buffer' を呼び出す。"
      (when (and (boundp 'lsp-mode) lsp-mode lsp-format-before-save)
        (lsp-format-buffer-no-error)))
    (add-hook 'before-save-hook #'lsp-format-before-save))
  ;; 既に LSP サーバが既に走っているプロジェクトのファイルを開くときは自動的に接続。
  ;; ただし少しだけファイルを開きたいときにいちいち LSP サーバが起動すると鬱陶しいので、
  ;; LSP を使いたいプロジェクトでは初めに明示的に `lsp' を呼び出す。
  ;; 参考: https://github.com/kurnevsky/dotfiles/blob/c0049a655a502cd81f1aba7321ff65d178a557c9/.emacs.d/init.el#L1231-L1237
  (defun lsp-hook-activation-in-activated-workspace (hook server-id)
    (add-hook hook
              `(lambda ()
                 (when (and (functionp 'lsp-find-workspace)
                            (lsp-find-workspace (quote ,server-id) (buffer-file-name)))
                   (lsp)))))
  :config
  (leaf lsp-lens :diminish lsp-lens-mode)
  (leaf lsp-ivy :ensure t
    :bind
    (lsp-command-map
     ("s" . lsp-ivy-workspace-symbol))))

(leaf scala-mode :when (executable-find "scala")
  :config
  (leaf lsp-metals :ensure t
    :config (lsp-hook-activation-in-activated-workspace 'scala-mode-hook 'metals))
  (leaf bloop :require t :after scala-mode
    :doc "Scalaプロジェクトを素早くコンパイル/実行/テストする"
    :custom (bloop-cli-command-name . "bloop")
    :bind (scala-mode-map
           ("C-c C-c" . bloop-do-compile)
           ("C-c RET" . bloop-do-runMain)
           ("C-c C-t" . bloop-do-testOnly)
           ("C-c C-k" . bloop-call-compile))
    (comint-mode-map ("C-c j" . bloop-find-file-other-window)))
  (leaf bloop-metals :after bloop lsp-metals
    :commands bloop-metals-query-analyze-stacktrace
    :bind (comint-mode-map ("C-c a" . bloop-metals-query-analyze-stacktrace)))
  (leaf sbt-mode :ensure t :require t
    :custom (sbt:program-name . "sbtn")))

(leaf java-mode :when (executable-find "mvn")
  :config
  (leaf lsp-java :ensure t
    :config (lsp-hook-activation-in-activated-workspace 'java-mode-hook 'jdtls)))

(leaf smtlib-mode :el-get kmn4/smtlib-mode :require t)

(leaf *cuda :mode ("\\.cu$" . c-mode))

(leaf rust-mode :when (executable-find "rustup") :ensure t
  :mode ("\\.rs$" . rust-mode)
  :config
  (leaf lsp-rust
    :custom
    (lsp-rust-server . 'rust-analyzer)
    ;; https://rust-analyzer.github.io/manual.html#rustup
    (lsp-rust-analyzer-server-command . '("rustup" "run" "stable" "rust-analyzer"))
    (lsp-hook-activation-in-activated-workspace 'rust-mode-hook 'rust-analyzer)))

(leaf racket-mode :when (executable-find "raco")
  :mode ("\\.rkt$" . racket-mode)
  :ensure t
  :config
  (leaf racket-xp
    :require t
    :hook (racket-mode-hook . racket-xp-mode)))

(leaf haskell-mode :when (executable-find "ghc")
  :ensure t
  :config (leaf lsp-haskell :ensure t))

(leaf web-mode :ensure t :mode ("\\.vue\\'")
  :custom
  (web-mode-markup-indent-offset . 2)
  (web-mode-css-indent-offset . 2)
  (web-mode-code-indent-offset . 2))
(leaf js-mode :custom (js-indent-level . 2))
(leaf css-mode :custom (css-indent-offset . 2))

(leaf latex :when (executable-find "tex")
  :ensure auctex
  :defvar TeX-command-list TeX-command-default
  :custom
  (TeX-default-mode . 'japanese-latex-mode)
  (TeX-view-program-selection . '((output-pdf "PDF Tools")))
  (LaTeX-electric-left-right-brace . t)
  :hook (LaTeX-mode-hook . TeX-source-correlate-mode)
  :config
  ;; これがあると end-of-defun でポイントがある一番内側の環境の終わりに移動する
  (advice-add 'LaTeX-find-matching-end :before #'forward-char)
  (prog1 "LaTeXMk を `TeX-command-list' に追加する。"
    (add-to-list 'TeX-command-list
                 '("LaTeXMk" "latexmk --synctex=1 %T" TeX-run-command nil t))
    (add-hook 'LaTeX-mode-hook (lambda () (setq TeX-command-default "LaTeXMk"))))
  (leaf pdf-sync :require t
    :bind (LaTeX-mode-map ("C-c g" . pdf-sync-forward-search)))
  (leaf reftex :diminish t :hook LaTeX-mode-hook
    :custom
    (reftex-toc-split-windows-fraction . 0.8)
    (reftex-label-alist
     . '(
         ("definition"  ?d  "def:"  "~\\ref{%s}" t (regexp ".*定義") -3)
         ("lemma"       ?l  "lem:"  "~\\ref{%s}" t (regexp ".*補題") -3)
         ("theorem"     ?t  "thm:"  "~\\ref{%s}" t (regexp ".*定理") -3)
         ("proposition" ?p  "prop:" "~\\ref{%s}" t (regexp ".*命題") -3)
         ("corollary"   ?c  "cor:"  "~\\ref{%s}" t (regexp ".*系")   -3)
         ("example"     ?e  "ex:"   "~\\ref{%s}" t (regexp ".*例")   -3)
         ("remark"      ?r  "remk:" "~\\ref{%s}" t (regexp ".*注意") -3)
         ))
    :config
    (leaf company-reftex :ensure t
      :config
      (add-to-list 'company-backends 'company-reftex-labels)
      (add-to-list 'company-backends 'company-reftex-citations))
    ;; よく C-c RET と間違えて C-c \ を叩くので…
    (put 'reftex-index-phrase-selection-or-word 'disabled t)
    )
  ) ; end of latex block

(leaf satysfi :when (executable-find "satysfi")
  :el-get gfngfn/satysfi.el :require t
  :mode (("\\.saty$" "\\.satyh$" "\\.satyg$") . satysfi-mode)
  :defun satysfi-find-pdf-other-window ; defined within this block
  :hook
  (satysfi-mode-hook . (lambda () (indent-tabs-mode -1)))
  (satysfi-mode-hook . (lambda () (display-line-numbers-mode +1)))
  :config
  (defun satysfi-find-pdf-other-window ()
    "Visit PDF for visiting SATySFi source in other window."
    (interactive)
    (let ((pdf-name (s-replace-regexp "\\.saty$" ".pdf" (buffer-file-name))))
      (find-file-other-window pdf-name)))
  (defalias 'satysfi-mode/open-pdf #'satysfi-find-pdf-other-window))

(leaf go-mode :when (executable-find "go") :ensure t)

(leaf cperl-mode :when *unix?
  :defvar cperl-mode-map
  :custom
  (cperl-indent-level . 4)
  (cperl-electric-backspace-untabify . nil)
  :config
  (add-to-list 'major-mode-remap-alist '(perl-mode . cperl-mode))
  (add-hook 'cperl-mode-hook #'indent-tabs-mode)
  (add-hook 'cperl-mode-hook (lambda () (setq tab-width 4)))
  :defer-config
  ;; smartparens との衝突を解決
  (define-key cperl-mode-map "{" nil)
  (define-key cperl-mode-map (kbd "C-h") #'cperl-electric-backspace))

;;;; 見た目

(leaf treemacs :ensure t
  :custom
  (treemacs-hide-gitignored-files-mode . t)
  :bind
  ("M-0" . #'treemacs)
  (treemacs-mode-map
   ("k" . #'treemacs-previous-line)
   ("j" . #'treemacs-next-line))
  (leader-map :package init ("0" . treemacs)))

(leaf nerd-icons :ensure t :require t)

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

(prog1 "フォント"
  (defun linux-install-hackgen ()
    "HackGen NF をインストールします。"
    (interactive)
    (unless (test-shell-command "fc-list | grep -i 'hackgen'")
      (if (not (test-shell-command "which unzip"))
          (message "install unzip")
        (run-shell-command "curl -sSLo /tmp/HackGen_NF_v2.9.0.zip https://github.com/yuru7/HackGen/releases/download/v2.9.0/HackGen_NF_v2.9.0.zip")
        (run-shell-command "unzip /tmp/HackGen_NF_v2.9.0.zip -d /tmp")
        (run-shell-command "mkdir -p ~/.local/share/fonts")
        (run-shell-command "cp /tmp/HackGen_NF_v2.9.0/*.ttf ~/.local/share/fonts")
        (run-shell-command "fc-cache -f")
        (if (test-shell-command "fc-list | grep -i 'hackgen'")
            (message "succeeded to install HackGen")
          (message "failed to install HackGen")))
      ))
  (when *wsl? (linux-install-hackgen))
  (defcustom default-face-family "HackGen Console NF"
    "`default' フェイスのファミリ。" :type 'string :group 'init)
  (defcustom default-face-height 110
    "`default' フェイスの高さ。" :type 'number :group 'init)
  (custom-set-faces
   `(default ((t (:family ,default-face-family :height ,default-face-height))))
   '(variable-pitch ((t (:family "Noto Sans JP"))))))

(prog1 "文字サイズ変更"
  (when (version< emacs-version "29")
    (defun default-face-font-height () (face-attribute 'default :height))
    (defun global-text-scale-increment ()
      (interactive)
      (set-face-attribute 'default nil
                          :height (+ (default-face-font-height) 5)))
    (defun global-text-scale-decrement ()
      (interactive)
      (set-face-attribute 'default nil
                          :height (- (default-face-font-height) 5))))
  (when (version>= emacs-version "29")
    (defun global-text-scale-increment ()
      (interactive)
      (global-text-scale-adjust 1))
    (defun global-text-scale-decrement ()
      (interactive)
      (global-text-scale-adjust -1)))
  (global-set-key (kbd "C-M-+") #'global-text-scale-increment)
  (global-set-key (kbd "C-M--") #'global-text-scale-decrement))

(leaf frame
  :custom
  (default-frame-alist . '((fullscreen . maximized)))
  `(menu-bar-mode . ,(if (eq system-type 'darwin) t nil))
  (tool-bar-mode . nil)
  :bind (leader-map :package init ("tm" . menu-bar-mode)))

(leaf centaur-tabs :ensure t :global-minor-mode centaur-tabs-mode
  :defun centaur-tabs-headline-match
  :bind
  (centaur-tabs-mode-map
   ("C-<next>" . centaur-tabs-forward)
   ("C-<prior>" . centaur-tabs-backward))
  (leader-map :package init ("tt" . centaur-tabs-mode))
  :hook
  ((imenu-list-major-mode-hook) . centaur-tabs-local-mode)
  :custom
  (centaur-tabs-set-icons . t)
  (centaur-tabs-set-modified-marker . t)
  :defer-config (centaur-tabs-headline-match))

(leaf whitespace :hook (conf-mode-hook . whitespace-mode))
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
(leaf page-break-lines :ensure t :global-minor-mode global-page-break-lines-mode)

(leaf *theme
  :ensure vscode-dark-plus-theme spacemacs-theme doom-themes shades-of-purple-theme
  :defvar
  ;; defined in ensured packages
  doom-themes-enable-bold doom-themes-enable-italic
  ;; defined within this block
  default-dark-theme default-light-theme
  :defun switch-theme in-light-theme ; defined within this block
  :setq
  (doom-themes-enable-bold . t)
  (doom-themes-enable-italic . t)
  :config
  (defvar favorite-themes
    '(
      shades-of-purple
      doom-dark+
      vscode-dark-plus
      doom-moonlight
      spacemacs-light
      ))
  (defun switch-theme (theme &optional no-confirm)
    "`favorite-themes' からテーマを選択して切り替える。"
    (interactive (list (intern (completing-read "Theme: " favorite-themes))))
    (mapc #'disable-theme custom-enabled-themes)
    (load-theme theme no-confirm))
  (defun toggle-theme ()
    "ライトテーマとダークテーマを切り替える。"
    (interactive)
    (switch-theme (if (in-light-theme) default-dark-theme default-light-theme)))
  (defcustom default-light-theme 'spacemacs-light "デフォルトのライトテーマ"
    :type 'symbol :group 'init)
  (defcustom default-dark-theme 'shades-of-purple "デフォルトのダークテーマ"
    :type 'symbol :group 'init)
  (defun in-light-theme ()
    "現在、ライトテーマが設定されている。"
    (eq (car custom-enabled-themes) default-light-theme))
  (switch-theme default-dark-theme t))

(leaf *mode-line
  ;; https://ayatakesi.github.io/emacs/28.1/html/Optional-Mode-Line.html#Optional-Mode-Line
  :global-minor-mode line-number-mode column-number-mode
  :custom
  (column-number-indicator-zero-based . nil)
  (mode-line-percent-position . nil)
  (mode-line-compact . nil)
  ;; TODO `swiper-migemo-mode' がオンならそのことを表示する
  :config
  (leaf doom-modeline :ensure t :global-minor-mode doom-modeline-mode
    :custom
    (doom-modeline-buffer-name              . nil) ; Centaur Tabs があるので必要ない
    (doom-modeline-buffer-modification-icon . nil) ; 同上
    (doom-modeline-major-mode-icon          . nil) ; 同上
    ))

(leaf dashboard :ensure t :require t
  :config
  (dashboard-setup-startup-hook)
  :bind
  (dashboard-mode-map ("n" . dashboard-next-line)
                      ("p" . dashboard-previous-line))
  (leader-map
   :package init
   ("fd" . (lambda () (interactive) (switch-to-buffer "*dashboard*"))))
  :custom
  (dashboard-startup-banner . 'logo)
  (dashboard-center-content . t)
  (dashboard-items . '((bookmarks . 10) (recents . 3)))
  (dashboard-footer-messages . '("Happy Coding!"))
  (initial-buffer-choice . (lambda () (get-buffer-create "*dashboard*"))))

(prog1 "いくつかの関数の 'disabled プロパティを `nil' にする。"
  (put 'narrow-to-region 'disabled nil)
  (put 'list-timers 'disabled nil)
  (put 'list-threads 'disabled nil)
  (put 'dired-find-alternate-file 'disabled nil)
  (put 'LaTeX-narrow-to-environment 'disabled nil)
  (defun disabled-function-list ()
    (let* ((disabled-syms . nil)
           (filt (lambda (sym)
                   (when (get sym 'disabled) (add-to-list 'disabled-syms sym)))))
      (mapatoms filt)
      disabled-syms)))

(leaf server
  :defun server-running-p
  :config (unless (server-running-p) (server-start)))

(report-time "init.el 読み込み完了")
(advice-remove 'leaf #'advice-leaf-report-time)

(provide 'init)

;; Local Variables:
;; indent-tabs-mode: nil
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:

;;; init.el ends here
