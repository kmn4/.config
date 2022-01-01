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

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
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
 ;; [c]ustomize
 "cv" #'customize-variable-other-window
 "cg" #'customize-group-other-window
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
 "sm" #'global-swiper-migemo-mode
 ;; [i]nsert
 "i0" #'insert-at-beginnings-in-region
 ;; [j]ump
 "jf" #'find-function
 "jl" #'find-library
 ;; [l]sp
 "l" #'lsp
 ;; [o]rg mode
 "oa" #'org-agenda
 ;; [w]indow
 "wt" #'toggle-window-split)

(leaf util
  :ensure dash s f
  :require dash s f)

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

;; 出典: https://stackoverflow.com/a/33456622
(defun toggle-window-split ()
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

(defun forward-line-point (&optional n) (save-excursion (forward-line n) (point)))

;; TODO もとからある空白はそのままにする？
(defun join-line-without-spaces ()
  (interactive)
  (save-excursion
    (call-interactively #'join-line)
    (while (search-forward " " (forward-line-point) t) (replace-match ""))))

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

(defconst linux? (eq system-type 'gnu/linux))
(defconst macos? (eq system-type 'darwin))
(defconst unix? (or linux? macos?))
(defconst wsl? (and unix? (s-contains-p "WSL2" (shell-command-output "uname -a"))))

;; ブラウザ
;; 参考: https://www.iplab.cs.tsukuba.ac.jp/~takakura/blog/20200715/
(defun browser-function--wsl (url &rest _)
  (just-run-shell-command (format "cmd.exe /c start %s" url)))
(when wsl? (customize-set-variable 'browse-url-browser-function #'browser-function--wsl))

;; 以下は "NOT part of Emacs" なパッケージも使う

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

(leaf restart-emacs :ensure t)

(leaf paradox :ensure t
  :custom (paradox-github-token . t))

(leaf google-translate :ensure t popup
  :config
  ;; 出典: https://github.com/atykhonov/google-translate/issues/137#issuecomment-723938431
  (defun google-translate--search-tkk () "Search TKK." (list 430675 2721866130)))

(leaf move-text :ensure t
  :config (move-text-default-bindings))

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
  (defun set-font (&optional new-default)
    "フォントを `default-font-name' に設定する。

NEW-DEFAULT が非 nil のときは、現在のセッションに限りこれを新たなデフォルトとする。"
    (when new-default (setq default-font-name new-default))
    (set-frame-font default-font-name nil t))
  (if (daemonp) (add-hook 'server-after-make-frame-hook #'set-font)
    (add-hook 'after-init-hook #'set-font)))

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

(leaf project
  :custom (project-vc-merge-submodules . nil)  ; サブモジュールは別プロジェクト扱い
  :config
  (defun project-counsel-rg ()
    (interactive)
    (counsel-rg nil (project-root (project-current t))))
  :bind (project-prefix-map ("C-s" . project-counsel-rg)))

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
  :config (set-leader-map "tm" #'minimap-mode))

(leaf undo-tree
  :ensure t
  :config (global-undo-tree-mode +1))

(leaf hl-todo :ensure t
  :hook prog-mode-hook org-mode-hook
  :config (add-to-list 'hl-todo-keyword-faces (cons "WARN" "#ff0000")))

(leaf ivy
  :ensure t swiper counsel ivy-hydra
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

;; `completion-at-point' を直接利用する場合と比べて、補完中にドキュメントを読めることが company の利点。
;; 独自 UI よりも `counsel-company' ほうが候補の絞り込みに便利だが、後者ではドキュメント表示ができないので我慢。
(leaf company :ensure t :require t ; require しないと `global-company-mode' が呼ばれない
  :custom
  ;; `company-posframe' があれば `company-echo-metadata-frontend' は不要
  (company-frontends . '(company-pseudo-tooltip-unless-just-one-frontend company-preview-if-just-one-frontend))
  (company-idle-delay . 0)
  :bind
  (company-mode-map ([remap completion-at-point] . company-complete))
  (company-active-map ("C-h" . backward-delete-char-untabify)
                      ("M-<" . #'company-select-first)
                      ("M->" . #'company-select-last))
  :config (global-company-mode +1)
  (leaf company-posframe :ensure t
    ;; *Help* が汚染されるのでドキュメントは手動 (<f1>キー) で開く
    :custom (company-posframe-quickhelp-delay . nil)
    :config (company-posframe-mode +1)))

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
  (setq migemo-options '("--quiet" "--nonewline" "--emacs")))

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

(leaf flymake
  :config
  (set-leader-map
   "en" #'flymake-goto-next-error
   "ep" #'flymake-goto-prev-error
   "el" #'flymake-show-buffer-diagnostics
   "pel" #'flymake-show-project-diagnostics))

(leaf lsp
  :ensure lsp-mode lsp-ui
  :custom
  `(lsp-keymap-prefix . ,(concat leader-key " l"))
  :config
  ;; 既に LSP サーバが既に走っているプロジェクトのファイルを開くときは自動的に接続。
  ;; ただし少しだけファイルを開きたいときにいちいち LSP サーバが起動すると鬱陶しいので、
  ;; LSP を使いたいプロジェクトでは初めに明示的に `lsp' を呼び出す。
  ;; 出典: https://github.com/kurnevsky/dotfiles/blob/c0049a655a502cd81f1aba7321ff65d178a557c9/.emacs.d/init.el#L1231-L1237
  (defun lsp-activate-if-already-activated (server-id)
    (when (lsp-find-workspace server-id (buffer-file-name)) (lsp)))
  (add-hook 'scala-mode-hook (lambda () (lsp-activate-if-already-activated 'metals))))

;; Scala

(leaf lsp-metals :when (executable-find "scala")
  :ensure t
  :config
  (when (executable-find "metals-emacs")
    (setq lsp-metals-server-command "metals-emacs")))

;; SMT-LIB

(leaf smtlib-mode :require t)

;; Racket

(leaf racket-mode :when (executable-find "raco")
  :ensure t
  :config
  (leaf racket-xp
    :require t
    :hook (racket-mode-hook . racket-xp-mode)))

;; TeX

(defun tex-installed-p () (executable-find "tex"))

(leaf auctex :when (tex-installed-p)
  :ensure t
  :custom
  (LaTeX-electric-left-right-brace . t)
  :hook
  (LaTeX-mode-hook . (lambda () (reftex-mode +1)))
  :config
  (setq TeX-command-list (-remove (lambda (l) (string-equal (car l) "LaTeX")) TeX-command-list))
  (push '("LaTeX" "latexmk --synctex=1 %T" TeX-run-command nil t) TeX-command-list)

  (leaf magic-latex-buffer :when (tex-installed-p)
    :ensure t
    :after auctex
    :custom
    (magic-latex-enable-block-align . nil)
    (magic-latex-enable-inline-image . nil)
    :hook (LaTeX-mode-hook . (lambda () (when (display-graphic-p) (magic-latex-buffer +1)))))
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

;; TODO
;; (leaf projectile :ensure t)
;; (leaf flycheck :ensure t)
;; (leaf all-the-icon :ensure t)
;; TODO モードライン
;; - SWM (swiper-migemo-mode) を目立たせる
;; - 常時オンなモードは表示しない

;; Local Variables:
;; indent-tabs-mode: nil
;; End:
