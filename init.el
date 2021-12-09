(setq user-emacs-directory (expand-file-name (file-name-directory load-file-name)))

(let ((default-directory (concat user-emacs-directory "site-lisp")))
  (push default-directory load-path)
  (normal-top-level-add-subdirs-to-load-path))

(setq custom-file (concat user-emacs-directory "emacs-custom-settings.el"))
(unless (file-exists-p custom-file)
  (with-temp-buffer (write-file custom-file)))
(load custom-file)

(defun visit-init-file ()
  "Visit init.el."
  (interactive)
  (find-file user-init-file))

;; macOS で pakcage-refresh-contents が失敗するのを直す．
;; 出典: https://emacs.stackexchange.com/a/68568
(when (and (equal emacs-version "27.2")
           (eql system-type 'darwin))
  (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("gnu" . "https://elpa.gnu.org/packages/")))
(package-refresh-contents (boundp 'package-selected-packages))
(package-initialize)

(package-install 'dash)
(package-install 's)
(require 'dash)
(require 's)

(unless (eq system-type 'windows-nt)
  (package-install 'exec-path-from-shell)
  (exec-path-from-shell-initialize))
(setenv "LANG" "ja_JP.utf-8")

(defun shell-command-output (command)
  "COMMAND を実行し、その標準出力を文字列として返す。"
  (with-temp-buffer
    (shell-command command (current-buffer))
    (buffer-string)))
(defconst unix? (memq system-type '(gnu/linux darwin)))
(defconst macos? (eq system-type 'darwin))
(defconst wsl? (and unix? (s-contains-p "WSL2" (shell-command-output "uname -a"))))

(defcustom +dropbox-root (substitute-env-vars "$HOME/Dropbox")
  "Dropbox sync root directory.")
(defun +dropbox-root (path) (s-lex-format "${+dropbox-root}/${path}"))

(setq confirm-kill-emacs #'yes-or-no-p)

(defmacro add-hooks (fn &rest hooks) "Add FN to each of HOOKS."
          `(let ((fn ,fn))
             (dolist (hook ',hooks)
               (add-hook hook fn))))

(defun display-line-numbers-mode-on ()
  (interactive) (display-line-numbers-mode +1))
(add-hooks #'display-line-numbers-mode-on
	   prog-mode-hook org-mode-hook LaTeX-mode-hook)

(add-hooks (lambda ()
             (when (derived-mode-p 'prog-mode 'tex-mode)
               (delete-trailing-whitespace)))
           before-save-hook)

(defconst source-code-pro "Source Han Code JP-13")
(defcustom default-font-name source-code-pro "Default font name.")
(defun set-font (name)
  (if-let ((spec (font-spec :name name))
	   (font (find-font spec)))
      (progn (set-frame-font name nil t)
	     (setf (alist-get 'font default-frame-alist) name))
    (message "Font not found: %s" name)))
(set-font default-font-name)

(with-eval-after-load 'info
  (setq Info-directory-list (-union Info-directory-list Info-default-directory-list))
  (push (substitute-env-vars "$XDG_DATA_HOME/info") Info-directory-list) ; TODO
  )

(add-hooks (lambda () (electric-pair-local-mode +1))
           prog-mode-hook)
(add-hooks (lambda () (show-paren-mode +1))
           prog-mode-hook)
(add-hooks (lambda () (hs-minor-mode +1))
           prog-mode-hook)

(setq recentf-max-saved-items 1000)
(recentf-mode +1)

(tool-bar-mode -1)

(package-install 'undo-tree)
(global-undo-tree-mode +1)

(package-install 'ivy)
(package-install 'swiper)
(package-install 'counsel)
(define-key global-map (kbd "C-s") 'swiper)
(ivy-mode +1)
(counsel-mode +1)

(package-install 'winum)
(winum-mode +1)
(let ((digit 0))
  (while (<= digit 9)
    (define-key global-map (kbd (format "M-%d" digit))
      (intern (format "winum-select-window-%d" digit)))
    (setq digit (1+ digit))
    ))

(package-install 'which-key)
(which-key-mode +1)

(package-install 'magit)
(package-install 'git-gutter+)
(global-git-gutter+-mode +1)
(defmacro in-all-buffers-where (pred &rest body)
  "Do BODY in all buffers where PRED evaluates to t."
  `(dolist (buf (buffer-list))
     (with-current-buffer buf
       (when (funcall ,pred) ,@body))))

(defun git-gutter+-refresh-all-buffers ()
  (interactive)
  (in-all-buffers-where (-const git-gutter+-mode) (git-gutter+-refresh)))

(package-install 'rg)

(package-install 'yasnippet)
(yas-global-mode)

(when unix? (package-install 'fish-mode))

(set-language-environment "Japanese")
(prefer-coding-system 'utf-8)
;; Ubuntu  -- apt install cmigemo
;; Windows -- https://www.kaoriya.net/software/cmigemo/
;; M-x customize-variable migemo-dictionary
(package-install 'migemo)
(require 'migemo)
(package-install 'avy)
(require 'avy-migemo)
(require 'avy-migemo-e.g.ivy)
(require 'avy-migemo-e.g.counsel)
(require 'avy-migemo-e.g.swiper)
(avy-migemo-mode +1)

;; 入力メソッドの設定。
(package-install 'agda2-mode)
(require 'agda2-mode)
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
(when wsl?
  (package-install 'mozc)
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
  (global-set-key (kbd "C-\\") #'im-C-backslash)
  (define-key mozc-mode-map (kbd "C-\\") #'im-C-backslash)
  (global-set-key (kbd "<C-henkan>") #'im-C-henkan)
  (global-set-key (kbd "<C-muhenkan>") #'im-C-muhenkan)
  )

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

(package-install 'lsp-mode)
(package-install 'lsp-ui)

(package-install 'lsp-metals)
(when (executable-find "metals-emacs")
  (setq lsp-metals-server-command "metals-emacs"))

(package-install 'auctex)
(package-install 'magic-latex-buffer)
(setq magic-latex-enable-block-align nil
      magic-latex-enable-inline-image nil)
(add-hook 'LaTeX-mode-hook (lambda () (when (graphical?) (magic-latex-buffer +1))))
(defun graphical? () (null (eq (framep (selected-frame)) t)))
(add-hook 'LaTeX-mode-hook (lambda () (reftex-mode +1)))
(with-eval-after-load 'tex
  (require 'reftex)
  (require 'bibtex)
  (setq LaTeX-electric-left-right-brace t)
  (setq TeX-command-list (-remove (lambda (l) (string-equal (car l) "LaTeX")) TeX-command-list))
  (push '("LaTeX" "latexmk --synctex=1 %T" TeX-run-command nil t) TeX-command-list)
  (setq reftex-default-bibliography `(,(+dropbox-root "lab/bib/ref.bib")) ; TODO
        bibtex-files '(bibtex-file-path))
  (setq reftex-label-alist
        '(("definition" ?d "def:" "~\\ref{%s}" t ("definition" "def.") -3)
          ("lemma" ?m "lem:" "~\\ref{%s}" t ("lemma" "lem.") -3)
          ("theorem" ?h "thm:" "~\\ref{%s}" t   ("theorem" "th.") -3)))
  )

(with-eval-after-load 'org
  (define-key org-mode-map (kbd "M-p") #'org-move-subtree-up)
  (define-key org-mode-map (kbd "M-n") #'org-move-subtree-down)
  (setq org-agenda-files (list (+dropbox-root "org"))
        org-agenda-span 'month)
  (setq org-export-use-babel nil
        org-export-with-broken-links t))

(package-install 'dockerfile-mode)

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
 "sr" #'counsel-rg-migemo
 ;; [p]roject-aware commands
 "pf" #'project-find-file
 ;; [j]ump
 "jf" #'find-function
 "jl" #'find-library
 ;; [l]sp
 "l" #'lsp
 ;; [o]rg mode
 "oa" #'org-agenda)

(require 'server)
(unless (server-running-p) (server-start))

;; Local Variables:
;; indent-tabs-mode: nil
;; End:
