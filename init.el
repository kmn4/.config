(let ((default-directory (concat user-emacs-directory "site-lisp")))
  (push default-directory load-path)
  (normal-top-level-add-subdirs-to-load-path))

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("gnu" . "https://elpa.gnu.org/packages/")))
(package-refresh-contents t)
(package-initialize)

(package-install 'dash)
(package-install 's)
(require 'dash)
(require 's)

(defcustom +dropbox-root (substitute-env-vars "$HOME/Dropbox")
  "Dropbox sync root directory.")
(defun +dropbox-root (path) (s-lex-format "${+dropbox-root}/${path}"))

(defmacro add-hooks (fn &rest hooks) "Add FN to each of HOOKS."
          `(let ((fn ,fn))
             (dolist (hook ',hooks)
               (add-hook hook fn))))

(define-key global-map (kbd "C-h") 'backward-delete-char-untabify)
(define-key global-map (kbd "<f5>") 'revert-buffer)

(defun display-line-numbers-mode-on ()
  (interactive) (display-line-numbers-mode +1))
(add-hooks #'display-line-numbers-mode-on
	   prog-mode-hook org-mode-hook LaTeX-mode-hook)

(add-hooks (lambda ()
             (when (derived-mode-p 'prog-mode 'tex-mode)
               (delete-trailing-whitespace)))
           before-save-hook)

(defconst source-code-pro "源ノ角ゴシック Code JP-13")
(defvar default-font-name source-code-pro)
(defun set-font (name)
  (if-let ((spec (font-spec :name name))
	   (font (find-font spec)))
      (progn (set-frame-font font nil t)
	     (setf (alist-get 'font default-frame-alist) name))
    (message "Font not found: %s" name)))
(set-font default-font-name)

(with-eval-after-load 'info
  (setq Info-directory-list (-union Info-directory-list Info-default-directory-list))
  (push (substitute-env-vars "$XDG_DATA_HOME/info") Info-directory-list) ; TODO
  )

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

(set-language-environment "Japanese")
(prefer-coding-system 'utf-8)
(package-install 'migemo)
(require 'migemo)
(require 'avy-migemo)
(setq migemo-dictionary "/usr/share/cmigemo/utf-8/migemo-dict")
(require 'avy-migemo-e.g.ivy)
(require 'avy-migemo-e.g.counsel)
(require 'avy-migemo-e.g.swiper)
(avy-migemo-mode +1)

(package-install 'lsp-mode)
(package-install 'lsp-ui)

(package-install 'lsp-metals)
(when (executable-find "metals-emacs")
  (setq lsp-metals-server-command "metals-emacs"))

(package-install 'auctex)
(package-install 'magic-latex-buffer)
(add-hook 'LaTeX-mode-hook
	  (lambda ()
	    (magic-latex-buffer +1)
	    (reftex-mode +1)))
(with-eval-after-load 'tex
  (require 'reftex)
  (require 'bibtex)
  (setq TeX-command-list (-remove (lambda (l) (string-equal (car l) "LaTeX")) TeX-command-list))
  (push '("LaTeX" "latexmk --synctex=1 %T" TeX-run-command nil t) TeX-command-list)
  (setq reftex-default-bibliography `(,(+dropbox-root "lab/bib/ref.bib")) ; TODO
        bibtex-files '(bibtex-file-path))
  (setq reftex-label-alist
        '(("definition" ?d "def:" "~\\ref{%s}" t ("definition" "def.") -3)
          ("lemma" ?m "lem:" "~\\ref{%s}" t ("lemma" "lem.") -3)
          ("theorem" ?h "thm:" "~\\ref{%s}" t   ("theorem" "th.") -3)))
  )


(require 'server)
(unless server-process (server-start))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-view-program-selection
   '(((output-dvi has-no-display-manager)
      "dvi2tty")
     ((output-dvi style-pstricks)
      "dvips and gv")
     (output-dvi "xdvi")
     (output-pdf "Okular")
     (output-html "xdg-open")))
 '(avy-migemo-function-names
   '((counsel-clj :around avy-migemo-disable-around)
     (counsel-grep :around counsel-grep-migemo-around)
     counsel-grep-function-migemo counsel-grep-occur-migemo
     (counsel-git-occur :around counsel-git-occur-migemo-around)
     (counsel-find-file-occur :around counsel-find-file-occur-migemo-around)
     swiper--make-overlays-migemo
     (swiper--re-builder :around swiper--re-builder-migemo-around)
     (ivy--regex :around ivy--regex-migemo-around)
     (ivy--regex-ignore-order :around ivy--regex-ignore-order-migemo-around)
     (ivy--regex-plus :around ivy--regex-plus-migemo-around)
     ivy--highlight-default-migemo ivy-occur-revert-buffer-migemo ivy-occur-press-migemo avy-migemo-goto-char avy-migemo-goto-char-2 avy-migemo-goto-char-in-line avy-migemo-goto-char-timer avy-migemo-goto-subword-1 avy-migemo-goto-word-1 avy-migemo-isearch avy-migemo-org-goto-heading-timer avy-migemo--overlay-at avy-migemo--overlay-at-full))
 '(custom-enabled-themes '(wombat))
 '(package-selected-packages
   '(avy-migemo migemo git-gutter+ magit s dash magic-latex-buffer auctex undo-tree redo+ undo+ winum counsel ivy which-key lsp-ui lsp-mode)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
