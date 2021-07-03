(setq user-emacs-directory (expand-file-name (file-name-directory load-file-name)))

(let ((default-directory (concat user-emacs-directory "site-lisp")))
  (push default-directory load-path)
  (normal-top-level-add-subdirs-to-load-path))

(setq custom-file (concat user-emacs-directory "emacs-custom-settings.el"))
(load custom-file)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("gnu" . "https://elpa.gnu.org/packages/")))
(package-refresh-contents t)
(package-initialize)

(package-install 'dash)
(package-install 's)
(require 'dash)
(require 's)

(defconst unix? (memq system-type '(gnu/linux darwin)))

(defcustom +dropbox-root (substitute-env-vars "$HOME/Dropbox")
  "Dropbox sync root directory.")
(defun +dropbox-root (path) (s-lex-format "${+dropbox-root}/${path}"))

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
        org-agenda-span 'month))

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

(defconst leader-map (make-sparse-keymap) "My keymap.")
(defun set-leader-map (key def &rest bindings)
  "Add KEY and DEF to my keymap."
  (while key
    (define-key leader-map (kbd key) def)
    (setq key (pop bindings) def (pop bindings))))

(global-set-key (kbd "<henkan>") leader-map)
(define-key global-map (kbd "C-h") 'backward-delete-char-untabify)
(define-key global-map (kbd "<f5>") 'revert-buffer)
(set-leader-map
 "fr" #'counsel-recentf
 "sg" #'counsel-git-grep
 "pf" #'project-find-file
 "jf" #'find-function
 "jl" #'find-library
 "l" #'lsp
 "oa" #'org-agenda
 "gb" #'magit-blame)

(require 'server)
(unless (server-running-p) (server-start))

;; Local Variables:
;; indent-tabs-mode: nil
;; End:
