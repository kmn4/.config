;;; bloop.el --- compile, run and test Scala project within Emacs

;;; Commentary:

;;; Code:

(require 'comint)
(require 's)
(require 'seq)
(require 'project)

(defcustom bloop-cli-command-name "bloop"
  "bloop CLI のコマンド名。"
  :type 'string :group 'bloop)

(defun bloop-find-root ()
  ".bloop/ を含んでいる最も近い祖先を返す。"
  (locate-dominating-file default-directory ".bloop/"))

(defvar bloop-command-history nil)
(defvar bloop-main-history nil)
(defvar bloop-test-history nil)

;;;###autoload
(defun bloop-call-compile (command)
  "bloop コマンド COMMAND を `compile' で呼び出す。"
  (interactive (list (read-from-minibuffer "bloop: " nil nil nil 'bloop-command-history)))
  (let ((default-directory (bloop-find-root)))
    (compile (concat bloop-cli-command-name " " command) t)))

(defun bloop--command-to-string (command)
  (let ((default-directory (bloop-find-root)))
    (shell-command-to-string (concat bloop-cli-command-name " " command))))

(defun bloop--command-to-lines (command)
  (let* ((output (bloop--command-to-string command))
	 (trimmed (s-trim output))
	 (lines (s-split "\n" trimmed)))
    lines))

(defun bloop-projects ()
  (bloop--command-to-lines "projects"))

(defun bloop-select-project ()
  (completing-read "Bloop project: " (bloop-projects)))

(defun bloop--test-project-p (p) (s-ends-with? "-test" p))
(defun bloop--main-project-p (p) (null (bloop--test-project-p p)))

(defun bloop--test-projects () (seq-filter #'bloop--test-project-p (bloop-projects)))
(defun bloop--main-projects () (seq-filter #'bloop--main-project-p (bloop-projects)))

(defun completing-read-if-multiple (prompt collection)
  (if (length= collection 1)
      (car collection)
    (completing-read prompt collection nil t)))

(defun bloop-select-main-project ()
  (interactive)
  (completing-read-if-multiple "Bloop project (main): " (bloop--main-projects)))

(defun bloop-select-test-project ()
  (interactive)
  (completing-read-if-multiple "Bloop project (test): " (bloop--test-projects)))

(defun bloop-do-test ()
  (interactive)
  (let ((project (bloop-select-test-project)))
    (bloop-call-compile (concat "test -p " project))))

(defun bloop--select-test-class ()
  (completing-read
   "test class: "
   (bloop--command-to-lines
    (concat "autocomplete --mode testsfqcn --format bash --project "
	    (bloop-select-test-project)))
   nil t nil 'bloop-test-history))

(defun bloop--select-main-class ()
  (completing-read
   "main class: "
   (bloop--command-to-lines
    (concat "autocomplete --mode mainsfqcn --format bash --project "
	    (bloop-select-main-project)))
   nil t nil 'bloop-main-history))

;;;###autoload
(defun bloop-do-compile ()
  (interactive)
  (bloop-call-compile
   (concat "compile" " -p " (bloop-select-main-project))))

;;;###autoload
(defun bloop-do-testOnly ()
  (interactive)
  (let* ((project (bloop-select-test-project)))
    (bloop-call-compile
     (concat "test"
	     " -p " project
	     " -o " (bloop--select-test-class)))))

;;;###autoload
(defun bloop-do-runMain ()
  (interactive)
  (let* ((project (bloop-select-main-project)))
    (bloop-call-compile
     (concat "run"
	     " -p " project
	     " -m " (bloop--select-main-class)))))

(defun enclosed-filename-linum-pair ()
  (save-excursion
    (beginning-of-line)
    (let ((line (buffer-substring
                 (line-beginning-position)
                 (line-end-position)))
          (regexp ".*(\\(.*\.scala\\):\\([[:digit:]]+\\))"))
      (save-match-data
        (and (string-match regexp line)
             (cons (match-string 1 line)
                   (string-to-number (match-string 2 line))))))))

;;;###autoload
(defun bloop-find-file-other-window ()
  (interactive)
  (if-let* ((fname-line (enclosed-filename-linum-pair))
            (files (project-files (project-current)))
            (filepath (completing-read "Find file: " files
                                       nil t (car fname-line))))
      (progn
        (find-file-other-window filepath)
        (goto-char (point-min))
        (forward-line (1- (cdr fname-line))))
    (user-error "No Scala file found on this line.")))

(provide 'bloop)
;;; bloop.el ends here

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:
