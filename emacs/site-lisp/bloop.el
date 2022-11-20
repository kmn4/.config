;;; bloop.el --- compile, run and test Scala project within Emacs

;;; Commentary:

;;; Code:

(require 'comint)
(require 's)
(require 'seq)

(defcustom bloop-cli-command-name "bloop"
  "bloop CLI のコマンド名。"
  :type 'string :group 'bloop)

(defun nearest-directory-containing (file)
  "FILE を含んでいる直近の祖先を返す。見つからなかったらエラー。"
  (cl-labels
      ((root-directory (dir) (equal dir "/"))
       (search () (cond ((file-exists-p file) default-directory)
			   ((root-directory default-directory)
			    (error "%s was not found" file))
			   (t (cd "..") (search)))))
    (with-temp-buffer (search))))

(defun bloop-find-root ()
  ".bloop/ を含んでいる最も近い祖先を返す。"
  (nearest-directory-containing ".bloop/"))

;;;###autoload
(defun bloop-call-compile (command)
  "bloop コマンド COMMAND を `compile' で呼び出す。"
  (interactive "sbloop: ")
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
    (completing-read prompt collection)))

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
	    (bloop-select-test-project)))))

(defun bloop--select-main-class ()
  (completing-read
   "main class: "
   (bloop--command-to-lines
    (concat "autocomplete --mode mainsfqcn --format bash --project "
	    (bloop-select-main-project)))))

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
    (search-forward-regexp "(\\(.*\.scala\\):\\([[:digit:]]+\\)" (forward-line-point))
    (cons (buffer-substring (match-beginning 1) (match-end 1))
          (string-to-number
           (buffer-substring (match-beginning 2) (match-end 2))))
    ))

;;;###autoload
(defun bloop-find-file-other-window ()
  (interactive)
  (let* ((fname-line (enclosed-filename-linum-pair))
         (filepath (projectile-completing-read "Find file: " (projectile-project-files (projectile-acquire-root)) :initial-input (car fname-line))))
    (find-file-other-window filepath)
    (goto-line (cdr fname-line))))

(provide 'bloop)
;;; bloop.el ends here

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:
