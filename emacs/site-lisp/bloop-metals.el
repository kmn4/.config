;;; bloop-metals.el --- jump to the code from the stack trace of `bloop run`.

;;; Commentary:

;; M-x `bloop-metals-query-analyze-stacktrace' で
;; スタックトレースの解析を Metals に投げる。
;; C-i で次のリンク、C-M-i で前のリンクへジャンプ。

;; TODO:
;; - `bloop-do-runMain' が失敗したら自動的にスタックトレースを解析する。
;; -

;;; Code:

(require 'bloop)
(require 'lsp-metals)

(defconst bloop-stacktrace-regex
  "\\(^\\[E\\] Exception in thread [[:ascii:][:nonascii:]]*\\)Comint")

(defvar bloop-metals-compilation-buffer nil)
(defvar bloop-metals-stacktrace-range nil)

(defun bloop-stacktrace-range ()
  (save-excursion
    (goto-char (point-min))
    (search-forward-regexp bloop-stacktrace-regex nil t)
    (cons (match-beginning 1) (match-end 1))))

;;;###autoload
(defun bloop-metals-query-analyze-stacktrace ()
  (interactive)
  (save-excursion
    (let ((range (bloop-stacktrace-range)))
      ;; HACK: `lsp-metals-analyze-stacktrace' がリージョンを使うので
      ;;       `set-mark' を呼んでいるが、本来この関数は Lisp から呼ぶべきでない。
      (set-mark (car range))
      (goto-char (cdr range))
      (setq bloop-metals-compilation-buffer (current-buffer))
      (setq bloop-metals-stacktrace-range range))
    (lsp-metals-analyze-stacktrace)))

(defun bloop-metals-render-stacktrace-analysis (_workspace html)
  (save-excursion
    (with-current-buffer bloop-metals-compilation-buffer
      (let ((beg (car bloop-metals-stacktrace-range))
            (end (cdr bloop-metals-stacktrace-range)))
        (setq-local browse-url-handlers '(("\\`command:" . lsp-metals--browse-url)))
        (delete-region beg end)
        (insert html)
        (shr-render-region beg (+ beg (length html)))))))

(defun bloop-metals-analyze-stacktrace-advice (oldfn _workspace html)
  (if bloop-metals-compilation-buffer
      (bloop-metals-render-stacktrace-analysis nil html)
    (funcall oldfn _workspace html)))

(advice-add 'lsp-metals--show-stacktrace :around
            #'bloop-metals-analyze-stacktrace-advice)

(provide 'bloop-metals)

;;; bloop-metals.el ends here

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:
