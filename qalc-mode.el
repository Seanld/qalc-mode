(defvar qalc-mode-hook nil)

(defun qalc--eval-expression (entry-string)
  "Returns string result from evaluating a Qalc expression."
  (shell-command-to-string (format "qalc -t %s" entry-string)))

(defun qalc--go-to-line-type (line-type reverse)
  "Goes to the next (or stays on it if currently on one) line of type
`line-type' which is any of:
* 0 for an empty line
* 1 for a comment/description line
* 2 for a result line
* 3 for an entry line
And `reverse' can be:
* nil to search forward, or
* t to search backward"
  (let ((empty-line 0)
        (commented-line 1)
        (result-line 2)
        (entry-line 3)
        (move-direction (if (eq reverse nil)
                            1
                          -1))
        (current-line '(buffer-substring-no-properties (line-beginning-position)
                                                      (line-end-position))))
    (while (not (= line-type (cond ((equal (buffer-substring-no-properties (line-beginning-position)
                                                                           (line-end-position))
                                           "")
                                    empty-line)
                                   ((equal (substring (buffer-substring-no-properties (line-beginning-position)
                                                                                      (line-end-position))
                                                      0 1)
                                           "#")
                                    commented-line)
                                   ((equal (substring (buffer-substring-no-properties (line-beginning-position)
                                                                                      (line-end-position))
                                                      0 3)
                                           "==>")
                                    result-line)
                                   (t
                                    entry-line))))
      (forward-line move-direction))))

(defun qalc-eval-entry ()
  "Evaluate an entry in the file, and add the result to the end of it."
  (interactive)
  )

(defvar qalc-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") 'qalc-eval-entry)
    map))

(add-to-list 'auto-mode-alist '("\\.qalc\\'" . qalc-mode))

(defvar qalc-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?_ "w" st)
    (modify-syntax-entry ?# "< b" st)
    (modify-syntax-entry ?\n "> b" st)
    st))

(defvar qalc-highlights
  `((,(rx line-start "==>" (0+ anything)) . font-lock-warning-face)
    (,(rx (1+ punct)) . font-lock-constant-face)))

(define-derived-mode qalc-mode text-mode ()
  "Major mode for interactively evaluating qalculate entries."
  (setq major-mode 'qalc-mode)
  (setq mode-name "Qalc")
  (setq font-lock-defaults '(qalc-highlights))
  (run-hooks 'qalc-mode-hook))

;; Parse libqalculate unit XML file.
(defvar qalc-units-xml
      (with-temp-buffer
        (insert-file-contents "/usr/share/qalculate/units.xml")
        (libxml-parse-xml-region (point-min) (point-max))))

;; (message (dom-by-tag qalc-units-xml 'names))

;; (provide 'qalc-mode)
