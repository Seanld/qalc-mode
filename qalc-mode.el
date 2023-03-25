(defvar qalc-mode-hook nil)

(defun qalc-eval-entry ()
  "Evaluate an entries in the file, and add the result to the end."
  (interactive)
  (message "Evaluated"))

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
  '(((rx (one-or-more punct)) . font-lock-constant-face)))

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
