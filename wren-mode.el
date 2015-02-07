(defconst wren-keywords:data
  '("class" "var" "new")
  "wren keywords for data")

(defconst wren-keywords:control-flow
  '("if" "else" "while" "for" "return")
  "wren keywords for control flow")

(defconst wren-keywords:op:logic
  '("and" "or" "not" "is")
  "wren keywords for control flow")

(defcustom wren-tab-width tab-width
  "The tab width to use when indenting."
  :type 'integer
  :group 'wren
  :safe 'integerp)

(defvar wren-this-regexp "_\\w+")
(defvar wren-super-regexp "\\<super\\>")
;; (defvar wren-defun-regexp "\\w+\\( \\|\t\\){")


(defvar wren-font-lock-keywords
  (let ((beg "\\<")
        (end "\\>"))
    (list
     (cons (concat beg (regexp-opt wren-keywords:data t) end)
           font-lock-keyword-face)
     (cons (concat beg (regexp-opt wren-keywords:control-flow t) end)
           font-lock-keyword-face)
     (cons (concat beg (regexp-opt wren-keywords:op:logic t) end)
           font-lock-keyword-face)
     (cons wren-this-regexp font-lock-variable-name-face)
     (cons wren-super-regexp font-lock-variable-name-face)))
  "wren keywords highlighting")


(defun wren-comment-or-string-p (&optional pos)
  "Returns true if the point is in a comment or string."
  (save-excursion (let ((parse-result (syntax-ppss pos)))
                    (or (elt parse-result 3) (elt parse-result 4)))))


(defun wren-previous-indent ()
  "Return the indentation level of the previous non-blank line."
  (save-excursion
    (wren-goto-preivous-nonblank-line)
    (current-indentation)))


(defun wren-goto-preivous-nonblank-line ()
  (forward-line -1)
  (while (and (looking-at "^[ \t]*$") (not (bobp)))
    (forward-line -1)))


(defun wren-indent-to (x)
  (when x
    (let (shift top beg)
      (and (< x 0) (error "invalid nest"))
      (setq shift (current-column))
      (beginning-of-line)
      (setq beg (point))
      (back-to-indentation)
      (setq top (current-column))
      (skip-chars-backward " \t")
      (if (>= shift top) (setq shift (- shift top))
        (setq shift 0))
      (if (and (bolp)
               (= x top))
          (move-to-column (+ x shift))
        (move-to-column top)
        (delete-region beg (point))
        (beginning-of-line)
        (indent-to x)
        (move-to-column (+ x shift))))))


;;;###autoload
(defun wren-calculate-indent ()
  (interactive)

  (let* ((pos (point))
         (line (line-number-at-pos pos))
         (closing-p (save-excursion
                      (beginning-of-line)
                      (skip-chars-forward " \t")
                      (looking-at "[]})]"))))

    (save-excursion
      (wren-goto-preivous-nonblank-line)
      (end-of-line)
      (skip-chars-backward " \t")
      (forward-char -1)

      (cond
       ((or (looking-at "^[ \t]*$")
            (= line (line-number-at-pos (point))))
        0)

       ;; TODO: /* */
       ((wren-comment-or-string-p pos)
        (current-indentation))

       (closing-p
        (if (looking-at "[\\[{(]")
            (current-indentation)
          (- (current-indentation) wren-tab-width)))

       ((looking-at "[\\[{(]")
        (+ (current-indentation) wren-tab-width))

       (t (current-indentation))))))


;;;###autoload
(defun wren-indent-line ()
  (interactive)
  (wren-indent-to (wren-calculate-indent)))


;;;###autoload
(define-derived-mode wren-mode prog-mode "wren"
  "Major mode for editing Wren."

  ;; syntax table
  (modify-syntax-entry ?/ ". 124b" wren-mode-syntax-table)
  (modify-syntax-entry ?* ". 23" wren-mode-syntax-table)
  (modify-syntax-entry ?\n "> b" wren-mode-syntax-table)
  (modify-syntax-entry ?' "\"" wren-mode-syntax-table)
  (modify-syntax-entry ?+ "." wren-mode-syntax-table)
  (modify-syntax-entry ?^ "." wren-mode-syntax-table)
  (modify-syntax-entry ?% "." wren-mode-syntax-table)
  (modify-syntax-entry ?> "." wren-mode-syntax-table)
  (modify-syntax-entry ?< "." wren-mode-syntax-table)
  (modify-syntax-entry ?= "." wren-mode-syntax-table)
  (modify-syntax-entry ?~ "." wren-mode-syntax-table)

  (setq font-lock-defaults '((wren-font-lock-keywords)))

  (set (make-local-variable 'comment-start) "//")

  (set (make-local-variable 'indent-line-function) 'wren-indent-line))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.wren\\'" . wren-mode))

(provide 'wren-mode)
