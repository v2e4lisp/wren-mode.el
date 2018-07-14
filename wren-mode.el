;;; wren-mode.el --- wren.io emacs mode     -*- lexical-binding: t -*-

(defvar wren-mode-hook nil)

(defconst wren-keywords:data
  '("break" "class" "construct" "else" "for" "foreign" "if"
    "import" "in" "is" "return" "static" "super" "this" "var"
    "while" "new")
  "wren keywords for data")

(defconst wren-keywords:constant
  '("true" "false" "null")
  "wren keywords for constants")

(defcustom wren-indent-offset 2
  "Indentation offset for `wren-mode'."
  :type 'integer
  :group 'wren
  :safe #'integerp)

(defcustom wren-mode-hook nil
  "Hooks called on wren mode."
  :type 'hook
  :group 'wren)

(defvar wren-this-regexp "_\\w+")
(defvar wren-super-regexp "\\<super\\>")
;; (defvar wren-class-regexp "class[\t ]\\w+")
;; (defvar wren-defun-regexp "\\w+\\( \\|\t\\){")


(defvar wren-font-lock-keywords
  (let ((beg "\\<")
        (end "\\>"))
    (list
     (cons (concat beg (regexp-opt wren-keywords:constant t) end)
           font-lock-constant-face)
     (cons (concat beg (regexp-opt wren-keywords:data t) end)
           font-lock-keyword-face)
     ;; (cons wren-class-regexp font-lock-type-face)
     (cons wren-this-regexp font-lock-variable-name-face)
     (cons wren-super-regexp font-lock-variable-name-face)))
  "wren keywords highlighting")


(defun wren-indent-line ()
  "Indent current line for `wren-mode'."
  (interactive)
  (let ((indent-col 0))
    (save-excursion
      (beginning-of-line)
      (condition-case nil
	      (while t
	        (backward-up-list 1)
	        (when (looking-at "[{]")
	          (setq indent-col (+ indent-col wren-indent-offset))))
        (error nil)))
    (save-excursion
      (back-to-indentation)
      (when (and (looking-at "[}]") (>= indent-col wren-indent-offset))
        (setq indent-col (- indent-col wren-indent-offset)))
	  (indent-line-to indent-col))
    (if (string-match "^[ \t]+$" (thing-at-point 'line))
        (end-of-line))))


;; syntax table
(defvar wren-mode-syntax-table
  (with-syntax-table (copy-syntax-table)
    (modify-syntax-entry ?/ ". 124b")
    (modify-syntax-entry ?* ". 23")
    (modify-syntax-entry ?\n "> b")
    (modify-syntax-entry ?' "\"")
    (modify-syntax-entry ?+ ".")
    (modify-syntax-entry ?^ ".")
    (modify-syntax-entry ?% ".")
    (modify-syntax-entry ?> ".")
    (modify-syntax-entry ?< ".")
    (modify-syntax-entry ?= ".")
    (modify-syntax-entry ?~ ".")
    (syntax-table))
  "`wren-mode' Syntax table")


;;;###autoload
(define-derived-mode wren-mode prog-mode "wren"
  "Major mode for editing Wren."
  :syntax-table wren-mode-syntax-table
  :group 'wren
  (setq-local font-lock-defaults '((wren-font-lock-keywords)))
  (setq-local comment-start "//")
  (setq-local indent-line-function 'wren-indent-line))


;;;###autoload
(add-to-list 'auto-mode-alist '("\\.wren\\'" . wren-mode))

(provide 'wren-mode)
