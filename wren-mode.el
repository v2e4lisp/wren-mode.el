;;; wren-mode.el --- wren.io emacs mode     -*- lexical-binding: t -*-

(defvar wren-mode-hook nil)

(defconst wren-keywords
  '("break" "class" "construct" "else" "for" "foreign" "if"
    "import" "in" "is" "return" "static" "super" "this" "var" "while"))

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

(defvar wren-this-regexp "\\_<_.*?\\_>")
;; (defvar wren-class-regexp "class[\t ]\\w+")
;; (defvar wren-defun-regexp "\\w+\\( \\|\t\\){")

(eval-when-compile
  (defun wren-ppre (re)
    (format "\\<\\(%s\\)\\>" (regexp-opt re))))

(defvar wren-font-lock-keywords
  (append
   `(
     (,(regexp-opt wren-keywords 'symbols) . font-lock-keyword-face)
     (,(regexp-opt wren-keywords:constant 'symbols) . font-lock-constant-face)
     ("\\_<_.*?\\_>" . 'font-lock-variable-name-face)       ;; this
     ("\\_<__.*?\\_>" . font-lock-variable-name-face)     ;; static this
     ("\\_<\\(.*?\\)\\_>(.*)[[:space:]]+{" 1 font-lock-function-name-face)   ;; function
   ))
  "wren keywords highlight")

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

(defun wren-close-curly ()
  (interactive)
  (insert "}")
  (wren-indent-line))

(defvar wren-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'newline-and-indent)
    (define-key map "\C-j" 'newline-and-indent)
    (define-key map "}" 'wren-close-curly)
    map)
  "keymap for wren major mode")


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
    (modify-syntax-entry ?_ "w")
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
