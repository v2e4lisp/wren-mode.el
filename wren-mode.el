;;; wren-mode.el --- Major mode for editing Wren scripts
;;
;; Version: 0.0.0
;; Keywords: languages, wren
;; URL: https://github.com/v2e4lisp/wren-mode.el
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of Emacs.
;;
;; This is free and unencumbered software released into the public domain.
;;
;; Anyone is free to copy, modify, publish, use, compile, sell, or
;; distribute this software, either in source code form or as a compiled
;; binary, for any purpose, commercial or non-commercial, and by any
;; means.
;;
;; In jurisdictions that recognize copyright laws, the author or authors
;; of this software dedicate any and all copyright interest in the
;; software to the public domain. We make this dedication for the benefit
;; of the public at large and to the detriment of our heirs and
;; successors. We intend this dedication to be an overt act of
;; relinquishment in perpetuity of all present and future rights to this
;; software under copyright law.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
;; IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR ANY CLAIM, DAMAGES OR
;; OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
;; ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
;; OTHER DEALINGS IN THE SOFTWARE.
;;
;; For more information, please refer to <http://unlicense.org>
;;
;;; Commentary:
;;
;; Provides support for editing Wren, providing indentation
;; and syntactical font-locking.
;;
;;; Code:


(defconst wren-keywords
  '("break" "class" "construct" "else" "for"
    "foreign" "if" "import" "in" "is"
    "return" "static" "super" "this" "var"
    "while")
  "Wren language keywords.")

(defconst wren-constants
  '("true" "false" "null")
  "Wren language constants.")


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
     (cons (concat beg (regexp-opt wren-keywords t) end)
           font-lock-keyword-face)
     (cons (concat beg (regexp-opt wren-constants t) end)
           font-lock-constant-face)
     (cons wren-this-regexp font-lock-variable-name-face)
     (cons wren-super-regexp font-lock-variable-name-face)))
  "Wren keywords highlighting.")


(defun wren-comment-or-string-p (&optional pos)
  "Return t if it's a comment or string at POS, defaulting to point."
  (save-excursion (let ((parse-result (syntax-ppss pos)))
                    (or (elt parse-result 3) (elt parse-result 4)))))


(defun wren-previous-indent ()
  "Return the indentation level of the previous non-blank line."
  (save-excursion
    (wren-goto-preivous-nonblank-line)
    (current-indentation)))


(defun wren-goto-preivous-nonblank-line ()
  "Move backward until on a non blank line."
  (forward-line -1)
  (while (and (looking-at "^[ \t]*$") (not (bobp)))
    (forward-line -1)))


(defun wren-indent-to (x)
  "Indent line to X column."
  (when x
    (let (shift top beg)
      (and (< x 0) (error "Invalid nest"))
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
  "Return the column to which the current line should be indented."
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
  "Indent current line of Wren code."
  (interactive)
  (wren-indent-to (wren-calculate-indent)))


;;;###autoload
(define-derived-mode wren-mode prog-mode "Wren"
  "Major mode for editing Wren."

  ;; syntax table
  (modify-syntax-entry ?/ ". 124bn" wren-mode-syntax-table)
  (modify-syntax-entry ?* ". 23bn" wren-mode-syntax-table)
  (modify-syntax-entry ?\n "> b" wren-mode-syntax-table)
  (modify-syntax-entry ?+ "." wren-mode-syntax-table)
  (modify-syntax-entry ?^ "." wren-mode-syntax-table)
  (modify-syntax-entry ?% "." wren-mode-syntax-table)
  (modify-syntax-entry ?> "." wren-mode-syntax-table)
  (modify-syntax-entry ?< "." wren-mode-syntax-table)
  (modify-syntax-entry ?= "." wren-mode-syntax-table)
  (modify-syntax-entry ?~ "." wren-mode-syntax-table)
  (modify-syntax-entry ?_ "_" wren-mode-syntax-table)

  ;; abbrev table
  ;; See: https://groups.google.com/forum/#!topic/wren-lang/T3uZmpZT6PA
  (define-abbrev wren-mode-abbrev-table "Sys" "System" nil :case-fixed)

  (setq font-lock-defaults '((wren-font-lock-keywords)))

  (set (make-local-variable 'electric-indent-chars) '(?\n ?\}))

  (set (make-local-variable 'comment-start) "//")

  (set (make-local-variable 'indent-line-function) 'wren-indent-line))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.wren\\'" . wren-mode))

(provide 'wren-mode)
;;; wren-mode.el ends here
