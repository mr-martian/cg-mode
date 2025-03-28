;;; cg-mode.el --- major mode for editing CG files

;; Copyright (C) 2025 Daniel Swanson

;; Author: Daniel Swanson <apertium@dangswan.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.0"))
;; Url: http://wiki.apertium.org/wiki/Emacs
;; Keywords: languages

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Provides syntax highlighting for Constraint Grammar .cg3/.rlx files

;; Usage:
;;
;; (use-package cg-mode
;;   :defer t
;;   :mode (("\\.cg3\\'" . cg-mode) ("\\.rlx\\'" . cg-mode)))

;;; Code:

(defconst cg-mode-version "0.1.0" "Version of cg-mode.")

(require 'xref)
(require 'treesit)

;;;============================================================================
;;;
;;; Define the formal stuff for a major mode named cg.
;;;

(defgroup cg-mode nil
  "Major mode for editing CG source files."
  :tag "CG"
  :group 'languages)

(defcustom cg-indent-offset 4
  "Basic size of one indentation step."
  :type 'integer
  :safe 'integerp)

(defcustom cg-context-indent-offset (* 2 cg-indent-offset)
  "Indent size for contextual tests."
  :type 'integer
  :safe 'integerp)

(defvar cg--treesit-indent-rules
  '((cg
      ((and (parent-is "rule_with") (node-is "rule.*")) parent-bol cg-indent-offset)
      ((match "}" "rule_with") parent-bol 0)
      ((parent-is "rule.*") parent-bol cg-context-indent-offset)
      ((parent-is "list") parent-bol cg-indent-offset)
      ((parent-is "set") parent-bol cg-indent-offset)))
  "Tree-sitter indentation rules for `cg-mode'.")

(defun cg--treesit-defun-name (node)
  "Return the defun name of NODE.
Return nil if there is no name or if NODE is not a defun node."
  (pcase (treesit-node-type node)
    ("list" (treesit-node-text
             (treesit-node-child-by-field-name node "name") t))
    ("set" (treesit-node-text
            (treesit-node-child-by-field-name node "name") t))))

(defvar cg--treesit-settings
  (treesit-font-lock-rules
    ; LEVEL 1
    :feature 'comment
    :language 'cg
    '((comment) @font-lock-comment-face)

    :feature 'keyword
    :language 'cg
    '([(END)
        (LIST) (SET)
        (ANCHOR)
        (IF)
        (TARGET)
        (EXCEPT)
        (BEFORE) (AFTER)
        (TO) (FROM)
        (WITHCHILD) (NOCHILD)
        (WITH)
        (ONCE) (ALWAYS)
        (TEMPLATE)
        ] @font-lock-keyword-face)

    :feature 'rule
    :language 'cg
    '([
        (ruletype)
        (ruletype_substitute_etc)
        (ruletype_parentchild)
        (ruletype_relation) (ruletype_relations)
        (ruletype_map_etc)
        (ruletype_addcohort) (ruletype_mergecohorts) (ruletype_copycohort)
        (ruletype_move) (ruletype_switch)
        (ruletype_external)
        (ruletype_with)
        ] @font-lock-function-call-face)

    :feature 'constant
    :language 'cg
    '([(special_list_name)
        (STATIC_SETS)
        (MAPPING_PREFIX)
        (SUBREADINGS) (LTR) (RTL)
        (OPTIONS)
        (LIST_TAGS) (STRICT_TAGS) (PARENTHESES)
        ] @font-lock-builtin-face)

    ; LEVEL 2
    :feature 'variable
    :language 'cg
    '(
       (setname) @font-lock-variable-use-face
       (inlineset_single (taglist (ntag) @font-lock-constant-face))
       )

    :feature 'context
    :language 'cg
    '((contextpos) @font-lock-number-face)

    :feature 'bracket
    :language 'cg
    '((["(" ")" "[" "]" "{" "}"]) @font-lock-bracket-face)

    :feature 'string
    :language 'cg
    '(((qtag) @font-lock-string-face))

    ; LEVEL 3
    :feature 'delimiter
    :language 'cg
    '((semicolon) @font-lock-delimiter-face)

    :feature 'operator
    :language 'cg
    '([(eq) (pluseq)
        (set_op)
        ] @font-lock-operator-face)

    :feature 'error
    :language 'cg
    '((ERROR) @error))
  "Tree-sitter font-lock settings for `cg-mode'.")

;;;###autoload
(define-derived-mode cg-mode prog-mode "CG"
  "Major mode for editing Constraint Grammar .cg3 files.
CG-mode provides the following specific keyboard key bindings:

\\{cg-mode-map}"
  (set (make-local-variable 'comment-start) "#")
  (set (make-local-variable 'comment-start-skip) "#+[\t ]*")

  (if (not (treesit-ready-p 'cg))
      (message "Run `M-x cg-install-tree-sitter' to install syntax highlighting, indentation and imenu support.")
    ;; Tree-sitter specific setup.
    (treesit-parser-create 'cg)
    (setq-local treesit-simple-indent-rules cg--treesit-indent-rules)
    (setq-local treesit-defun-type-regexp "list\|set")
    (setq-local treesit-defun-name-function #'cg--treesit-defun-name)
    (setq-local treesit-font-lock-settings cg--treesit-settings)
    (setq-local treesit-font-lock-feature-list
                '((comment keyword rule constant)
                  (variable context bracket string)
                  (delimiter operator error)))
    (treesit-major-mode-setup)))

(defun cg-install-tree-sitter ()
  "Install the tree-sitter grammar for cg."
  (interactive)
  (let ((treesit-language-source-alist
         '((cg "https://github.com/apertium/tree-sitter-apertium" "master" "tree-sitter-cg/src"))))
    (treesit-install-language-grammar 'cg))
  ;; refresh currently open cg buffers:
  (mapc (lambda (b) (with-current-buffer b
                 (when (eq major-mode 'cg-mode)
                   (cg-mode))))
        (buffer-list)))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.cg3$" . cg-mode))
(add-to-list 'auto-mode-alist '("\\.rlx$" . cg-mode))

;;; Interactive functions -----------------------------------------------------

(defun cg-mode-goto-definition ()
  "Go to definition (attribute category or output pattern) of symbol at point."
  ;; TODO: Use tree-sitter query instead
  (interactive)
  (when-let* ((thing (thing-at-point 'symbol))
              (delim (if (save-excursion
                           (goto-char (car (bounds-of-thing-at-point 'symbol)))
                           (looking-back "%" (- (point) 1)))
                         ":"
                       "="))
              (match (save-excursion
                       (goto-char (point-min))
                       (re-search-forward (format "^\\s *%s\\s *%s"
                                                  (regexp-quote thing)
                                                  delim)
                                          nil
                                          'noerror))))
    (xref--push-markers)
    (goto-char match)))

;;; Keybindings --------------------------------------------------------------
(define-key cg-mode-map (kbd "M-.") #'cg-mode-goto-definition)
(define-key cg-mode-map (kbd "M-,") #'pop-to-mark-command)

;;; Run hooks -----------------------------------------------------------------
(run-hooks 'cg-mode-load-hook)

(provide 'cg-mode)

;;;============================================================================

;;; cg-mode.el ends here
