;;; perl-ts-mode.el --- Another Major mode for Perl -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Pranshu Sharma

;; Author: Pranshu Sharma <pranshu@bauherren.ovh>
;; Created: 2025, Jan
;; Keywords: languages, Perl
;; Package-Requires: ((emacs "30.1"))
;; Version: 1.1.0
;; URL: https://hg.sr.ht/~pranshu/perl-ts-mode

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Major mode for editing perl

;; Uses grammer at:
;; https://github.com/tree-sitter-perl/tree-sitter-perl branch: release
;; https://github.com/tree-sitter-perl/tree-sitter-pod branch: release

(require 'cperl-mode)
(require 'treesit)

;;; Code:

(defvar treesit-primary-parser)

(defgroup perl-ts nil
  "Major mode for editing perl files."
  :group 'languages
  :prefix "perl-ts-")

(defcustom perl-ts-highlight-verbatim t
  "Use font lock inside the verbatim.
`perl-ts-mode' will need to be reloaded if this changed."
  :type 'boolean)

(defcustom perl-ts-font-lock-level 4
  "Level of font lock, 1 for minimum highlghting and 4 for maximum."
  :type '(choice (const :tag "Minimal Highlighting" 1)
		 (const :tag "Low Highlighting" 2)
		 (const :tag "High Highlighting" 3)
		 (const :tag "Maximum Highlighting" 4)))

(defun perl-ts-str-to-list (str)
  "Converts a space seperated string(s) to an array.
Argument STR is either a string, or a list of strings."
  (split-string
   (if (listp str)
       (mapconcat (lambda (s) (concat s " ")) str)
     str)))

(defvar perl-ts-syntax-table
  (eval-when-compile
    (let ((table (make-syntax-table))
	  (syntax-list
	   '(("." ?/ ?* ?+ ?- ?= ?% ?< ?> ?&)
	     ("\\"  ?$ ?\\)
	     ("\"" ?' ?`)
	     ("." ?|)
	     ("<" ?#)
	     (">" ?\n)
	     ("_" ?:))))
      ;; The defaults are mostly fine
      (dolist (ls syntax-list table)
	(dolist (char (cdr ls))
	  (modify-syntax-entry char (car ls) table)))))
  "Syntax table for perl.")

(defvar perl-ts-main-keywords
  (vconcat
   (perl-ts-str-to-list
    "return if elsif else unless while do for local my our field"))
  "Primary keywords for perl.")

(defvar perl-ts-special-operators
  (vconcat (split-string
	    "x or and eq ne cmp lt le ge gt isa"))
  "Special operator faces for perl.")

(defvar perl-ts-non-overidable
  (concat
   "\\`"
   (regexp-opt
    (perl-ts-str-to-list
     '("break catch chomp chop default"
       "defined delete each eval say"
       "evalbytes exists finally foreach format given"
       "grep keys last map next"
       "no our package pop pos print printf prototype"
       "splice split state study tie"
       "tied try unless unshift untie"))
    t)
   "\\'")
  "Functions cannot be overriden in perl.")

;; These keywords don't exist in the grammer, but are inbuilt
(defvar perl-ts-keywords
  (concat
   "\\`"
   (regexp-opt
    (perl-ts-str-to-list
     '("accept atan2 bind binmode bless chmod chown chr cmp connect join"
       "continue crypt dbmopen dump endgrent endhostent endnetent setprotoent"
       "endprotoent endpwent endservent eof exec fcntl flock fork formline"
       "getgrent gethostbyaddr gethostbyname gethostent getlogin getnetbyaddr"
       "getnetent getppid getpriority getprotobyname getprotobynumber  listen"
       "getprotoent getpwent getpwnam getservbyname getservbyport getservent"
       "getsockname getsockopt glob gt index ioctl kill lcfirst link push"
       "mkdir msgctl msgget msgrcv msgsnd not open opendir pack pipe read"
       "readdir recv rename reverse rindex seek seekdir select semctl semget"
       "semop send setgrent sethostent setnetent setpgrp setpriority sysseek"
       "setpwent setservent setsockopt shmctl shmget shmread shmwrite shutdown"
       "socket socketpair sprintf substr symlink syscall sysopen sysread"
       "system syswrite tell time times truncate unlink unpack utime values vec"
       "wait waitpid wantarray warn"))
    t)
   "\\'"))

(defun perl-ts-fontify-node (node face)
  "Put FACE on NODE."
  (and node
       (put-text-property (treesit-node-start node)
			  (treesit-node-end node)
			  'face face)))

(defun perl-ts-fontify-function (node &rest _)
  "Fontify perl function name at NODE."
  (let ((node-text (treesit-node-text node)))
    (cond
     ((string-match perl-ts-non-overidable node-text)
      (perl-ts-fontify-node node 'cperl-nonoverridable-face))
     ((string-match perl-ts-keywords node-text)
      (perl-ts-fontify-node node 'font-lock-type-face)))))

;; Not using regexes to highlight regexes? Ya know that's heard of
(defun perl-ts-highlight-regex (node &rest _)
  "Function that highlights regex ranging from NODE."
  (let ((start (treesit-node-start node))
	(end (treesit-node-end node)))
    ;; To do this most efficiantly, we will go char by char
    (save-excursion
      (goto-char start)
      (while (< (point) end)
	;; `thing-at-point' doesn't accept char argument
	;; We need 1 char lookahead
	(put-text-property
	 (point)
	 (1+ (point))
	 'face
	 (pcase (car (string-to-list
		      (buffer-substring-no-properties (point) (+ 1 (point)))))
	   ((or ?\[ ?\] ?^ ?$) 'font-lock-function-name-face)
	   ((or ?\( ?\)) 'font-lock-keyword-face)
	   ((or ?* ?\\ ?+) 'font-lock-builtin-face)
	   (_ 'font-lock-regexp-face)))
	(forward-char 1)))))

;; Wow, the indenting was simpler than I expected.  I am still have
;; trauma from haskell-ts-mode.
(defvar perl-ts-indent-settings
  `((perl
     ((parent-is "source_file") column-0 0)

     ;; We want indenting like:
     ;; my ($a,
     ;;     $b) = @_
     
     ((lambda (node parent _)
	(and (string= "variable_declaration" (treesit-node-type parent))
	     (treesit-node-prev-sibling node t)))
      prev-sibling 0)

     ((parent-is "list_expression\\|anonymous_hash_expression") parent 0)

     ((node-is "}") standalone-parent 0)
     ((node-is ")") standalone-parent 2)
     
     ;; Don't indent.
     ((parent-is "heredoc_content\\|string_content") (lambda (_ _ bol) bol) 0)

     ((node-is "'\\|\"") column-0 0)
     
     
     ((parent-is "block") standalone-parent 2)

     ((parent-is "binary_expression") standalone-parent 2)
     
     (catch-all parent 2))))

(defun perl-ts-highlight-hash (node &rest _)
  "Highlight the perl hash at NODE."
  (unless (treesit-node-children node)
    (put-text-property
     (treesit-node-start node)
     (treesit-node-end node)
     'face 'cperl-hash-face)))

(defun perl-ts-doc-markup (node &rest _)
  "Markup special markup in NODE constructs for POD."
  (seq-let (letter content)
      (treesit-node-children node t)
    (put-text-property (treesit-node-start letter)
		       (treesit-node-end letter)
		       'face
		       'font-lock-doc-markup-face)
    (when-let*
	((prop (pcase (string-to-char (treesit-node-text letter))
		 (?B 'bold)
		 (?I 'italic)
		 (?L 'link)
		 ;; We could do code injection for "C"
		 ((or ?F ?C) 'default)
		 (_ nil))))
      (put-text-property (treesit-node-start content)
			 (treesit-node-end content)
			 'face
			 prop))))

(defvar perl-ts-font-lock
  (treesit-font-lock-rules
   :language 'perl
   :feature 'keywords
   ;; We will leave globs out from here as they aren't supported
   ;; properly here.
   `((use_statement "use" @font-lock-keyword-face
		    :anchor (_) @font-lock-function-name-face)
     (require_expression "require" @font-lock-keyword-face
			 :anchor (_) @font-lock-function-name-face)
     (phaser_statement
      phase: _ @font-lock-keyword-face)
     (package_statement "package" @font-lock-keyword-face
			:anchor (_) @font-lock-function-name-face)
     ;; Undefs are handeled weirdly by the grammer.
     (undef_expression "undef" @cperl-nonoverridable-face)
     ,perl-ts-special-operators @font-lock-type-face
     ,perl-ts-main-keywords @font-lock-keyword-face
     (prototype) @font-lock-string-face)
   :language 'perl
   :feature 'core-functions
   '((func1op_call_expression function: _ @font-lock-type-face)
     ["sort" "grep" "map"] @cperl-nonoverridable-face
     (goto_expression (bareword) @font-lock-constant-face)
     (subroutine_declaration_statement
      "sub" @font-lock-keyword-face
      name: (_) @font-lock-function-name-face))
   :language 'perl
   :feature 'special-functions
   '((function) @perl-ts-fontify-function
     (bareword) @perl-ts-fontify-function)
   :language 'perl
   :feature 'comment
   '((comment) @font-lock-comment-face)
   :language 'perl
   :feature 'string
   '((heredoc_content) @font-lock-string-face
     "'" @font-lock-constant-face
     (string_content) @font-lock-string-face
     (autoquoted_bareword) @font-lock-string-face)
   :language 'perl
   :feature 'declaration
   '((variable_declaration (scalar) @font-lock-variable-name-face)
     (signature (mandatory_parameter (scalar) @font-lock-variable-name-face))
     (signature (slurpy_parameter (array) @cperl-array-face))
     (signature (slurpy_parameter (hash) @cperl-hash-face))
     (signature (optional_parameter (scalar) @font-lock-variable-name-face))
     (localization_expression (scalar) @font-lock-variable-name-face))
   :language 'perl
   :feature 'array
   :override 'keep
   '((arraylen) @cperl-array-face
     (hash_element_expression
      (container_variable) @cperl-hash-face)
     (array_element_expression
      (container_variable) @cperl-array-face)
     (hash_element_expression hash: (_)
			      @cperl-hash-face)
     (slice_container_variable) @cperl-hash-face
     (arraylen) @cperl-array-face)
   :language 'perl
   :feature 'intorpolated
   :override t
   '((string_content (scalar) @font-lock-variable-name-face
		     (array) @cperl-array-face))
   :language 'perl
   ;; needed since heredoc end overlaps with string_literal
   :override t
   :feature 'heredoc
   '([(heredoc_token) (heredoc_end)] @font-lock-constant-face)
   :language 'perl
   :feature 'number
   '((number) @font-lock-number-face)
   :language 'perl
   :feature 'regex-highlighting
   '((regexp_content) @perl-ts-highlight-regex
     (substitution_regexp
      "s" @cperl-nonoverridable-face)
     [(match_regexp_modifiers)
      (substitution_regexp_modifiers)
      (transliteration_modifiers)]
     @cperl-nonoverridable-face
     (match_regexp "m" @cperl-nonoverridable-face)
     (transliteration_expression
      ["y" "tr"] @cperl-nonoverridable-face)
     ["qw" "qq" "q"] @cperl-nonoverridable-face)
   :language 'perl
   :feature 'attributes
   '((attribute_name) @font-lock-constant-face
     (attribute_value) @font-lock-string-face)
   ;; "Treesit font lock setting for perl."
   :language 'perl
   :feature 'class
   '((class_statement "class" @font-lock-keyword-face
		      name: (_) @font-lock-function-name-face)
     (method_declaration_statement
      "method" @font-lock-keyword-face
      name: (_) @font-lock-function-name-face)
     (class_phaser_statement
      phase: _ @font-lock-keyword-face))
   :language 'pod
   :feature 'pod
   '((command_paragraph
      (command) @font-lock-doc-markup-face)
     (command_paragraph
      (content) @font-lock-variable-name-face)
     (cut_paragraph) @font-lock-doc-markup-face
     ((plain_paragraph) @font-lock-doc-face))
   :language 'pod
   :feature 'pod
   '((interior_sequence) @perl-ts-doc-markup)))

(defvar perl-ts-font-lock-feature-list
  '((keywords comment string class pod)
    (declaration core-functions number attributes)
    (heredoc array intorpolated regex-highlighting pod-markup)
    (special-functions)))

(defun perl-ts-function-name (node)
  "Highlight the function name at NODE accordingly."
  (while (and
	  node
	  (not (string= "subroutine_declaration_statement"
			(treesit-node-type node))))
    (setq node (treesit-node-parent node)))
  (and node
       (treesit-node-text (treesit-node-child-by-field-name node "name") t)))

(defun perl-ts-imenu-create-index ()
  "Creates the imenu index for perl."
  (let* ((main "(main)")
	 mainn
	 (mainp (treesit-node-parent (treesit-node-at 1))))
    ;; `n' is either a package or function name
    (save-excursion
      (seq-filter
       #'identity
       (append
	(mapcar
	 (pcase-lambda (`(,name . ,node))
	   (if (eq 'package name)
	       (prog1 nil
		 (setq main (treesit-node-text node t))
		 (setq mainn (treesit-node-parent node))
		 (setq mainp (treesit-node-parent mainn)))
	     (let ((parent (treesit-node-parent
			    (treesit-node-parent node))))
	       (when (or (equal mainp parent)
			 (equal mainn (treesit-node-parent
				       parent)))
		 ;; Must not be a lexical funciton
		 (cons (concat main "::" (treesit-node-text node))
		       (progn (goto-char (treesit-node-start node))
			      (point-marker)))))))
	 (treesit-query-capture
	  treesit-primary-parser
	  '((package_statement name: (_) @package)
	    (subroutine_declaration_statement
	     name: (_) @name))))
	(apply #'append
	       (mapcar
		(lambda (class_node)
		  (let ((cname (treesit-node-text
				(treesit-node-child-by-field-name (cdr class_node) "name")
				t)))
		    (mapcar
		     (lambda (node)
		       (cons
			(concat cname "::"
				(treesit-node-text (cdr node)))
			(progn
			  (goto-char
			   (treesit-node-start (cdr node)))
			  (point-marker))))
		     (treesit-query-capture
		      (cdr class_node)
		      '((subroutine_declaration_statement
			 name: (_) @sub_name)
			(method_declaration_statement
			 name: (_) @method_name))))))
		(treesit-query-capture treesit-primary-parser
				       '((class_statement) @class)))))))))

(defun perl-ts-sexp (node)
  "Returns non-nil when NODE is a sexp."
  (let ((type (treesit-node-type node)))
    (not
     (or
      ;; Expression statements are technically sexps, but this really
      ;; makes navigation annoying.  Plus sentence commands already
      ;; exist to work on these statements.
      (string-match (regexp-opt
		     (cons "expression_statement"
			   (split-string "[{(;)}]" "" t)))
		    type
		    0 t)
      (string= "expression_statement"
	       (treesit-node-type (treesit-node-parent node)))
      (and (string= "operator" (treesit-node-field-name node))
	   (= 1 (length (treesit-node-text node t))))))))

(defvar perl-ts-thing-settings
  `((perl
     (comment "comment")
     (sentence "expression_statement")
     ;; IMO this way it's way more intutive that an bigger
     ;; operator is a sexp, while the small pluses should not be
     (sexp perl-ts-sexp)
     (string (or "string_content"
		 "heredoc_content"))
     (text (or "comment"
	       "string_content"
	       "heredoc_content"
	       "pod")))
    (pod
     (sentence "command_paragraph")
     (sexp ".*")
     (text "plain_paragraph")))
  "Thingy Minginy.")

(defun perl-ts-outline-acceptable (node)
  "Matches a comment NODE that starts with multiple hashes."
  (let ((gp (treesit-node-parent (treesit-node-parent node))))
    (and (or (not gp)
	     (member (treesit-node-type gp)
		     '("class_statement" "package_statement")))
	 (if (string= (treesit-node-type node) "comment")
	     (string= "###" (substring (treesit-node-text node t) 0 3))
	   t))))

(defun perl-ts-language-at-point (point)
  "Return language at POINT."
  (if (string= "pod"
	       (treesit-node-type (treesit-node-at point 'perl)))
      'pod
    'perl))

(defun perl-ts-forward-sentence (arg)
  "`perl-ts-mode' implementation of `forward-sentence-function'.

ARG is the same as in `forward-sentence'.  This function is a wrapper
around `treesit-forward-sentence'."
  (if (> arg 0)
      (dotimes (_ arg)
	(treesit-forward-sentence 1)
	(while-let ((char (buffer-substring-no-properties (point) (+ (point) 1)))
		    ((member char '(" " "\t" ";"))))
	  (forward-char 1)))
    (treesit-forward-sentence arg)))

(defun perl-ts-outline-level ()
  "The function `outline-level' function for `perl-ts-mode'."
  (let ((node (treesit-node-at (point) treesit-primary-parser)))
    (pcase (treesit-node-type node)
      ;; pod comment
      ("package" 1)
      ("class" 2)
      ("comment"
       (string-match "^\\(#+\\)" (treesit-node-text node))
       (match-end 1))
      ("sub" 4)
      ("method" 4)
      ("pod" 2)
      (_ 1))))

(defvar-keymap perl-ts-mode-map
  :doc "Keymap `for perl-ts-mode'.
Takes all the relevent commands from `cperl-mode'."
  "C-c C-a" 'cperl-toggle-auto-newline
  "C-c C-k" 'cperl-toggle-abbrev
  ;; Doesn't work for some reason
  ;; "C-c C-t" 'cperl-invert-if-unless
  "C-c C-b" 'cperl-find-bad-style
  "C-c C-p" 'cperl-pod-spell
  "C-c C-d" 'cperl-here-doc-spell
  "C-c C-n" 'cperl-narrow-to-here-doc
  "C-c C-h p" 'cperl-perldoc
  "C-c C-h P" 'cperl-perldoc-at-point)

(define-derived-mode perl-ts-mode prog-mode "PTS"
  :abbrev-table cperl-mode-abbrev-table
  :syntax-table perl-ts-syntax-table
  "Major mode for perl powered by tree-sitter."
  (unless (and (treesit-ready-p 'perl)
	       (treesit-ready-p 'pod))
    (error "Grammer for perl or pod is not available"))
  (setq-local cperl-electric-keywords t)
  (setq-local electric-pair-pairs
	      '((?\" . ?\") (?\' . ?\') (?\" . ?\")
		(?\[ . ?\]) (?\{ . ?\}) (?\( . ?\))))
  (setq-local treesit-language-at-point-function 'perl-ts-language-at-point)
  ;; We can't use the treesitter interface of imenu because of perl's
  ;; package class system where it is hard to tell which class
  ;; function is in.
  (setq-local imenu-create-index-function 'perl-ts-imenu-create-index)
  (setq-local treesit-defun-type-regexp
	      "subroutine_declaration_statement\\|method_declaration_statement")
  (setq-local treesit-font-lock-level perl-ts-font-lock-level)
  (setq-local treesit-thing-settings perl-ts-thing-settings)
  (setq-local treesit-defun-name-function 'perl-ts-function-name)
  (setq-local treesit-outline-predicate
	      (cons
	       (concat "\\`"
		       (regexp-opt '("class_statement"
				     "method_declaration_statement"
				     "subroutine_declaration_statement"
				     "package_statement"
				     "pod"
				     "comment")
				   t)
		       "\\'")
	       'perl-ts-outline-acceptable))
  (setq-local comment-start "#")
  ;; EXTREMLY delicate
  (setq-local font-lock-defaults nil
	      treesit-primary-parser (treesit-parser-create 'perl)
	      treesit-font-lock-settings perl-ts-font-lock
	      treesit-font-lock-feature-list perl-ts-font-lock-feature-list
	      treesit-simple-indent-rules perl-ts-indent-settings)
  (treesit-parser-create 'pod)
  (let ((args '(:host perl
		      :embed perl
		      :local t
		      ((substitution_regexp
			(replacement) @cap
			(substitution_regexp_modifiers) @_mod
			(:match ".*e.*" @_mod)))
		      :embed pod
		      :host perl
		      ((pod) @capture))))
    (setq-local treesit-range-settings
		(apply
		 #'treesit-range-rules
		 (if perl-ts-highlight-verbatim
		     (append args
			     '(:host pod
				     :embed perl
				     :local t
				     ((verbatim_paragraph (content) @cap))))
		   args))))
  (treesit-major-mode-setup)
  (setq-local outline-level #'perl-ts-outline-level
	      forward-sentence-function #'perl-ts-forward-sentence))

(provide 'perl-ts-mode)

;;; perl-ts-mode.el ends here
