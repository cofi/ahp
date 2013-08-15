;;; ahp.el --- ad hoc projects

;; Copyright (C) 2013 Michael Markert
;; Author: Michael Markert <markert.michael@gmail.com>
;; Created: 2013-08-02
;; Version:
;; Keywords: projects convenicence
;; URL: https://github.com/cofi/ahp
;; Package-Requires: ((queue "0.1")
;;
;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;;; Code:

(require 'cl-lib)
(require 'queue)

(defgroup ahp nil
  "Ad hoc projects."
  :group 'ahp
  :prefix 'ahp)

(defcustom ahp-roots
  (list ".git" ".hg" ".svn" "Makefile" ".ahp" ".dir-locals.el")
  "Files that make the containing directory roots of ad hook projects."
  :type '(repeat string)
  :group 'ahp)

(defcustom ahp-ignored-dirs
  (list ".git" ".hg" ".svn")
  "List of directory names whose subdirectories will not be
traversed for files or directories."
  :type '(repeat string)
  :group 'ahp)

(defcustom ahp-ignored-files
  (list "TAGS" "GTAGS" "GPATH" "GRTAGS" "GSYMS")
  "List of files that will not be included in the file list."
  :type '(repeat string)
  :group 'ahp)

(defcustom ahp-ignored-file-patterns
  (list "\.elc" "\.o" "\.pyc")
  "List of file patterns that will not be included in the file list.
Interpreted as regexp."
  :type '(repeat string)
  :group 'ahp)

(defcustom ahp-only-these-patterns nil
  "List of file patterns that will not be included in the file list.
Interpreted as regexp."
  :type '(repeat string)
  :group 'ahp)

(defvar ahp--projects nil)

(provide 'ahp)
;;; ahp.el ends here
