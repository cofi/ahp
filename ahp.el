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

(defun ahp-update-projects ()
  "Update all projects."
  (interactive)
  (setq ahp-projects
        (cl-loop for (base . files) in ahp--projects
                 collect (cons base (ahp--files-in base)))))

(defun ahp--files-in (dir)
  "Return a sorted list of files that are recursively contained in `dir'.

Returned files are pruned, see `ahp--pruned-ls'.
Files contained in pruned directories are not included."
  (let ((q (queue-create)))
    (queue-enqueue q dir)
    (cl-loop until (null (queue-head q)) ; expanded `queue-empty', didn't compile otherwise
             for (dirs files) = (ahp--pruned-ls (queue-dequeue q))
             do (ahp--enqueue-all q dirs)
             nconc files into rfiles
             finally (cl-return (sort rfiles #'string<)))))

(defun ahp--dirs-in (dir)
  "Return a sorted list of directories that are recursively contained in `dir'.

Returned directories are pruned, see `ahp--pruned-ls'.
Directories contained in pruned directories are not included."
  (let ((q (queue-create)))
    (queue-enqueue q dir)
    (cl-loop until (null (queue-head q))
             for (dirs _) = (ahp--pruned-ls (queue-dequeue q))
             do (ahp--enqueue-all q dirs)
             nconc dirs into rdirs
             finally (cl-return (progn (cl-delete-duplicates rdirs)
                                    (sort rdirs #'string<))))))

(defun ahp--pruned-ls (dir)
  "Return a two element list of pruned directories and files in `dir'.

Directories are pruned according to `ahp-ignored-dirs'
and files according to `ahp-ignored-files' and `ahp-ignored-file-patterns'.
If `ahp-only-these-patterns' is non-nil only files that match are
collected."
  (cl-loop for file in (directory-files dir t)
           for name = (file-name-nondirectory file)
           when (and (file-directory-p file) (not (member name (cl-union '("." "..")
                                                                     ahp-ignored-dirs))))
             collect file into dirs
           when (if ahp-only-these-patterns
                    (string-match-p (regexp-opt ahp-only-these-patterns) name)
                  (and (not (file-directory-p file))
                       (not (member name ahp-ignored-files))
                       (not (string-match-p (regexp-opt ahp-ignored-file-patterns) name))))
             collect file into files
           finally
             (cl-return (list dirs files))))

(defun ahp--enqueue-all (queue xs)
  "Enqueue everything in `xs' in `queue'."
  (cl-loop for x in xs
           do (queue-enqueue queue x)))

(provide 'ahp)
;;; ahp.el ends here
