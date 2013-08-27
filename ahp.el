;;; ahp.el --- ad hoc projects

;; Copyright (C) 2013 Michael Markert
;; Author: Michael Markert <markert.michael@gmail.com>
;; Created: 2013-08-02
;; Version: 0.3
;; Keywords: projects convenience
;; URL: https://github.com/cofi/ahp
;; Package-Requires: ((queue "0.1"))
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

(defcustom ahp-ignored-dir-pattern nil
  "Pattern of directory names whose subdirectories will not
be traversed for files or directories."
  :type 'string
  :group 'ahp)

(defcustom ahp-ignored-files
  (list "TAGS" "GTAGS" "GPATH" "GRTAGS" "GSYMS")
  "List of files that will not be included in the file list."
  :type '(repeat string)
  :group 'ahp)

(defcustom ahp-ignored-file-pattern
  (regexp-opt '("\.elc" "\.o" "\.pyc"))
  "Pattern of files that will not be included in the file list."
  :type 'string
  :group 'ahp)

(defcustom ahp-only-this-pattern nil
  "Pattern of files that will only be included in the file list."
  :type 'string
  :group 'ahp)

(defcustom ahp-completing-read #'ido-completing-read
  "Completing read to use."
  :type '(choice
          (const :tag "Normal" completing-read)
          (const :tag "Ido" ido-completing-read)
          (function :tag "Other"))
  :group 'ahp)

(defcustom ahp-project-save-file (expand-file-name "ahp-projects" user-emacs-directory)
  "File to save projects on exit and load on start.
Set to nil to prevent saving and loading."
  :type 'string
  :group 'ahp)

(defvar ahp--projects nil)

(defun ahp-update-projects ()
  "Update all projects."
  (interactive)
  (cl-loop for (project . plist) in ahp--projects
           when (plist-member plist :files)
             do (ahp--project-files project t)
           when (plist-member plist :dirs)
           do (ahp--project-dirs project t)))

;;;###autoload
(defun ahp-dired (dir)
  "Open a project directory in dired."
  (interactive (list (funcall ahp-completing-read (format "(%s) Directory: " (ahp--project-name))
                              (ahp--project-dirs (ahp--project-root))
                              nil t)))
  (dired dir))

;;;###autoload
(defun ahp-root-dired (choose-project)
  "Open the project's base directory in dired.

With a prefix choose the project first."
  (interactive "P")
  (let* ((project (if (or choose-project (not (buffer-file-name)))
                      (funcall ahp-completing-read "Project: " (ahp--projects) nil t)
                    (ahp--project-root))))
    (dired project)))

;;;###autoload
(defun ahp-switch-to-buffer (buffer)
  "Switch to a buffer in the project."
  (interactive (list (funcall ahp-completing-read (format "(%s) Buffer: " (ahp--project-name))
                              (mapcar #'buffer-name
                                      (ahp--project-buffers (expand-file-name (ahp--project-root))))
                              nil t)))
  (switch-to-buffer buffer))

;;;###autoload
(defun ahp-find-file (choose-project)
  "Find a file in the project.

With a prefix choose the project first."
  (interactive "P")
  (let* ((project (if (or choose-project (not (buffer-file-name)))
                      (funcall ahp-completing-read "Project: " (ahp--projects) nil t)
                    (ahp--project-root))))
    (find-file (funcall ahp-completing-read (format "(%s) File: " (ahp--project-name project))
                        (ahp--project-files project)
                        nil t))))

;;;###autoload
(defun ahp-kill-other-buffers ()
  "Kill other buffers of the current project."
  (interactive)
  (let ((buffers (ahp--project-buffers (expand-file-name (ahp--project-root)))))
    (mapc #'kill-buffer buffers)
    (message "Killed %d buffer" (length buffers))))

;;;###autoload
(defun ahp-kill-buffers ()
  "Kill all buffers of the current project."
  (interactive)
  (ahp-kill-other-buffers)
  (kill-this-buffer))

;;;###autoload
(defun ahp-save-projects ()
  "Save projects to `ahp-project-save-file'."
  (interactive)
  (let ((print-length nil))
    (when (stringp ahp-project-save-file)
      (with-current-buffer (find-file-noselect ahp-project-save-file)
        (delete-region (point-min) (point-max))
        (print (ahp--projects) (current-buffer))
        (save-buffer)
        (kill-buffer (current-buffer))))))

;;;###autoload
(defun ahp-read-projects (initialize)
  "Read projects from `ahp-project-save-file'.

With prefix also initialize caches."
  (interactive "P")
  (ignore-errors
    (let ((projects (when (and (stringp ahp-project-save-file) (file-exists-p ahp-project-save-file))
                      (with-temp-buffer
                        (insert-file-contents ahp-project-save-file)
                        (goto-char (point-min))
                        (read (current-buffer))))))
      (cl-loop for project in projects
               do (cl-pushnew (list project) ahp--projects :key #'first :test #'string=))
      (when initialize
        (cl-loop for project in (ahp--projects)
                 do (progn (ahp--project-files project)
                           (ahp--project-dirs project)))))))

(defun ahp--projects ()
  "Return the projects."
  (cl-loop for (name . _) in ahp--projects
           collect name))

(defun ahp--project-root (&optional buffer)
  "Return the base directory of the current project or the project `buffer' is in."
  (let ((fname (buffer-file-name buffer)))
    (cl-loop for root in ahp-roots
             for dir = (locate-dominating-file fname root)
             when dir
              do (cl-return dir))))

(defun ahp--project-name (&optional buffer-or-path)
  "Return the name of the project."
  (let ((root (if (stringp buffer-or-path)
                  buffer-or-path
                (ahp--project-root buffer-or-path))))
    (file-name-nondirectory (directory-file-name root))))

(defun ahp--access-property (project property constructor force-update)
  "Return `property' of `project' and construct it using `constructor' if it does not exist.

`constructor' has to be a function taking the base directory of the project."
  (let ((entry (assoc project ahp--projects)))
    (unless entry
      (setq entry (list project))
      (push entry ahp--projects))
    (when (or force-update (not (plist-get (cdr entry) property)))
      (let* ((project-config (ahp--read-project-config project))
             (ahp-ignored-dirs (or (cdr (assoc 'ahp-ignored-dirs project-config)) ahp-ignored-dirs))
             (ahp-ignored-dir-pattern (or (cdr (assoc 'ahp-ignored-dir-pattern project-config)) ahp-ignored-dir-pattern))
             (ahp-ignored-files (or (cdr (assoc 'ahp-ignored-files project-config)) ahp-ignored-files))
             (ahp-ignored-file-pattern (or (cdr (assoc 'ahp-ignored-file-pattern project-config)) ahp-ignored-file-pattern))
             (ahp-only-this-pattern (or (cdr (assoc 'ahp-only-this-pattern project-config)) ahp-only-this-pattern)))
        (setf (cdr entry) (plist-put (cdr entry) property (funcall constructor project)))))
    (plist-get (cdr entry) property)))

(defun ahp--project-files (project &optional force-update)
  "Return files of `project'.
If `force-update' is non-nil the cache will be ignored."
  (ahp--access-property project :files #'ahp--files-in force-update))

(defun ahp--project-dirs (project &optional force-update)
  "Return directories of `project'.
If `force-update' is non-nil the cache will be ignored."
  (ahp--access-property project :dirs #'ahp--dirs-in force-update))

(defun ahp--project-buffers (project)
  "Return the buffers which are in `project'."
    (cl-loop for buffer in (buffer-list)
             for fname = (buffer-file-name buffer)
             when (and fname (string-prefix-p project (expand-file-name fname)))
               collect buffer))

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

Directories are pruned according to `ahp-ignored-dirs' and
`ahp-ignored-dir-pattern', files according to `ahp-ignored-files'
and `ahp-ignored-file-pattern'.
If `ahp-only-this-pattern' is non-nil only files that match are collected."
  (cl-loop for file in (directory-files dir t)
           when (ahp--accept-dir-p file)
             collect file into dirs
           when (ahp--accept-file-p file)
             collect file into files
           finally
             (cl-return (list dirs files))))

(defun ahp--accept-dir-p (file)
  (let ((name (file-name-nondirectory file)))
    (and (file-directory-p file)
         (not (member name (cl-union '("." "..") ahp-ignored-dirs)))
         (not (and ahp-ignored-dir-pattern (string-match-p ahp-ignored-dir-pattern name))))))

(defun ahp--accept-file-p (file)
  (let ((name (file-name-nondirectory file)))
    (if ahp-only-this-pattern
        (string-match-p ahp-only-this-pattern name)
      (and (not (file-directory-p file))
           (not (member name ahp-ignored-files))
           (not (and ahp-ignored-file-pattern (string-match-p ahp-ignored-file-pattern name)))))))

(defun ahp--enqueue-all (queue xs)
  "Enqueue everything in `xs' in `queue'."
  (cl-loop for x in xs
           do (queue-enqueue queue x)))
  "Predicate if directory `file' should be considered as part of a project."

(defun ahp--read-project-config (project)
  "Read local config of `project' and return it as an alist."
  (let ((config-file (expand-file-name ".ahp" project)))
    (if (file-readable-p config-file)
        (with-temp-buffer
  "Predicate if file `file' should be considered as part of a project."
          (insert-file-contents config-file)
          (goto-char (point-min))
          (read (current-buffer)))
        nil)))

(when ahp-project-save-file
  (add-hook 'kill-emacs-hook #'ahp-save-projects)
  (ahp-read-projects nil))

(provide 'ahp)
;;; ahp.el ends here
