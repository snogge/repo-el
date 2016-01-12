;;; test-helper.el --- Repo mode test suite initialization  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Damien Merenne

;; Author: Damien Merenne
;; URL: https://github.com/canatella/repo

;; This file is NOT part of GNU Emacs.

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
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Initialization code for test suite for repo mode

;;; Code:

(require 'f)
(require 'url)

(defvar repo-test/test-path
  (f-dirname (f-this-file)))

(defvar repo-test/root-path
  (f-parent repo-test/test-path))

(defvar repo-test/tmp-path
  (file-name-as-directory (f-join repo-test/test-path "tmp/")))

(defvar repo-test/repo-path
  (f-join repo-test/tmp-path "repo"))

(defvar repo-test/resources-path
  (file-name-as-directory (f-join repo-test/test-path "resources/")))

(defvar repo-test/fake-workspace-path
  (file-name-as-directory (f-join repo-test/resources-path "fake-workspace/")))

(defvar repo-test/git-repositories-path
  (file-name-as-directory (f-join repo-test/tmp-path "git/")))

(defvar repo-test/workspace-path
  (file-name-as-directory (f-join repo-test/tmp-path "workspace/")))


(defvar repo-test/no-cleanup nil "Do not cleanup if set to true.")
;(setq repo-test/no-cleanup t)

(defun repo-test/message (&rest args)
  "Display message ARGS."
  (if ert-runner-verbose
      (apply 'message args)))


(repo-test/message "Fetching repo in %s" repo-test/repo-path)
(f-mkdir repo-test/tmp-path)
(with-current-buffer
    (url-retrieve-synchronously "https://storage.googleapis.com/git-repo-downloads/repo" 't)
  (setq buffer-file-name repo-test/repo-path)
  (goto-char (point-min))
  (search-forward "\n\n" nil t)
  (delete-region (point-min) (point))
  (save-buffer)
  (chmod repo-test/repo-path #o755))

(custom-set-variables '(repo-executable repo-test/repo-path))

(defun repo-test/cleanup (directory)
  "Remove DIRECTORY unless `report-test/no-cleanup' is set."
  (unless repo-test/no-cleanup
    (f-delete directory t)))

(defun repo-test/git (directory command)
  "Run git in DIRECTORY with COMMAND."
  (let ((git-command (format "git --work-tree=%s --git-dir=%s %s"
                             (f-expand directory repo-test/resources-path)
                             (f-expand directory repo-test/git-repositories-path)
                             command)))
    (repo-test/message "Running: %s" git-command)
    (repo-test/message (shell-command-to-string git-command))))

(defun repo-test/git-setup (directory)
  "Setup a git repository in DIRECTORY."
  (repo-test/git directory "init")
  (repo-test/git directory "add --all")
  (repo-test/git directory "commit -a -m 'initial commit'"))

(defmacro with-git-repositories (&rest body)
  "Setup git repositories, execute BODY and cleanup."
  `(progn
     (f-mkdir repo-test/git-repositories-path)
     (repo-test/git-setup "manifests")
     (repo-test/git-setup "project1")
     (repo-test/git-setup "project2")
     ,@body
     (repo-test/cleanup repo-test/git-repositories-path)))
(put 'with-git-repositories 'lisp-indent-function 1)

(defun repo-test/repo (command)
  "Run repo COMMAND in `repo-test/workspace-path'."
  (let* ((default-directory repo-test/workspace-path)
         (repo-command (format "%s %s" repo-test/repo-path command)))
    (repo-test/message "Running: %s" repo-command)
    (repo-test/message (shell-command-to-string repo-command))))

(defmacro with-workspace (&rest body)
  "Setup git repositories, run repo init, repo sync, execute BODY and cleanup."
  `(with-git-repositories
    (f-mkdir repo-test/workspace-path)
    (repo-test/repo (format "init -q --no-repo-verify -u %s" (f-expand "manifests" repo-test/git-repositories-path)))
    (repo-test/repo "sync -q")
    (repo-test/repo "start dev --all")
    (let ((workspace repo-test/workspace-path))
      ,@body)
    (repo-test/cleanup repo-test/workspace-path)))
(put 'with-workspace 'lisp-indent-function 1)

(defun report-test/cleanup (&rest _)
  "Cleanup test data."
  (repo-test/cleanup repo-test/tmp-path))

(add-hook 'ert-runner-reporter-run-ended-functions (function report-test/cleanup))

(load (f-expand "repo" repo-test/root-path))

(add-to-list 'load-path repo-test/root-path)

(defmacro with-resource (resource &rest body)
  "Load RESOURCE file in a temporary buffer and execute the forms in BODY in it."
  `(with-temp-buffer
     (insert-file-contents (f-expand ,resource repo-test/resources-path))
     (goto-char (point-min))
     ,@body))
(put 'with-resource 'lisp-indent-function 1)

(defmacro with-status-buffer (workspace &rest body)
  "In WORKSPACE status buffer, run BODY forms."
  `(with-current-buffer (repo-status-buffer ,workspace)
     ,@body))
(put 'with-status-buffer 'lisp-indent-function 1)

(defmacro ert-wait-for (timeout predicate &rest body)
  "Wait for maximum TIMEOUT second for PREDICATE to verify, than execute forms in BODY."
  `(with-timeout
       (,timeout (ert-fail (format "Timeout of %ds exceeded while waiting for predicate." ,timeout)))
     (while (not (funcall ,predicate))
       (accept-process-output nil 0.05))
     ,@body))
(put 'ert-wait-for 'lisp-indent-function 2)

(defmacro wait-for-status (timeout workspace &rest body)
  "Wait for maximum TIMEOUT second for WORKSPACE status buffer, than execute forms in BODY."
  `(ert-wait-for ,timeout (lambda () (get-buffer (repo-status-buffer-name ,workspace)))
     (with-current-buffer (get-buffer (repo-status-buffer-name ,workspace))
       ,@body)))
(put 'wait-for-status 'lisp-indent-function 2)

(defmacro wait-for-regexp (timeout buffer regexp &rest body)
  "Wait for maximum TIMEOUT second for status BUFFER to match REGEXP, than execute forms in BODY."
  `(ert-wait-for ,timeout (lambda ()
                            (with-current-buffer ,buffer
                              (goto-char (point-min))
                              (re-search-forward ,regexp nil t)))
     (with-current-buffer ,buffer
       ,@body)))
(put 'wait-for-regexp 'lisp-indent-function 3)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; test-helper.el ends here
