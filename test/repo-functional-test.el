;;; repo-functional-test.el --- Repo mode functional test suite  -*- lexical-binding: t; -*-

;; Copyright (C) 2014  Damien Merenne

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

;; Functional test suite for repo mode

;;; Code:

(require 'f)
(require 'repo)

(ert-deftest repo-status/call-vc-function ()
  :tags '(functional)
  (with-workspace
   (let* ((opened nil)
          (project nil)
          (repo-vc-function (lambda (directory)
                              (setq opened directory)))
          (manifest-path (file-name-as-directory
                          (f-join workspace ".repo/manifests/"))))
     (repo-status workspace)
     (wait-for-status 3 workspace
       (goto-char (point-min))
       (should (re-search-forward "^Workspace:.*test/tmp/workspace" nil t))
       (should (re-search-forward "Manifest branch: +refs/heads/master" nil t))
       (repo-find)
       (should (string= opened manifest-path))
       (should (re-search-forward "Manifest merge branch: refs/heads/master" nil t))
       (setq opened nil)
       (repo-find)
       (should (string= opened manifest-path))
       (should (re-search-forward "Manifest groups: +all,-notdefault" nil t))
       (setq opened nil)
       (repo-find)
       (should-not opened)
       (should (re-search-forward "project \\(project./\\) +branch dev" nil t))
       (setq project (match-string 1))
       (setq opened nil)
       (repo-find)
       (should (string= opened (concat repo-test/workspace-path project)))
       (should (re-search-forward "project \\(project./\\) +branch dev" nil t))
       (setq project (match-string 1))
       (setq opened nil)
       (repo-find)
       (should (string= opened (concat repo-test/workspace-path project)))
       (kill-buffer)))))

(ert-deftest repo-status/revert ()
  :tags '(functional)
  (with-workspace
   (repo-status workspace)
   (wait-for-status 3 workspace)
   (let ((newfile   (f-join workspace "project1" "newfile")))
     (with-temp-file newfile
       (insert "foobar"))
     (with-status-buffer workspace
       (revert-buffer)
       (wait-for-regexp 10 (current-buffer) "--"
         (goto-char (point-min))
         (should (re-search-forward "project project1/" nil t))
         (forward-line)
         (should (looking-at "^ --\\W+newfile$")))
       (kill-buffer))
     (delete-file newfile))))

(ert-deftest repo-status/repo-error ()
  (let ((proc-buffer (repo-process-buffer-name repo-test/fake-workspace-path))
        (status-buffer (repo-status-buffer-name repo-test/fake-workspace-path)))
    (repo-status repo-test/fake-workspace-path)
    (should-error (ert-wait-for 10
                      (lambda ()
                        (and (get-buffer proc-buffer)
                             (with-current-buffer proc-buffer
                               (goto-char (point-max))
                               (re-search-backward "Process exited" nil t))))))
    (with-current-buffer proc-buffer
      (goto-char (point-max))
      (should (re-search-backward "Running repo info -lo" nil t))
      (forward-line)
      (should (looking-at "error: command 'info'"))
      (should (re-search-forward "Repo process exited abnormally with code" nil t)))
    (should-not (get-buffer status-buffer))
    (kill-buffer proc-buffer)))


(provide 'repo-functional-test)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; repo-functional-test.el ends here
