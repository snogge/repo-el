;;; repo-test.el --- Repo mode test suite -*- lexical-binding: t; -*-

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

;; Unit test suite for repo mode

;;; Code:

(require 'repo)
(require 'el-mock)
(require 'cl)

(ert-deftest repo-status-buffer-name/file-name ()
  (should (string= (repo-status-buffer-name "/test/foo-bar") "*repo: foo-bar")))

(ert-deftest repo-status-buffer-name/directory-name ()
  (should (string= (repo-status-buffer-name "/test/foo-bar/") "*repo: foo-bar")))

(ert-deftest repo-process-buffer-name/file-name ()
  (should (string= (repo-process-buffer-name "/test/foo-bar") "*repo-process: foo-bar")))

(ert-deftest repo-process-buffer-name/directory-name ()
  (should (string= (repo-process-buffer-name "/test/foo-bar/") "*repo-process: foo-bar")))

(ert-deftest repo-toplevel/nil-when-not-found ()
  (should (not (repo-toplevel "/I/Do/Not/Exist"))))

(ert-deftest repo-toplevel/toplevel-dir ()
  (should (string= (repo-toplevel repo-test/fake-workspace-path)
                   repo-test/fake-workspace-path)))

(ert-deftest repo-toplevel/sublevel-dir ()
  (should (string= (repo-toplevel (f-expand "project1" repo-test/fake-workspace-path))
                   repo-test/fake-workspace-path)))

(ert-deftest repo-toplevel/default-directory-nil-when-not-found ()
  (let ((default-directory "/I/Do/Not/Exist"))
    (should (not (repo-toplevel )))))

(ert-deftest repo-toplevel/default-directory-toplevel-dir ()
  (let ((default-directory repo-test/fake-workspace-path))
    (should (string= (repo-toplevel )
                     repo-test/fake-workspace-path))))

(ert-deftest repo-toplevel/sublevel-dir-default ()
  (let ((default-directory (f-expand "project1" repo-test/fake-workspace-path)))
    (should (string= (repo-toplevel)
                     repo-test/fake-workspace-path))))

(ert-deftest repo-workspace-p ()
  (should-not (repo-workspace-p "I/Do/Not/Exist"))
  (should-not (repo-workspace-p repo-test/tmp-path))
  (should (repo-workspace-p repo-test/fake-workspace-path)))

(ert-deftest repo-revert-buffer/revert-buffer ()
  (with-temp-buffer
    (read-only-mode)
    (setq repo-workspace "foo/bar")
    (with-mock
     (mock (repo-status "foo/bar"))
     (repo-revert-buffer nil nil))))

(ert-deftest repo-internal-vc-function/no-custom-no-magit ()
  (should (eql (repo-internal-vc-function) (function vc-dir))))

(ert-deftest repo-internal-vc-function/no-custom-with-magit ()
  (cl-letf (((symbol-function 'magit-status-setup-buffer) #'ignore))
    (should (eql (repo-internal-vc-function) (function magit-status-setup-buffer)))))

(ert-deftest repo-internal-vc-function/with-custom ()
  (cl-letf (((symbol-function 'magit-status-setup-buffer) #'ignore)
            ((symbol-function 'custom-vc-function) #'ignore))
    (let ((repo-vc-function (function custom-vc-function)))
      (should (eql (repo-internal-vc-function) (function custom-vc-function))))))

(ert-deftest repo-call-vc-function/call-function ()
  (with-mock
   (mock (custom-vc-function "foo/bar"))
   (let ((repo-vc-function (function custom-vc-function)))
     (repo-call-vc-function "foo/bar"))))

(ert-deftest repo-find/workspace ()
  (with-resource "status-buffer.txt"
    (with-mock
     (mock (dired "~/foo/bar") :times 2)
     (repo-find)
     (end-of-line)
     (repo-find))))

(ert-deftest repo-find/branch ()
  (with-resource "status-buffer.txt"
    (with-mock
     (mock (repo-call-vc-function "~/foo/bar/.repo/manifests/") :times 3)
     (setq repo-workspace "~/foo/bar/")
     (forward-line)
     (repo-find)
     (forward-line)
     (repo-find)
     (end-of-line)
     (repo-find))))

(ert-deftest repo-find/project ()
  (with-resource "status-buffer.txt"
    (with-mock
     (mock (repo-call-vc-function "~/foo/bar/foo2/") :times 2)
     (setq repo-workspace "~/foo/bar/")
     (forward-line 14)
     (repo-find)
     (end-of-line)
     (repo-find))))

(ert-deftest repo-find/file ()
  (with-resource "status-buffer.txt"
    (setq repo-workspace "~/foo/bar/")
    (forward-line 6)
    (with-mock
      (mock (find-file "~/foo/bar/foo1/nodiff_1") :times 2)
      (repo-find)
      (end-of-line)
      (repo-find))))

(ert-deftest repo-exec/failure ()
  (let ((repo-executable "I/Do/Not/Exist"))
    (should-error (repo-exec "I/Do/Not/Exist" (current-buffer) "--version")
                  :type 'file-error)
    (should-error (repo-exec repo-test/fake-workspace-path
                             (current-buffer) "--version")
                  :type 'file-error)))

(ert-deftest repo-default-sentinel/should-error-unless-finished ()
  (with-temp-buffer
    (with-mock
     (mock (process-buffer 'proc) => (current-buffer))
     (should-error (repo-default-sentinel 'proc "failed\n"))
     (goto-char (point-min))
     (should (looking-at "Repo process failed\n")))))


(ert-deftest repo-status/wrong-executable ()
  (let ((repo-executable "I/Do/Not/Exist"))
    (should-error (repo-status repo-test/fake-workspace-path) :type 'file-error)))


(ert-deftest repo-status/executable-error ()
  (let ((repo-executable "false")
        (proc-buffer (repo-process-buffer-name repo-test/fake-workspace-path))
        ;; The error we want to verify is actually signalled by (a
        ;; function called by) the sentinel function
        ;; `repo-status-exec-status'.  Override that function so it
        ;; stores any signal in `sentinel-error'.
        (orig-sentinel (symbol-function 'repo-status-exec-status))
        sentinel-error)
    (cl-letf (((symbol-function 'repo-status-exec-status)
               (lambda (proc event)
                 (condition-case err
                     (funcall orig-sentinel proc event)
                   (error (setq sentinel-error err))))))
      (repo-status repo-test/fake-workspace-path)
      (let ((process-buffer (get-buffer proc-buffer))
            (status-process (get-buffer-process proc-buffer)))
        (should process-buffer)
        (ert-wait-for 10 (lambda ()
                           (eql (process-status status-process) 'exit)))))
    (should sentinel-error)
    (kill-buffer proc-buffer)))

(ert-deftest repo-status/call-init-on-bad-workspace ()
  (with-temp-buffer
    (with-mock
     (mock (repo-call-init-default-directory repo-test/tmp-path) => 't)
     (mock (repo-status-exec-info repo-test/tmp-path))
     (repo-status repo-test/tmp-path)))
  (with-temp-buffer
    (with-mock
     (mock (repo-call-init-default-directory repo-test/tmp-path) => nil)
     (not-called repo-status-exec-info)
     (should-error (repo-status repo-test/tmp-path)))))

(ert-deftest repo-status-setup-buffer/buffer ()
  (with-mock
   (mock (repo-mode))
   (with-temp-buffer
     (repo-status-setup-buffer (current-buffer) repo-test/fake-workspace-path)
     (should (string= (buffer-name) "*repo: fake-workspace"))
     (should (string= (buffer-local-value 'default-directory (current-buffer))
                      repo-test/fake-workspace-path)))))


(ert-deftest repo-status-parse-buffer/failure ()
  (with-resource "status-buffer.txt"
    (setq-local default-directory repo-test/fake-workspace-path)
    (with-mock
     (mock (repo-default-sentinel 'proc "finished\n"))
     (mock (process-buffer 'proc) => (current-buffer))
     (should-error (repo-status-parse-buffer 'proc "finished\n"))
     (should-not (get-buffer (repo-status-buffer-name repo-test/fake-workspace-path))))))

(ert-deftest repo-status-parse-buffer/parse ()
  (with-resource "process-buffer-info-status.txt"
    (setq-local default-directory repo-test/fake-workspace-path)
    (with-mock
     (mock (repo-default-sentinel 'proc "finished\n"))
     (mock (process-buffer *) => (current-buffer))
     (repo-status-parse-buffer 'proc "finished\n"))
    (with-current-buffer (repo-status-buffer repo-test/fake-workspace-path)
       (goto-char (point-min))
       (should (looking-at (format "Workspace: +%s" repo-test/fake-workspace-path)))
       (forward-line)
       (should (looking-at "Manifest branch: +refs/heads/master"))
       (forward-line)
       (should (looking-at "Manifest merge branch: +refs/heads/merge"))
       (forward-line)
       (should (looking-at "Manifest groups: +all,-notdefault"))
       (forward-line)
       (forward-line)
       (should (looking-at "project project-foo/ +branch dev"))
       (forward-line)
       (should (looking-at "project project-bar/ +branch dev"))
       (kill-buffer))))


(provide 'repo-test)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; repo-test.el ends here
