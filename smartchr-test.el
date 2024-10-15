;;; smartchr-test.el --- Tests for smartchr          -*- lexical-binding: t; -*-

;; Copyright (c) 2009 by IMAKADO.

;; Author: IMAKADO <ken.imakado@gmail.com>
;; URL: https://github.com/imakado/emacs-smartchr
;; blog: http://d.hatena.ne.jp/IMAKADO (japanese)
;; Prefix: smartchr
;; Package-Requires: ((emacs "24.3"))
;; LICENSE: GPL-2.0-or-later

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; These tests depends on ert-expectation package.

;;; Code:
(require 'smartchr)
(require 'ert)
(require 'cl-lib)

(ert-deftest smartchr-test-1 nil
  (should
   (string=
    "{  }"
    (with-temp-buffer
      (let ((smartchr-struct-cursor-re "`!!'")
            (struct (smartchr-parse "{ `!!' }")))
        (cl-assert (smartchr-struct-p struct))
        (funcall (smartchr-struct-insert-fn struct))
        (buffer-string))))))

(ert-deftest smartchr-test-2 nil
  (should
   (string=
    "{  }"
    (with-temp-buffer
      (let ((smartchr-struct-cursor-re "`!!'")
            (struct (smartchr-parse "{ `!!' }")))
        (cl-assert (smartchr-struct-p struct))
        (funcall (smartchr-struct-insert-fn struct))
        (buffer-string))))))

(ert-deftest smartchr-test-3 nil
  (should
   (string=
    ""
    (with-temp-buffer
      (let ((smartchr-struct-cursor-re "`!!'")
            (struct (smartchr-parse "{ `!!' }")))
        (cl-assert (smartchr-struct-p struct))
        (funcall (smartchr-struct-insert-fn struct))
        (funcall (smartchr-struct-cleanup-fn struct))
        (buffer-string))))))

;; template allow function
(ert-deftest smartchr-test-4 nil
  (should
   (eq
    t
    (with-temp-buffer
      (let ((smartchr-struct-cursor-re "`!!'")
            (fn-called nil)
            (struct (smartchr-parse (lambda nil (setq fn-called t)))))
        (cl-assert (smartchr-struct-p struct))
        (funcall (smartchr-struct-insert-fn struct))
        fn-called)))))

(ert-deftest smartchr-test-5 nil
  (should
   (string=
    "hi"
    (with-temp-buffer
      (let ((smartchr-struct-cursor-re "`!!'")
            (struct (smartchr-parse (lambda nil "hi"))))
        (cl-assert (smartchr-struct-p struct))
        (funcall (smartchr-struct-insert-fn struct))
        (buffer-string))))))

;; smartchr-parse pass argument if argument is already struct
(ert-deftest smartchr-test-6 nil
  (should
   (eq
    t
    (smartchr-struct-p
     (smartchr-parse
      (smartchr-make-struct :cleanup-fn (lambda nil)
                    :insert-fn (lambda nil)))))))

;; smartchr-parse rest args
(ert-deftest smartchr-test-6 nil
  (should
   (string=
    "a"
    (with-temp-buffer
      (let ((smartchr-struct-cursor-re "`!!'")
            (cmd (smartchr "a" "b")))
        (call-interactively cmd)
        (buffer-string))))))

;;; smartchr-test.el ends here
