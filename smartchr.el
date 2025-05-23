;;; smartchr.el --- Support input several candidates with a single key -*- lexical-binding: t -*-

;; Copyright (c) 2009 by IMAKADO.

;; Author: IMAKADO <ken.imakado@gmail.com>
;; URL: https://github.com/imakado/emacs-smartchr
;; blog: http://d.hatena.ne.jp/IMAKADO (japanese)
;; Package-Requires: ((emacs "24.3"))
;; License: GPL-2.0-or-later

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

;;; Thanks to k1LoW for original idea.

;;; Commentary:

;; ## Usage
;;
;; smartchr generates an interactive closure, so bind it to keymap.
;;
;;     (global-set-key (kbd "=") (smartchr '(" = " " == " " === ")))
;;     (define-key python-mode-map (kbd "=") (smartchr '(" = " " == ")))
;;
;; Substitute `!!' with cursor.
;;
;;     (global-set-key (kbd "{")
;;                     (smartchr '("{ `!!' }" "{ \"`!!'\" }" "{")))
;;
;; The cursor symbol can be changed with the `smartchr-template-cursor-re' variable.

;;; Code:
(eval-when-compile
  (require 'cl-lib)
  (require 'rx))

(defgroup smartchr nil
  "Support input several candidates with a single key."
  :group 'smartchr)

(defcustom smartchr-template-cursor-re (eval-when-compile (rx "`!!'"))
  "A regexp that searches for the position where the cursor moves when inserting."
  :type 'regexp
  :group 'smartchr)

(cl-defstruct (smartchr-struct
               (:constructor smartchr-make-struct
                             (&key cleanup-fn insert-fn)))
  cleanup-fn insert-fn)

(defun smartchr (&rest list-of-string)
  "Make an interactive command to support input several LIST-OF-STRING candidates."
  (let* ((list-of-string (if (consp (car-safe list-of-string))
                             (car-safe list-of-string)
                           list-of-string))
         (smartchr-structs (mapcar 'smartchr-parse list-of-string))
         (last-struct nil)
         (count 0))
    (lambda ()
      (interactive)
      (if (eq this-command real-last-command)
          (cl-incf count)
        (setq count 0))
      (when (>= count (length smartchr-structs))
        (setq count 0))
      ;; cleanup -> insert
      (let ((struct (nth count smartchr-structs)))
        (cl-assert (smartchr-struct-p struct))
        (when (eq this-command real-last-command)
          (cl-assert (smartchr-struct-p last-struct))
          (funcall (smartchr-struct-cleanup-fn last-struct)))
        (setq last-struct struct)
        (funcall (smartchr-struct-insert-fn struct))))))

(defun smartchr-parse (template)
  "Return smartchr-struct by TEMPLATE."
  (cond
   ((smartchr-struct-p template)
    template)
   ((functionp template)
    (let ((str-or-struct (funcall template)))
      (cond
       ((smartchr-struct-p str-or-struct)
        str-or-struct)
       ((stringp str-or-struct)
        (smartchr-parse str-or-struct))
       ((smartchr-make-struct
         :cleanup-fn (lambda ())
         :insert-fn (lambda ()))))))
   ((string-match smartchr-template-cursor-re template)
    (cl-destructuring-bind (pre post) (save-match-data (split-string template smartchr-template-cursor-re))
      (let ((pre pre) (post post))
        (smartchr-make-struct
         :cleanup-fn (lambda ()
                       (delete-char (- (length pre)))
                       (delete-char (length post)))
         :insert-fn (lambda ()
                      (insert pre)
                      (save-excursion (insert post)))))))
   ((let ((template template))
      (smartchr-make-struct
       :cleanup-fn (lambda () (delete-char (- (length template))))
       :insert-fn (lambda () (insert template)))))))

(provide 'smartchr)
;;; smartchr.el ends here.
