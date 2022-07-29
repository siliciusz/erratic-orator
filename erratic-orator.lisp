;;; erratic-orator
;;; Copyright: 2022 Tomasz Jeneralczyk <tj@schwi.pl>
;;;
;;; This file is part of erratic-orator.
;;;
;;; erratic-orator is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU Affero General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; erratic-orator is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU Affero General Public License for more details.
;;;
;;; You should have received a copy of the GNU Affero General Public License
;;; along with erratic-orator.  If not, see <http://www.gnu.org/licenses/>.

(in-package :erratic-orator)

(lexer:define-lexer lexer (state)
  ("%s+"  (values :next-token))  ; ignore whitespace
  ("%d+"  (values :number      $$))
  ("%w+"  (values :word        $$))
  ("%p+"  (values :punctuation $$))
  ("."    (values :special     $$)))

(defun state-type  (token) (symbol-name (car token)))
(defun state-value (token) (cdr token))

(defun tokenize-string (text &optional (source nil))
  (lexer:with-lexer (lexer #'lexer text :source source)
    (lexer:with-token-reader (token-reader lexer)
      (loop :for (class value) = (multiple-value-list (funcall token-reader))
	    :while class
	    :collect (cons class value)))))


(defun  tokenize-stream (stream &optional (source nil))
  (loop :for line = (read-line stream nil) :while line
	:appending (tokenize-string line source)))


(defun tokenize-file (file-path &optional (encoding :default))
  (with-open-file (file file-path :external-format encoding)
    (tokenize-stream file file-path)))
