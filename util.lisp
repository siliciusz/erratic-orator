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

(defun collect-files-from-tree (path)
  "Recursively collect all files from the tree with the root at given path."
  (let ((files '()))
    (uiop:collect-sub*directories
     path
     (constantly t)
     (constantly t)
     (lambda (sub-directory)
       (setf files (append files (uiop:directory-files sub-directory)))))
    files))


(defun extract-unique (list &key (test #'equal))
  "Returns all unique lements of the given list in an undefined order."
  (loop :for item :in list
	:with set
	:do (setf set (adjoin item set :test test))
	:finally (return set)))
