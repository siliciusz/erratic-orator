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


(defvar *default-database-path* ":memory:")
(defvar *database* nil "the connection to database")


(defun close-database ()
  (when *database*
    (sqlite:disconnect *database*)
    (setf *database* nil)))


(defun open-database (&optional (path *default-database-path*))
  (if *database*
      (error (format nil "Some database is already opened." ))
      (setf *database* (sqlite:connect path))))


(defun ensure-database-structure ()
  (sqlite:with-transaction *database*
    (dolist (query '("CREATE TABLE IF NOT EXISTS states (id INTEGER PRIMARY KEY AUTOINCREMENT, type TEXT NOT NULL, value TEXT UNIQUE)"
		     "CREATE TABLE IF NOT EXISTS rank_1_transitions (state_0 INTEGER, next_state INTEGER, occurences INTEGER DEFAULT 1,
                      PRIMARY KEY (state_0, next_state),
                      FOREIGN KEY (state_0, next_state) REFERENCES states(id, id))"))
      (sqlite:execute-non-query *database* query))))


(defun ensure-database-connection ()
  (unless *database*
    (open-database)
    (ensure-database-structure)))


(defun reset-database-connection ()
  (let ((database-path (slot-value *database* 'sqlite::database-path)))
    (close-database)
    (open-database database-path)
    (ensure-database-structure)))


(defun add-states-to-database (states)
  (loop :for state :in states
	:do (sqlite:execute-non-query *database*
				      "INSERT OR IGNORE INTO states (type, value) VALUES (?, ?)"
				      (state-type state)
				      (state-value state))))


(defun add-transitions-to-database (transitions)
  (loop :for (state-0 . next-state) :in transitions
	:do (sqlite:execute-non-query *database* "
INSERT INTO rank_1_transitions (state_0, next_state) 
  VALUES ((SELECT id FROM states WHERE value=?),
          (SELECT id FROM states WHERE value=?))
  ON CONFLICT DO UPDATE SET occurences=occurences+1"
				      (state-value state-0)
				      (state-value next-state))))


(defun make-transitions (state-sequence)
  (loop :for (state-0 state-1) :on state-sequence
	:when state-1
	  :collect (cons state-0 state-1)))


(defun commit-to-database (state-sequence)
  (sqlite:with-transaction *database*
    ;; first ensure all used states are in the database
    (add-states-to-database state-sequence)
    
    ;; then insert all transitions present in the input state sequence
    (add-transitions-to-database (make-transitions state-sequence))))

