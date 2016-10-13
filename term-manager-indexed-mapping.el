;;; term-manager-indexed-mapping.el --- -*- lexical-binding: t; -*-

;; Copyright (C) 2016 Ivan Malison

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; term-manager-indexed-mapping defines a mapping object with an
;; index of values to keys.

;;; Code:

(require 'dash)
(require 'eieio)

(defun term-manager-plist-delete (plist property)
  "Mutate PLIST in place so that it does not contain PROPERTY.
This is in contrast to merely setting it to 0."
  (let (p)
    (while plist
      (if (not (eq property (car plist)))
          (setq p (plist-put p (car plist) (nth 1 plist))))
      (setq plist (cddr plist)))
    p))

(defclass term-manager-im ()
  ((mapping :initarg :mapping :initform nil)
   (index :initarg :index :initform nil)))

(defmethod term-manager-im-get ((im term-manager-im) key)
  (plist-get (oref im mapping) key))

(defmethod term-manager-im-index-get ((im term-manager-im) value)
  (plist-get (oref im index) value))

(defmethod term-manager-im-index-get-one ((im term-manager-im) value)
  (let ((keys (plist-get (oref im index) value)))
    (when keys (car keys))))

(defmethod term-manager-im-maybe-put ((im term-manager-im) key value)
  (unless (equal (term-manager-im-get im key) value)
    (term-manager-im-put im key value)))

(defmethod term-manager-im-put ((im term-manager-im) key value)
  ;; Handle removing the key from where it is currently indexed
  (term-manager-im-unindex im key)
  ;; Add the key to its new position in the index
  (oset im :index
        (plist-put (oref im index)
                   value (cons key (plist-get (oref im index) value))))
  ;; Add the key, value pair to the mapping
  (oset im :mapping
        (plist-put (oref im mapping) key value)))

(defmethod term-manager-im-unindex ((im term-manager-im) key)
  (let* ((current-value (plist-get (oref im mapping) key))
         (value-list (plist-get (oref im index) current-value)))
    (when value-list
      (setq value-list (remove key value-list))
      (oset im :index
            (plist-put (oref im index)
                       current-value value-list)))))

(defmethod term-manager-im-delete ((im term-manager-im) key)
  (term-manager-im-unindex im key)
  (oset im :mapping
        (term-manager-plist-delete (oref im mapping) key)))

(defmethod term-manager-im-pairs ((im term-manager-im) &optional symbol)
  (if symbol
      (let ((values (term-manager-im-index-get (oref im index) symbol)))
        (cl-loop for value in values
                 collect (list symbol value)))
    (-partition 2 (oref im mapping))))

(provide 'term-manager-indexed-mapping)
;;; term-manager-indexed-mapping.el ends here
