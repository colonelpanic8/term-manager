;;; term-manager.el --- Contextual terminal management -*- lexical-binding: t; -*-

;; Copyright (C) 2016 Ivan Malison

;; Author: Ivan Malison <IvanMalison@gmail.com>
;; Keywords: term manager
;; URL: https://www.github.com/IvanMalison/term-manager
;; Version: 0.0.0

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

;; term-manager defines a projectile based terminal management system.

;;; Code:

(require 'eieio)
(require 'term-manager-indexed-mapping)

(defclass term-manager ()
  (buffer-index :initarg :buffer-index :initform
                (make-instance (term-manager-im)))
  (get-symbol :initarg :get-symbol :initform nil)
  (name-buffer :initarg :name-buffer :initform nil)
  (build-term :initarg :build-term :initform nil))

(defmethod term-manager-get-next-buffer-index ((tm term-manager)
                                               buffers &optional delta)
  (unless delta (setq delta 1))
  (let* ((the-current-buffer (current-buffer))
         (current-index (--find-index (eq it the-current-buffer) buffers)))
    (if current-index (mod (+ current-index delta) (length buffers)) 0)))

(defmethod term-manager-purge-dead-buffers ((tm term-manager) symbol)
  (cl-loop for buffer in (term-manager-im-index-get (oref tm :buffer-index) symbol)
           when (not (buffer-live-p buffer)) do
           (term-manager-im-delete (oref tm :buffer-index) buffer)))

(defmethod term-manager-get-buffer ((tm term-manager)
                                    &optional symbol delta)
  (interactive)
  (unless symbol (setq symbol (term-manager-get-symbol tm)))
  (let* ((buffers (progn
                    (term-manager-purge-dead-buffers tm symbol)
                    (term-manager-im-index-get (oref tm :buffer-index) symbol)))
         (next-buffer-index (term-manager-get-next-buffer-index tm buffers delta)))
    (if buffers (nth next-buffer-index buffers)
      (term-manager-build-term tm symbol))))

(defmethod term-manager-get-symbol ((tm term-manager) &optional buffer)
  (unless buffer (setq buffer (current-buffer)))
  (with-current-buffer buffer
    (funcall (oref tm :get-symbol))))

(defmethod term-manager-build-term ((tm term-manager) &optional symbol)
  (unless symbol (setq symbol (term-manager-get-symbol tm)))
  (funcall (oref tm :build-term) symbol))

(defun term-manager-default-name-buffer (buffer symbol)
  (format "term - %s" symbol))

(defmethod term-manager-get-buffer-name ((tm term-manager) &optional buffer)
  (let ((name-buffer (or (oref tm :name-buffer) 'term-manager-default-name-buffer)))
    (unless buffer (setq buffer (current-buffer)))
    (funcall name-buffer buffer)))

(defmethod term-manager-rename-buffer ((tm term-manager) &optional buffer)
  (rename-buffer (term-manager-get-buffer-name tm) t))

(defmethod term-manager-update-index-for-buffer ((tm term-manager)
                                                 &optional buffer new-symbol)
  (unless buffer (setq buffer (current-buffer)))
  (unless new-symbol (setq new-symbol (term-manager-get-symbol tm buffer)))
  (term-manager-im-maybe-put (oref tm :buffer-index) buffer new-symbol))

(defmethod term-manager-enable-buffer-renaming-and-reindexing ((tm term-manager))
  (advice-add 'term-handle-ansi-terminal-messages :after
              (lambda (&rest args)
                (term-manager-on-update-context tm))))

(defmethod term-manager-on-update-context ((tm term-manager) &optional buffer)
  (term-manager-update-index-for-buffer tm buffer)
  (term-manager-rename-buffer buffer))

(provide 'term-manager)
;;; term-manager.el ends here
