;;; term-manager.el --- Contextual terminal management -*- lexical-binding: t; -*-

;; Copyright (C) 2016 Ivan Malison

;; Author: Ivan Malison <IvanMalison@gmail.com>
;; Keywords: terminals tools
;; URL: https://www.github.com/IvanMalison/term-manager
;; Version: 0.1.1
;; Package-Requires: ((dash "2.12.0") (emacs "24.4"))

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

;; term-manager manages terminals in groups based on contextual values.

;;; Code:

(require 'cl-lib)
(require 'dash)
(require 'eieio)
(require 'term)
(require 'term-manager-indexed-mapping)

(defgroup term-manager ()
  "Frames minor mode."
  :group 'term-manager
  :prefix "term-manager-")

(defcustom term-manager-display-buffer-alist nil
  "Alist provided to `display-buffer' by `term-manager-display-buffer'."
  :group 'term-manager
  :type '(alist :key-type symbol :value-type sexp))

(defun term-manager-display-buffer (buffer)
  (display-buffer buffer term-manager-display-buffer-alist)
  (let* ((window (get-buffer-window buffer t))
         (frame (window-frame window)))
    (when window
      (select-window window)
      (select-frame-set-input-focus frame))))

(defclass term-manager ()
  ((buffer-index :initarg :buffer-index :initform
                 (make-instance term-manager-im))
   (get-symbol :initarg :get-symbol :initform nil)
   (name-buffer :initarg :name-buffer :initform nil)
   (build-term :initarg :build-term :initform nil)))

(cl-defmethod term-manager-get-next-buffer-index ((_tm term-manager)
                                                  buffers &optional (delta 1))
  (unless delta (setq delta 1))
  (let* ((the-current-buffer (current-buffer))
         (current-index (--find-index (eq it the-current-buffer) buffers)))
    (if (and current-index buffers)
        (mod (+ current-index delta) (length buffers))
      0)))

(defmethod term-manager-purge-dead-buffers ((tm term-manager) &optional symbol)
  (let ((buffers (if symbol
                    (term-manager-im-index-get (oref tm buffer-index) symbol)
                  (term-manager-get-all-buffers tm))))
    (cl-loop for buffer in buffers
             when (not (buffer-live-p buffer))
             do (term-manager-im-delete (oref tm buffer-index) buffer))))

(cl-defmethod term-manager-get-buffer ((tm term-manager) &optional
                                       symbol
                                       (buffer-index nil get-buffer-index))
  (unless symbol (setq symbol (term-manager-get-symbol tm)))
  (let* ((buffers (term-manager-get-terms tm symbol))
         (the-buffer-index
          (if get-buffer-index buffer-index
            (term-manager-get-next-buffer-index tm buffers))))
    (if buffers (nth the-buffer-index buffers)
      (term-manager-build-term tm symbol))))

(cl-defmethod term-manager-get-terms ((tm term-manager) &optional
                                      (symbol (term-manager-get-symbol tm)))
  (term-manager-purge-dead-buffers tm symbol)
  (term-manager-im-index-get (oref tm buffer-index) symbol))

(cl-defmethod term-manager-display-term
    ((tm term-manager) &key (symbol (term-manager-get-symbol tm)) (delta 1))
  (when (stringp symbol)
    (setq symbol (intern symbol)))
  (let* ((buffers (term-manager-get-terms tm symbol))
         (next-buffer-index
          (term-manager-get-next-buffer-index tm buffers delta))
         (target-buffer (term-manager-get-buffer tm symbol next-buffer-index)))
    (term-manager-display-buffer target-buffer)))

(cl-defmethod term-manager-get-symbol ((tm term-manager) &optional
                                       (buffer (current-buffer)))
  (with-current-buffer buffer
    (funcall (oref tm get-symbol) buffer)))

(defun term-manager-default-build-term (directory-symbol)
  (let* ((directory (symbol-name directory-symbol))
         (default-directory directory)
         (program (getenv "SHELL"))
         (buffer (get-buffer
                  ;; We need to use a name that is guaranteed to be
                  ;; unique so that term-ansi-make-term always makes a
                  ;; new term.
                  (term-ansi-make-term "new term temp name" program))))
    (with-current-buffer buffer
      (term-mode)
      (term-char-mode))
    buffer))

(cl-defmethod term-manager-build-term ((tm term-manager) &optional
                                       (symbol (term-manager-get-symbol tm)))
  (unless symbol (setq symbol (term-manager-get-symbol tm)))
  (let* ((build-term (or (oref tm build-term)
                         'term-manager-default-build-term))
         (buffer (funcall build-term symbol)))
    (term-manager-on-update-context tm buffer)
    buffer))

(defun term-manager-default-name-buffer (_buffer symbol)
  (format "term - %s" symbol))

(cl-defmethod term-manager-get-buffer-name ((tm term-manager) &optional
                                            (buffer (current-buffer)))
  (let ((name-buffer (or (oref tm name-buffer)
                         'term-manager-default-name-buffer)))
    (funcall name-buffer buffer (term-manager-get-symbol tm buffer))))

(cl-defmethod term-manager-rename-buffer ((tm term-manager) &optional
                                          (buffer (current-buffer)))
  (with-current-buffer buffer
    (rename-buffer (term-manager-get-buffer-name tm buffer) t)))

(cl-defmethod term-manager-update-index-for-buffer
    ((tm term-manager) &optional (buffer (current-buffer))
     (symbol (term-manager-get-symbol tm)))
  (term-manager-im-maybe-put (oref tm buffer-index) buffer symbol))

(defmethod term-manager-enable-buffer-renaming-and-reindexing ((tm term-manager))
  (advice-add 'term-handle-ansi-terminal-messages :after
              (lambda (&rest _args)
                (term-manager-on-update-context tm))))

(cl-defmethod term-manager-on-update-context ((tm term-manager) &optional
                                              (buffer (current-buffer)))
  (term-manager-update-index-for-buffer tm buffer)
  (term-manager-rename-buffer tm buffer))

(defmethod term-manager-get-all-buffers ((tm term-manager) &optional symbol)
  (cl-loop for (buffer _) in (term-manager-im-pairs (oref tm buffer-index) symbol)
           collect buffer))

(cl-defmethod term-manager-get-next-global-buffer
    ((tm term-manager) &key (buffer-order-function 'identity) (delta 1)
     (_current-buffer (current-buffer)))
  (term-manager-purge-dead-buffers tm)
  (let* ((all-buffers (term-manager-get-all-buffers tm))
         (ordered-buffers (funcall buffer-order-function all-buffers))
         (next-buffer-index
          (term-manager-get-next-buffer-index tm ordered-buffers delta)))
    (if ordered-buffers
        (nth next-buffer-index ordered-buffers)
      (term-manager-build-term tm (intern default-directory)))))

(provide 'term-manager)
;;; term-manager.el ends here
