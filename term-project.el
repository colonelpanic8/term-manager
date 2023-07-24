;;; term-project.el --- Terminal management for project.el -*- lexical-binding: t; -*-

;; Copyright (C) 2016-2023 Ivan Malison
;; Copyright (C) 2023 ROCKTAKEY

;; Author: Ivan Malison <IvanMalison@gmail.com>
;;         ROCKTAKEY <rocktakey@gmail.com>
;; Keywords: project tools terminals vc
;; URL: https://www.github.com/IvanMalison/term-manager
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1") (term-manager "0.1.0"))

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

;; term-project defines a project based terminal management system.

;;; Code:

(require 'project)
(require 'term-manager)

(defvar term-project-global-directory "~")

(defun term-project ()
  "Make a new term-manger instance configured for project usage."
  (interactive)
  (let ((manager
         (make-instance 'term-manager
                        :get-symbol 'term-project-get-symbol-for-buffer)))
    (term-manager-enable-buffer-renaming-and-reindexing manager)
    manager))

(defun term-project-maybe-intern (value)
  "Return `intern'ed value when VALUE is non-nil, or return nil otherwise."
  (when value (intern value)))

(defun term-project-get-symbol-for-buffer (buffer)
  "Get symbol which express project root of BUFFER."
  (term-project-maybe-intern (with-current-buffer buffer
            (if (derived-mode-p 'term-mode)
                ;; If we are in a term-mode buffer we should always associate
                ;; with default-directory because we don't want buffers that
                ;; were started in a project but moved to continue to be
                ;; considered as directories of that project.
                default-directory
              (project-root (project-current))))))

(defconst term-project-term-manager (term-project))

(defun term-project-switch (&rest args)
  "Switch terms in the current project.
ARGS is passed to `term-manager-display-term'."
  (apply 'term-manager-display-term term-project-term-manager args))

(defun term-project-global-switch (&rest args)
  "Switch terms globally.
ARGS is passed to `term-manager-get-next-global-buffer'."
  (let ((default-directory term-project-global-directory))
    (term-manager-display-buffer (apply 'term-manager-get-next-global-buffer
                                        term-project-term-manager args))))

(defun term-project-get-all-buffers ()
  "Get all `term-project' buffers in the current project."
  (term-manager-purge-dead-buffers term-project-term-manager)
  (term-manager-get-all-buffers term-project-term-manager))

(defun term-project-select-existing ()
  "Select `project-term' buffer in the current project."
  (completing-read "Select a term buffer: "
                   (mapcar 'buffer-name
                           (term-project-get-all-buffers))))

;;;###autoload
(defun term-project-switch-to ()
  "Switch to an existing `term-project' buffer using `completing-read'."
  (interactive)
  (term-manager-display-buffer (term-project-select-existing)))

;;;###autoload
(defun term-project-forward ()
  "Switch forward to the next `term-project' `ansi-term' buffer.
Make a new one if none exists."
  (interactive)
  (term-project-switch :symbol (term-project-maybe-intern (project-root (project-current)))))

;;;###autoload
(defun term-project-backward ()
  "Switch backward to the next `term-project' `ansi-term' buffer.
Make a new one if none exists."
  (interactive)
  (term-project-switch :delta -1 :symbol (term-project-maybe-intern (project-root (project-current)))))

;;;###autoload
(cl-defun term-project-create-new (&optional (directory (project-root (project-current))))
  "Make a new `ansi-term' buffer for DIRECTORY.
If directory is nil, use the current project project"
  (interactive)
  (when (stringp directory) (setq directory (term-project-maybe-intern directory)))
  (term-manager-display-buffer
   (term-manager-build-term term-project-term-manager directory)))

;;;###autoload
(defun term-project-default-directory-forward ()
  "Switch to the next `term-project' `ansi-term' buffer for `defualt-directory'."
  (interactive)
  (term-project-switch :symbol default-directory))

;;;###autoload
(defun term-project-default-directory-backward ()
  "Switch to the prev `term-project' `ansi-term' buffer for `defualt-directory'."
  (interactive)
  (term-project-switch :delta -1 :symbol default-directory))

;;;###autoload
(defun term-project-default-directory-create-new ()
  "Make a new `ansi-term' buffer in `default-directory'."
  (interactive)
  (let ((directory (if (stringp default-directory) (term-project-maybe-intern default-directory))))
    (term-project-create-new directory)))

;;;###autoload
(defun term-project-global-forward ()
  "Switch forward to the next `term-project' `ansi-term' buffer.
Make a new one if none exists."
  (interactive)
  (term-project-global-switch :delta 1))

;;;###autoload
(defun term-project-global-backward ()
  "Switch backward to the next `term-project' `ansi-term' buffer.
Make a new one if none exists."
  (interactive)
  (term-project-global-switch :delta -1))

;;;###autoload
(defun term-project-global-create-new ()
  "Make a new `ansi-term' buffer in `term-project-global-directory'."
  (interactive)
  (term-project-create-new term-project-global-directory))

(provide 'term-project)
;;; term-project.el ends here
