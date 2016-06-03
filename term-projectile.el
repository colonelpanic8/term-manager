;;; term-projectile.el --- projectile terminal management -*- lexical-binding: t; -*-

;; Copyright (C) 2016 Ivan Malison

;; Author: Ivan Malison <IvanMalison@gmail.com>
;; Keywords: term manager projectile
;; URL: https://www.github.com/IvanMalison/term-projectile
;; Version: 0.0.0
;; Package-Requires: ((term-manager "0.0.0") (projectile "0.13.0"))

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

;; term-projectile defines a projectile based terminal management system.

;;; Code:

(require 'term-manager)

(defun term-projectile ()
  (interactive)
  (let ((manager
         (make-instance term-manager
                        :get-symbol 'term-projectile-get-symbol-for-buffer)))
    (term-manager-enable-buffer-renaming-and-reindexing manager)
    manager))

(defun term-projectile-get-symbol-for-buffer (buffer)
  (intern (with-current-buffer buffer
            (if (derived-mode-p 'term-mode)
                default-directory
              (let ((projectile-require-project-root nil))
                (projectile-project-root))))))

(defconst term-projectile-term-manager (term-projectile))

(defun term-projectile-switch (&optional delta directory)
  (term-manager-switch-to-buffer
   term-projectile-term-manager directory delta))

(defun term-projectile-forward ()
  (interactive)
  (term-projectile-switch 1))

(defun term-projectile-backward ()
  (interactive)
  (term-projectile-switch -1))

(defun term-projectile-new-at-default-directory ()
  (term-projectile-switch nil default-directory))

(defun term-projectile-create-new (&optional directory)
  (interactive)
  (switch-to-buffer
   (term-manager-build-term term-projectile-term-manager directory)))

(provide 'term-projectile)
;;; term-projectile.el ends here
