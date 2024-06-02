;;; term-manager-eat.el --- terminal management for eat -*- lexical-binding: t; -*-

;; Copyright (C) 2016-2024 Ivan Malison

;; Author: Ivan Malison <IvanMalison@gmail.com>
;; Keywords: projectile tools terminals vc eat
;; URL: https://www.github.com/IvanMalison/term-manager
;; Version: 0.1.0
;; Package-Requires: ((emacs "24") (term-manager "0.1.0") (eat "0.9.4"))

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

;; term-manager-eat adds eat support to term-manager

;;; Code:

(require 'term-manager)
(require 'eat)

(defun term-manager-eat-build-term (directory-symbol)
  (let* ((directory (symbol-name directory-symbol))
         (default-directory directory)
         (eat-buffer-name term-manager-temp-buffer-name))
    (message "creating in %s" default-directory)
    (eat--1 nil nil (lambda (buffer) nil))))

(cl-defmethod term-manager-enable-eat-buffer-renaming-and-reindexing ((tm term-manager))
  (advice-add 'eat--t-set-cwd :after
              (lambda (&rest _args)
                (term-manager-on-update-context tm))))
;;; term-manager-eat.el ends here
