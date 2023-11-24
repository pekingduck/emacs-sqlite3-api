;;; sqlite3.el --- Direct access to the core SQLite3 API  -*- lexical-binding: t -*-

;; Copyright (C) 2018-2023  Y. N. Lo

;; Author: Y. N. Lo <elisp@fastmail.com>
;; Homepage: https://github.com/pekingduck/emacs-sqlite3-api
;; Keywords: comm, data, sql
;; Version: 0.17
;; Package-Requires: ((emacs "25.1"))

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; `sqlite3-api' is a dynamic module for GNU Emacs 25+ that provides
;; direct access to the core SQLite3 C API from Emacs Lisp.

;;; Code:

(require 'cl-lib)

(defvar sqlite3-api-build-command (or (getenv "SQLITE3_API_BUILD_COMMAND")
                                      "make all")
  "String containing the build command for the sqlite3-api module.

It defaults to \"make all\".  It can be overriden by
setting the SQLITE3_API_BUILD_COMMAND environment variable.")

(cl-eval-when (load eval)
  (unless (require 'sqlite3-api nil t)
    (if (or noninteractive
            (yes-or-no-p "sqlite3-api module must be built.  Do so now? "))
        (let ((default-directory (file-name-directory (or load-file-name
                                                          buffer-file-name))))
          (message "Building sqlite3-api module with %S" sqlite3-api-build-command)
          (with-temp-buffer
            (unless (zerop (call-process-shell-command
                            sqlite3-api-build-command nil t t))
              (error "Failed to compile module using: %s: %s"
                     sqlite3-api-build-command
                     (buffer-substring-no-properties
                      (point-min)
                      (point-max)))))
          (message "Loading sqlite3-api module...")
          (require 'sqlite3-api))
      (user-error "Abort"))))

(provide 'sqlite3)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; sqlite3.el ends here
