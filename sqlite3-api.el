;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; sqlite-* constants
(require 'sqlite3-api-constants)

;; Dynamic module
(require 'sqlite3-api-module nil t)

;; Error signals raised by the dynamic module
(define-error 'db-error "Database Error")
(define-error 'sql-error "SQL Error")

;; Install dynamic module
(defun sqlite3-api-install-dynamic-module ()
  "Install sqlite3-api-module from github."
  (interactive)
  (let ((log-buffer "*sqlite3-api-install-log*")
	(err-buffer "*sqlite3-api-install-error*")
	(tmp-dir (make-temp-file "sqlite3-api" t))
	(cur-dir default-directory)
	(repo-url "https://github.com/pekingduck/emacs-sqlite3-api")
	(tar))
    (unwind-protect
	(condition-case err
	    (progn
	      (when (get-buffer log-buffer)
		(kill-buffer log-buffer))
	      (when (get-buffer err-buffer)
		(kill-buffer err-buffer))
	      (cd tmp-dir)
	      (shell-command (format "git clone %s" repo-url)
			     log-buffer err-buffer)
	      (cd "emacs-sqlite3-api")
	      ;; compile the module and create a tar archive
	      (shell-command "make module" log-buffer err-buffer)
	      ;; A file called "MODULE" is created in the previous step
	      ;; and it contains the name of the tar file
	      (setq tar (with-temp-buffer
			  (insert-file-contents "MODULE")
			  (buffer-substring-no-properties
			   (point-min)
			   (1- (point-max)))))
	      (package-install-file tar)
	      (message "%s successfully installed" (file-name-base tar)))
	  (error
	   (message "Installation aborted: %S" err)))
      (delete-directory tmp-dir t)
      (cd cur-dir))))

(provide 'sqlite3-api)
