;;; redtime-track.el --- time tracking functionality

;;; Commentary:
;;;
;;

;;; Code:
(require 'redtime-config)

(defun redtime--tracking-p ()
  "Predicate to check if currently tracking any issue."
  (file-exists-p redtime/tracker-location))

(defun redtime--close-time ()
  "Remove any persisted time records."
  (when (file-exists-p redtime/tracker-location)
    (delete-file redtime/tracker-location)))

(defun redtime--write-time (issue-id)
  "Write current time with ISSUE-ID to a file."
  (let ((string (format "%d %s\n" issue-id (float-time)))
        (filename redtime/tracker-location) (mustbenew t)
        (end nil) (append nil) (lockname nil) (visit :no-message))
    (write-region string end filename append visit lockname mustbenew)))

(defun redtime--read-time ()
  "Read time of currently tracking issue ."
  (redtime--parse-entry (read-file redtime/tracker-location)))

(defun read-file (location)
  "Read file at LOCATION."
  (with-temp-buffer
    (insert-file-contents location)
    (buffer-substring-no-properties
     (point-min)
     (point-max))))

(defun redtime--parse-entry (string)
  "Parse STRING into '(issue-id . issue-float-time) list."
  (let* ((content (delete "" (split-string string)))
         (issue-id (car content))
         (issue-raw-time (cadr content))
         (issue-float-time (string-to-number issue-raw-time)))
    (cons issue-id issue-float-time)))

(defun redtime--humanify-seconds (seconds)
  "Formats SECONDS for humans."
  (format-seconds "%Y, %D, %H, %M, %z%S" seconds))

(defun redtime--fmt-hours (seconds)
  "Format SECONDS to Redmine hours format."
  (format-seconds "%h:%m" seconds))


(provide 'redtime-track)
;;; redtime-track.el ends here
