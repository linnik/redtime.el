;;; redtime.el --- track time to Redmine

;;; Commentary:
;;; Works best with flx package.
;;

;;; Code:
(require 'elmine)

(defvaralias 'redtime/host 'elmine/host)
(defvaralias 'redtime/api-key 'elmine/api-key)

(defvar redtime/tracker-location "~/.redtime"
  "Location to the file where stored currently tracked time.")

(defvar redtime--activities-cache nil
  "Private var with activities cache for current Redmine project.")


(defun redtime-start ()
  "Start tracking time."
  (interactive)
  (when (redtime--tracking-p)
    (error "Already tracking"))
  (let ((issue-id (redtime--ask-issue)))
    (redtime--write-time issue-id)))

(defun redtime-stop ()
  "Stop tracking time."
  (interactive)
  (unless (redtime--tracking-p)
    (error "Not tracking"))
  (let* ((time-entry (redtime--read-time))
         (issue-id (car time-entry))
         (issue-start-seconds (cdr time-entry))
         (spent-seconds (- (float-time) issue-start-seconds))
         (spent-hours (redtime--fmt-hours spent-seconds))
         ;; (spent-time (redtime--humanify-seconds spent-seconds))
         (message (format "Spent %s on issue #%s, submit?" spent-hours issue-id)))
    (when (y-or-n-p message)
      (let* ((activity-id (redtime--ask-activity))
             (comment (redtime--ask-comment))
             (response (elmine/create-time-entry
                        :issue_id issue-id :activity_id activity-id
                        :hours spent-hours :comments comment))
             (errors (plist-get response :errors)))
        (if errors
            (error (format "Error: %s" (car errors)))
          (redtime--close-time))))))

(defun redtime-discard ()
  "Stop and discard tracked time."
  (interactive)
  (unless (redtime--tracking-p)
    (error "Not tracking"))
  (let* ((time-entry (redtime--read-time))
         (issue-id (car time-entry))
         (issue-start-seconds (cdr time-entry))
         (spent-seconds (- (float-time) issue-start-seconds))
         (spent-string (redtime--humanify-seconds spent-seconds))
         (message (format "Discard %s for issue #%s?" spent-string issue-id)))
    (when (y-or-n-p message)
      (redtime--close-time))))

(defun redtime--ask-issue ()
  "Ask user to select issue from given list of suggestions."
  (let* ((completions (redtime--build-completions))
         (selected (completing-read "Select issue:" completions nil t)))
    (redtime--lookup-completion selected completions)))

(defun redtime--ask-comment ()
  "Ask user to write a comment on time entry."
  (let ((comment (read-string "Enter comment: ")))
    (if (> (length comment) 255)
        (error "Comment is too long")
      comment)))

(defun redtime--ask-activity ()
  "Ask user to select activity."
  (unless redtime--activities-cache
    (setq redtime--activities-cache (elmine/get-time-entry-activities)))
  (let* ((completions (mapcar (lambda (obj) (cons
                                             (get-decode :name obj)
                                             (plist-get obj :id) ))
                              redtime--activities-cache))
         (selected (completing-read "Select activity: " completions nil t)))
    (cdr (assoc selected completions))))

(defun redtime--build-completions ()
  "Build completions list."
  (mapcar 'redtime--build-completion (elmine/get-issues)))

(defun redtime--lookup-completion (completion completions)
  "Lookup COMPLETION in COMPLETIONS and return issue-id."
  (cdr (assoc completion completions)))

(defun redtime--build-completion (issue)
  "Build single completion entry from ISSUE object."
  (let* ((issue-id (plist-get issue :id))
         (subject (get-decode :subject issue))
         (issue-string (format "#%s %s" issue-id subject)))
    (cons issue-string issue-id)))

(defun get-decode (key object)
  "Get and decode KEY from OBJECT."
  (decode-coding-string (plist-get object key) 'utf-8))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions for time tracking ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
        (end nil) (append nil) (lockname nil) (visit :no-message) ;
        )
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

;;; redtime.el ends here
