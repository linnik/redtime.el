;;; redtime.el --- track time to Redmine

;;; Commentary:
;;; Works best with flx package.
;;

;;; Code:
(require 'elmine)
(require 'redtime-config)
(require 'redtime-track)
(require 'redtime-report)

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

             (redmine-host redtime/host)
             (redmine-api-key redtime/api-key)
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

(defun redtime-manual ()
  "Manually track time for specific date."
  (interactive)
  (let* ((issue-id (redtime--ask-issue))
         (date (redtime--ask-manual-date))
         (hours (redtime--ask-manual-hours))
         (activity-id (redtime--ask-activity))
         (comment (redtime--ask-comment))

         (redmine-host redtime/host)
         (redmine-api-key redtime/api-key)
         (response (elmine/create-time-entry
                    :issue_id issue-id :activity_id activity-id
                    :spent_on date :hours hours :comments comment))
         (errors (plist-get response :errors)))
    (if errors
        (error (format "Error: %s" (car errors)))
      (message (format "Successfully tracked %s hours for issue #%s on %s"
                       hours issue-id date)))))

(defun redtime--ask-manual-date ()
  "Ask user to pick a date for manual time entry."
  (format-time-string "%Y-%m-%d" (org-time-string-to-time (org-read-date))))

(defun redtime--ask-manual-hours ()
  "Ask user to enter amount of spent hours for manual time entry."
  (read-string "Enter hours: "))

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
    (let ((redmine-host redtime/host)
          (redmine-api-key redtime/api-key))
      (setq redtime--activities-cache (elmine/get-time-entry-activities))))
  (let* ((completions (mapcar (lambda (obj) (cons
                                             (get-decode :name obj)
                                             (plist-get obj :id) ))
                              redtime--activities-cache))
         (selected (completing-read "Select activity: " completions nil t)))
    (cdr (assoc selected completions))))

(defun redtime--build-completions ()
  "Build completions list."
  (let ((redmine-host redtime/host)
        (redmine-api-key redtime/api-key))
    (mapcar 'redtime--build-completion (elmine/get-issues))))

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

;;; redtime.el ends here
