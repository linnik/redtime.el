;;; redtime-api.el --- api functionality

;;; Commentary:
;;;
;;

;;; Code:
(require 'elmine)

(defun get-decode (key object)
  "Get and decode KEY from OBJECT."
  (decode-coding-string (plist-get object key) 'utf-8))

(defun redtime/get-user (id)
  "Get a specific user by ID."
  (elmine/api-get :user (format "/users/%s.json" id)))

(defun redtime/get-project-memberships (id)
  "Get a list of users in project ID."
  (elmine/api-get-all :memberships (format "/projects/%s/memberships.json" id)))

(defun redtime/get-time-entries (&rest filters)
  "Get a list of time entries, with optional FILTERS properties."
  (let* ((response-object
          (apply #'elmine/api-get nil "/time_entries.json" filters))
         ;; (offset (elmine/get response-object :offset))
         ;; (limit (elmine/get response-object :limit))
         ;; (total-count (elmine/get response-object :total_count))
         (issue-list (elmine/get response-object :time_entries)))
    issue-list))

(provide 'redtime-api)
;;; redtime-api.el ends here
