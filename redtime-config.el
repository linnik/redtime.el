;;; redtime-config.el --- config

;;; Commentary:
;;;
;;

;;; Code:

(defvar redtime/host nil
  "The default host of the redmine.")
(defvar redtime/api-key nil
  "The default API key for the redmine.")
(defvar redtime/project-id 2
  "Id of selected project.")  ;; temporarily hardcoded
(defvar redtime/tracker-location "~/.redtime"
  "Location to the file where currently tracked time is stored.")

(provide 'redtime-config)
;;; redtime-config.el ends here
