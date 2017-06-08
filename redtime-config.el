;;; redtime-config.el --- config management

;;; Commentary:
;;;
;;

;;; Code:

(defvar redtime/hosts '()
  "List with Redmine hosts.")
(defvar redtime/tracker-location "~/.redtime"
  "Location to the file where currently tracked time is stored.")

(defvar redtime/host nil
  "Selected host of the redmine.")
(defvar redtime/api-key nil
  "Selected API key for the redmine.")
(defvar redtime/project-id nil
  "Selected project ID.")

(defun redtime-get-conf ()
  "Read Redmine host and api key from config variable.
Prompt user to select one host if there are multiple values."
  (when (not redtime/hosts)
    (error "Config var 'redtime/hosts is empty!"))
  (if (and redtime/host redtime/api-key)
      (cons redtime/host redtime/api-key)
    (if (eq (length redtime/hosts) 1)
        (car redtime/hosts)
      (call-interactively 'redtime-switch-host))))

(defun redtime-switch-host ()
  "Prompt to select redmine host from pre-defined config variable."
  (interactive)
  (let* ((completions redtime/hosts)
         (selected-host (completing-read "Select host:" completions nil t))
         (selected-key (redtime--lookup-completion selected-host
                                                   completions)))
    (cons selected-host selected-key)))

(defun redtime--lookup-completion (completion completions)
  "Lookup COMPLETION in COMPLETIONS and return issue-id."
  (cdr (assoc completion completions)))

(provide 'redtime-config)
;;; redtime-config.el ends here
