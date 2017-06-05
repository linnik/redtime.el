;;; redtime-project.el --- project management

;;; Commentary:
;;;
;;

;;; Code:
(require 'redtime-config)

(defun redtime-get-project-id ()
  "Get currently selected project ID.  Prompt user if needed."
  (when (not redtime/project-id)
    (setq redtime/project-id (call-interactively 'redtime-switch-project)))
  redtime/project-id)

(defun redtime-switch-project ()
  "Switch current Redmine project."
  (interactive)
  (let* ((completions (redtime--project-completions))
         (selected (completing-read "Select project:" completions nil t)))
    (redtime--lookup-completion selected completions)))

(defun redtime--project-completions ()
  "Build completions list."
  (let* ((redmine-conf (redtime-get-conf))
         (redmine-host (car redmine-conf))
         (redmine-api-key (cdr redmine-conf)))
    (mapcar 'redtime--build-project-completion (elmine/get-projects))))

(defun redtime--build-project-completion (project)
  "Build single completion entry from PROJECT object."
  (let* ((project-id (plist-get project :id))
         (project-name (get-decode :name project)))
    (cons project-name project-id)))

(provide 'redtime-project)
;;; redtime-project.el ends here
