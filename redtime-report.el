;;; redtime-report.el --- report functionality

;;; Commentary:
;;;
;;

;;; Code:
(require 'cl-lib)
(require 'redtime-config)
(require 'redtime-api)
(require 'redtime-project)

(defvar redtime-report-columns
  '(:date :user-name :activity :issue :hours :comment)
  "Columns for report table.
You can customize report table content
by reordering or removing keywords in this list.")

(defvar redtime-report-limit 42
  "Maximum number of time entries to display in reports table.")

(defvar redtime-report-sorting-field :date
  "Default sorting field in reports table.")

(defvar redtime-report-sorting-order :descending
  "Default sorting order in reports table.  Either :descending or :ascending.")

(defvar redtime--report-column-labels
  (list :date "Date"
        :user-id "User ID"
        :user-name "Username"
        :activity "Activity"
        :issue "Issue"
        :hours "Hours"
        :comment "Comments")
  "Labels which will be used in table header.  Order is ignored here.")

(defvar redtime--report-cache nil
  "Contents of report table.")

(defvar redtime--report-user nil
  "Currently selected user.")

(defvar redtime--report-sort
  `(,redtime-report-sorting-field . ,redtime-report-sorting-order)
  "Currenty selected sorting method.")

(defvar *redtime-buffer-name* "*Redtime*"
  "Name of the redtime buffer.")

(defvar redtime-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "s a") #'redtime-report-sort-by-activity)
    (define-key map (kbd "s d") #'redtime-report-sort-by-date)
    (define-key map (kbd "s u") #'redtime-report-sort-by-username)
    (define-key map (kbd "s i") #'redtime-report-sort-by-issue)
    (define-key map (kbd "s h") #'redtime-report-sort-by-hours)
    (define-key map (kbd "f u") #'redtime-report-filter-by-user)
    (define-key map (kbd "f x") #'redtime-report-filters-clear)
    ;; (define-key map (kbd "m") #'redtime-report-select-month)
    ;; (define-key map (kbd "d") #'redtime-report-select-day)
    (define-key map (kbd "g") #'redtime-update-buffer)
    (define-key map (kbd "q") #'redtime-quit)
    (define-key map (kbd "?") #'describe-mode)
    map)
  "Keymap for `redtime-mode'.")

(defmacro redtime--with-writable-buffer (&rest body)
  "Evaluate BODY as if the current buffer was not in `read-only-mode'."
  (declare (indent 0) (debug t))
  `(let ((inhibit-read-only t))
     ,@body))

(defmacro redtime--save-point (&rest body)
  "Evaluate BODY and restore the point.
Similar to `save-excursion' but only restore the point."
  (declare (indent 0) (debug t))
  (let ((point (make-symbol "point")))
    `(let ((,point (point)))
       ,@body
       (goto-char (min ,point (point-max))))))

(define-derived-mode redtime-mode read-only-mode "Redtime"
  "Major mode for tracking time with Redmine

\\{redtime-mode-map}"
  (setq truncate-lines t))

(defun redtime-quit ()
  "Kill the buffer quitting the window."
  (interactive)
  (setq redtime--report-cache nil)
  (quit-window t))

(defun redtime--reset-sorting (&optional key)
  "Reset reports table sorting method to default column-direction pair.
Specify KEY for resetting direction on specific column."
  (unless key
    (setq key redtime-report-sorting-field))
  (setq redtime--report-sort `(,key . ,redtime-report-sorting-order)))

(defun redtime--swap-sort-direction (key)
  "Swap sort direction (:ascending or :descending) for KEY column."
  (let ((sort-key (car redtime--report-sort))
        (sort-order (cdr redtime--report-sort)))
    (if (eq sort-key key)
        (setq sort-order (if (eq sort-order :descending)
                             :ascending
                           :descending))
      (setq sort-key key)
      (setq sort-order redtime-report-sorting-order))
    (setq redtime--report-sort`(,sort-key . ,sort-order))))

(defun redtime-report-sort (key)
  "Perform sort operation on KEY column of reports table."
  (redtime--swap-sort-direction key)
  (let* ((sort-key (car redtime--report-sort))
         (sort-order (cdr redtime--report-sort))
         (sort-func (if (not (member sort-key '(:issue :hours)))
                        'string-lessp
                      '<)))
    (setq redtime--report-cache
          (cl-sort redtime--report-cache sort-func
                   :key #'(lambda (line) (plist-get line sort-key))))
    (when (eq sort-order :descending)
      (setq redtime--report-cache (reverse redtime--report-cache))))
  redtime--report-cache)

(defun redtime-report-sort-by-date ()
  "Sort reports table by date."
  (interactive)
  (redtime-report-sort :date)
  (redtime-update-buffer))

(defun redtime-report-sort-by-activity ()
  "Sort reports table by activity."
  (interactive)
  (redtime-report-sort :activity)
  (redtime-update-buffer))

(defun redtime-report-sort-by-username ()
  "Sort reports table by username."
  (interactive)
  (redtime-report-sort :user-name)
  (redtime-update-buffer))

(defun redtime-report-sort-by-issue ()
  "Sort reports table by issue."
  (interactive)
  (redtime-report-sort :issue)
  (redtime-update-buffer))

(defun redtime-report-sort-by-hours ()
  "Sort reports table by hours."
  (interactive)
  (redtime-report-sort :hours)
  (redtime-update-buffer))

(defun prefix-numeric-nullable-value (val &optional default)
  "Same as 'prefix-numeric-value, but in case if VAL is nil, return DEFAULT."
  (if (not (null val))
      (prefix-numeric-value val)
    default))

(defun redtime-pop ()
  "Pop redtime buffer."
  (let* ((buffer-read-only t)
         (buf (get-buffer-create *redtime-buffer-name*)))
    (pop-to-buffer buf)
    (unless (eq major-mode 'redtime-mode)
      (redtime-mode))))

(defun redtime-last-entries (&optional N)
  "List last time entries.
Accepts numeric prefix argument N, which limits amount of entries to fetch."
  (interactive "P")
  (redtime-pop)
  (let ((limit (prefix-numeric-nullable-value N redtime-report-limit)))
    (redtime--report-build-cache `(:limit ,limit))
    (redtime-update-buffer)))

(defun redtime-day-entries (&optional N)
  "List time entries for a specific day (prompts user).
Accepts numeric prefix argument N, which limits amount of entries to fetch."
  (interactive "P")
  (redtime-pop)
  (let ((limit (prefix-numeric-nullable-value N redtime-report-limit))
        (spent_on (format-time-string
                   "%Y-%m-%d" (org-time-string-to-time (org-read-date)))))
    (redtime--report-build-cache `(:spent_on ,spent_on :limit ,limit))
    (redtime-update-buffer)))

(defun redtime-update-buffer ()
  "Update the current buffer contents."
  (interactive)
  (redtime--save-point
    (redtime--with-writable-buffer
      (delete-region (point-min) (point-max))
      (let ((table-rows (redtime--report)))
        (dolist (elt table-rows )
          (insert elt)))
      (org-table-align))))

(defun redtime--report ()
  "Build report table."
  (let ((table '()))
    (add-to-list 'table (redtime--report-header) t)
    ;; (mapcar #'(lambda (val) (add-to-list 'table val t)) (redtime--report-body))
    (mapcar
     #'(lambda (value) (add-to-list 'table (redtime--report-row value) t))
     redtime--report-cache)
    (add-to-list 'table (redtime--report-footer) t)))

(defun redtime--report-header ()
  "Return report header."
  (let ((labels (mapcar
                 (apply-partially 'plist-get redtime--report-column-labels)
                 redtime-report-columns)))
    (concat "|-" "\n"
            "|"  (mapconcat 'identity labels "|") "|" "\n"
            "|-" "\n")))

(defun redtime--report-body ()
  "Construct report body."
  (mapcar 'redtime--report-row redtime--report-cache))

(defun redtime--report-footer ()
  "Construct report footer."
  "|-\n")

(defun redtime--report-row (row)
  "Construct single report table ROW."
  (let ((row-cells (mapcar
                    (apply-partially 'redtime--report-fmt-cell row)
                    redtime-report-columns)))
    (concat "|" (mapconcat 'identity row-cells "|") "|\n")))

(defun redtime--report-fmt-cell (row column-name)
  "Format single table cell from ROW COLUMN-NAME."
  (let ((value (plist-get row column-name)))
    (if (member column-name '(:issue :hours))
        (number-to-string value)
      value)))

(defun redtime--report-process (object)
  "Extract required data from Redmine response OBJECT."
  (let* ((activity (plist-get object :activity))
         (issue (plist-get object :issue))
         (user (plist-get object :user)))
    (list :date (plist-get object :spent_on)
          :activity (plist-get activity :name)
          :issue (plist-get issue :id)
          :user-id (plist-get user :id)
          :user-name (get-decode :name user)
          :hours (plist-get object :hours)
          :comment (get-decode :comments object))))

(defun redtime--report-build-cache (&optional filters)
  "Fetch time entries from Redmine, with optional FILTERS."
  (let* ((redmine-conf (redtime-get-conf))
         (redmine-host (car redmine-conf))
         (redmine-api-key (cdr redmine-conf))
         (offset 0) (limit redtime-report-limit)
         (given-filters filters)
         (filters `(:limit ,limit :offset ,offset))
         (entries nil))
    (unless (null redtime--report-user)
      (setq filters (plist-put filters :user_id redtime--report-user)))
    (when given-filters
      (cl-loop for (key value) on filters by #'cddr
               do (setq filters (plist-put filters key value))))
    (setq entries (apply 'redtime/get-time-entries filters))
    (redtime--reset-sorting)
    (setq redtime--report-cache (mapcar 'redtime--report-process entries))))

(defun redtime-report-filter-by-user ()
  "Show report table entries only for selected user."
  (interactive)
  (setq redtime--report-user (redtime--report-ask-user))
  (redtime--report-build-cache)
  (redtime-update-buffer))

(defun redtime-report-filters-clear ()
  "Clear reports table of any applied filters."
  (interactive)
  (setq redtime--report-user nil)
  (redtime--report-build-cache)
  (redtime-update-buffer))

(defun redtime--report-ask-user ()
  "Prompt to select redtime user from given list of suggestions."
  (let* ((completions (redtime--user-completions))
         (selected (completing-read "Select user:" completions nil t)))
    (redtime--lookup-user-completion selected completions)))

(defun redtime--user-completions ()
  "Build completions list."
  (let* ((project-id (redtime-get-project-id))
         (redmine-conf (redtime-get-conf))
         (redmine-host (car redmine-conf))
         (redmine-api-key (cdr redmine-conf)))
    (mapcar 'redtime--build-user-completion
            (redtime/get-project-memberships project-id))))

(defun redtime--lookup-user-completion (completion completions)
  "Lookup COMPLETION in COMPLETIONS and return issue-id."
  (cdr (assoc completion completions)))

(defun redtime--build-user-completion (membership)
  "Build single completion entry from MEMBERSHIP object."
  (let* ((user (plist-get membership :user))
         (user-id (plist-get user :id))
         (user-name (get-decode :name user)))
    (cons user-name user-id)))

(provide 'redtime-report)
;;; redtime-report.el ends here
