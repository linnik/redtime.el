;;; redtime-report.el --- report functionality

;;; Commentary:
;;;
;;

;;; Code:
(require 'cl-lib)
(require 'redtime-config)

(defvar redtime-report-columns
  '(:date :activity :issue :hours :comment)
  "Columns for report table.
You can customize report table content
by reordering or removing keywords in this list.")

(defvar redtime-report-sorting-field :date
  "Default sorting field in reports table.")

(defvar redtime-report-sorting-order :descending
  "Default sorting order in reports table.  Either :descending or :ascending.")

(defvar redtime--report-column-labels
  (list :date "Date"
        :activity "Activity"
        :issue "Issue"
        :hours "Hours"
        :comment "Comments")
  "Labels which will be used in table header.  Order is ignored here.")

(defvar redtime--report-cache nil
  "Contents of report table.")

(defvar redtime--report-sort
  `(,redtime-report-sorting-field . ,redtime-report-sorting-order)
  "Currenty selected sorting method.")

(defvar *redtime-buffer-name* "*Redtime*"
  "Name of the redtime buffer.")

(defvar redtime-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "s a") #'redtime-report-sort-by-activity)
    (define-key map (kbd "s d") #'redtime-report-sort-by-date)
    (define-key map (kbd "s i") #'redtime-report-sort-by-issue)
    (define-key map (kbd "s h") #'redtime-report-sort-by-hours)
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
  (let ((sort-key (car redtime--report-sort))
        (sort-order (cdr redtime--report-sort)))
    (setq redtime--report-cache
          (cl-sort redtime--report-cache 'string-lessp
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

;; (defun redtime-report-select-date ()
;;   "X.")

;; (defun redtime-report-set-user ()
;;   "X.")

;; (defun redtime-report-ask-user ()
;;   "X.")

(defun redtime-list-entries ()
  "List time entries."
  (interactive)
  (let* ((buffer-read-only t)
         (buf (get-buffer-create *redtime-buffer-name*)))
    (pop-to-buffer buf)
    (redtime-mode)
    (redtime--report-build-cache)
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

(defun redtime--report-row (object)
  "Construct single report table row from OBJECT."
  (let ((row-cells (mapcar
                   (apply-partially 'plist-get object)
                   redtime-report-columns)))
    (concat "|" (mapconcat 'identity row-cells "|") "|\n")))

(defun redtime--report-process (object)
  "Extract required data from Redmine response OBJECT."
  (let* ((activity-name (plist-get (plist-get object :activity) :name))
         (issue-id (number-to-string (plist-get (plist-get object :issue) :id)))
         (user (plist-get object :user))
         (user-name (plist-get user :name))
         (hours (number-to-string (plist-get object :hours)))
         (spent_on (plist-get object :spent_on))
         (comments (get-decode :comments object)))
    (list :date spent_on
          :activity activity-name
          :issue issue-id
          :hours hours
          :comment comments)))

(defun redtime--report-build-cache ()
  "Fetch time entries from Redmine."
  (let* ((redmine-host redtime/host)
         (redmine-api-key redtime/api-key)
         (entries (elmine/get-time-entries :offset 0 :limit 10)))
    (redtime--reset-sorting)
    (setq redtime--report-cache (mapcar 'redtime--report-process entries))))

(provide 'redtime-report)
;;; redtime-report.el ends here