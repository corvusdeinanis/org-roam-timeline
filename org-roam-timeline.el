;;; org-roam-timeline.el --- Timeline with Content Preview & Navigation -*- lexical-binding: t; -*-

(require 'org-roam)
(require 'simple-httpd)
(require 'json)
(require 'ox-html)

(defvar org-roam-timeline-root (file-name-directory (or load-file-name buffer-file-name)))

;; --- 1. Interactive Helper (Add Dates) ---
(defun org-roam-timeline-add-date ()
  "Interactively add timeline properties."
  (interactive)
  (let* ((start-input (read-string "Start Date (YYYY-MM-DD, YYYY, or -YYYY): "))
         (is-range (y-or-n-p "Is this a time range (period)? "))
         (end-input (if is-range (read-string "End Date: ") nil)))
    (unless (string-empty-p start-input)
      (org-set-property "TIMELINE_START" start-input))
    (if (and end-input (not (string-empty-p end-input)))
        (org-set-property "TIMELINE_END" end-input)
      (org-delete-property "TIMELINE_END"))))

;; --- 2. Helper: Case-Insensitive Property Lookup ---
(defun org-roam-timeline--get-prop (key props)
  (let ((match (seq-find (lambda (pair) (string-equal-ignore-case (car pair) key)) props)))
    (cdr match)))

;; --- 3. Data Extraction (Fail-Safe) ---
(defun org-roam-timeline--get-nodes ()
  (let ((nodes '()))
    (message "TIMELINE: Starting export...")
    (dolist (node (org-roam-node-list))
      ;; SAFETY: Catch errors per node so one bad apple doesn't kill the timeline
      (condition-case err
          (let* ((props (org-roam-node-properties node))
                 (id (org-roam-node-id node))
                 (start-raw (org-roam-timeline--get-prop "TIMELINE_START" props))
                 (end-raw   (org-roam-timeline--get-prop "TIMELINE_END" props))
                 (start-clean (org-roam-timeline--clean-date start-raw nil))
                 (end-clean   (org-roam-timeline--clean-date end-raw t)))

            (when start-clean
              (let* ((tags (or (org-roam-node-tags node) '("Uncategorized")))
                     (backlinks (org-roam-backlinks-get node))
                     (incoming-ids (mapcar (lambda (bl) (org-roam-node-id (org-roam-backlink-source-node bl))) backlinks))
                     (outgoing-ids (ignore-errors
                                     (mapcar #'car (org-roam-db-query [:select dest :from links :where (= source $s1)] id))))
                     (all-neighbors (append incoming-ids outgoing-ids))
                     (backlink-count (length backlinks))
                     (importance-class (cond ((> backlink-count 10) "item-huge") ((> backlink-count 5) "item-large") (t "item-small"))))
                
                (push `((id . ,id)
                        (content . ,(org-roam-node-title node))
                        (start . ,start-clean)
                        ,@(when end-clean `((end . ,end-clean)))
                        (type . ,(if end-clean "range" "point"))
                        (className . ,importance-class)
                        (all_tags . ,tags)
                        (neighbors . ,all-neighbors)) 
                      nodes))))
        (error (message "TIMELINE SKIP: '%s' failed: %s" (org-roam-node-title node) err))))
    
    (message "TIMELINE: Exported %d nodes." (length nodes))
    nodes))

;; --- 4. Date Cleaner ---
(defun org-roam-timeline--clean-date (date-str &optional is-end)
  (when (stringp date-str)
    (let ((clean (string-trim date-str)))
      (cond
       ((string-match "^-?[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}$" clean) clean)
       ((string-match "^\\(-?[0-9]\\{4\\}\\)$" clean)
        (if is-end (concat (match-string 1 clean) "-12-31") (concat (match-string 1 clean) "-01-01")))
       ((string-match "[\\[<]\\(-?[0-9]+\\S-*\\)[\\]>]" clean) (match-string 1 clean))
       (t nil)))))

;; --- 5. Servlets ---
(defservlet* data text/json ()
  (let ((data (org-roam-timeline--get-nodes)))
    (insert (json-encode data))))

(defservlet* open text/plain (id)
  (let ((node (org-roam-node-from-id id)))
    (if node
        (progn
          (with-current-buffer (window-buffer (selected-window))
            (org-roam-node-open node))
          (insert "Opened"))
      (insert "Node not found"))))

;; --- C. CONTENT PREVIEW (Robust String Export) ---
;; C. CONTENT PREVIEW (With Backlinks & Smart Links)
(defservlet* content text/html (id)
  (require 'ox-html)
  (let ((marker (org-id-find id 'marker))
        (final-output ""))
    
    (if (not marker)
        (setq final-output (format "<h3>Error: ID %s not found.</h3>" id))
      
      ;; 1. GET RAW CONTENT
      (let* ((raw-content 
              (with-current-buffer (marker-buffer marker)
                (save-excursion
                  (goto-char (marker-position marker))
                  (let ((beg (point))
                        (end (if (org-at-heading-p)
                                 (save-excursion (org-end-of-subtree) (point))
                               (point-max))))
                    (buffer-substring-no-properties beg end)))))
             (node (org-roam-node-from-id id))
             (backlinks (org-roam-backlinks-get node)))
        
        ;; 2. CONVERT CONTENT TO HTML
        (condition-case err
            (let ((org-export-use-babel nil)
                  (org-confirm-babel-evaluate nil)
                  (org-html-with-latex 'mathjax)
                  (org-export-with-section-numbers nil)
                  ;; CRITICAL: Force ID links to stay as "id:..." so JS can catch them
                  (org-html-link-org-files-as-html nil)) 
              
              (let ((html (org-export-string-as raw-content 'html t '(:with-toc nil :with-section-numbers nil))))
                
                ;; 3. GENERATE BACKLINKS SECTION
                (let ((bl-html "<div class='backlinks-section' style='margin-top:40px; border-top:1px solid #eee; padding-top:20px;'>
                                <h4 style='color:#888; text-transform:uppercase; font-size:12px; letter-spacing:1px;'>Linked References</h4>
                                <ul style='list-style:none; padding:0;'>"))
                  
                  (dolist (bl backlinks)
                    (let* ((source-node (org-roam-backlink-source-node bl))
                           (src-id (org-roam-node-id source-node))
                           (src-title (org-roam-node-title source-node)))
                      (setq bl-html (concat bl-html 
                                            (format "<li style='margin-bottom:8px;'><a href='id:%s' style='text-decoration:none; color:#3182ce; font-weight:500;'>%s</a></li>" 
                                                    src-id src-title)))))
                  (setq bl-html (concat bl-html "</ul></div>"))
                  
                  ;; 4. COMBINE
                  (setq final-output (concat "<div class='org-content'>" html bl-html "</div>")))))
          
          ;; FALLBACK
          (error 
           (setq final-output (format "<h3 style='color:red'>Export Failed</h3><pre>%s</pre>" err))))))
    
    (insert final-output)))

;; --- 6. Launcher ---
(defun org-roam-timeline-open ()
  (interactive)
  (setq httpd-root (expand-file-name "html" org-roam-timeline-root))
  (httpd-start)
  (browse-url (format "http://localhost:%d" httpd-port)))

(provide 'org-roam-timeline)
