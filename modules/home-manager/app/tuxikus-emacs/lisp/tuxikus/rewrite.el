(defun tuxikus/get-jira-ticket-number (branch)
  (when (string-match "[A-Z]\\{8\\}-[0-9]*" branch)
    (message (match-string 0 branch))))

(add-hook 'git-commit-setup-hook '(lambda () (insert (tuxikus/get-jira-ticket-number (magit-get-current-branch)))))

(defun tuxikus/get-bookmarks-from-file ()
  "Get bookmarks from the bookmark file"
  (with-temp-buffer
    (insert-file-contents "~/.bookmarks.org")
    (org-mode)
    (let (bookmarks)
      (org-element-map (org-element-parse-buffer) 'link
        (lambda (l)
          (let* ((link (org-element-property :raw-link l))
                 (name (org-element-interpret-data (org-element-contents l)))
                 (tags (org-element-property :tags (org-element-property :parent l))))
            (push (concat name
                          "\n"
                          link
                          "\n"
                          (format "[%s]" (mapconcat #'identity tags ", "))) bookmarks))))
      bookmarks)))

(defun tuxikus/add-bookmark ()
  "Add a new bookmark to the bookmark file."
  (interactive)
  (let* ((title (read-from-minibuffer "Title: "))
         (url (read-from-minibuffer "URL: "))
         (tags (read-from-minibuffer "Tags: ")))
    (write-region (format "* [[%s][%s]] %s\n" url title tags) nil "~/.bookmarks.org" 'append)))

(defun tuxikus/edit-bookmark ()
  "TODO implement."
  (interactive)
  (message "Not implemented."))

(defun tuxikus/delete-bookmark ()
  "TODO implement."
  (interactive)
  (message "Not implemented."))

(defun tuxikus/open-bookmark ()
  "Select a bookmark and open it."
  (interactive)
  (browse-url
   (seq-elt (split-string
             (completing-read "Open: " (tuxikus/get-bookmarks-from-file))
             "\n") 1)))

(defun tuxikus/change-org-directory ()
  "Change the active org directory."
  (interactive)
  (let ((selection (completing-read "Select: " '("~/org" "~/org-edu"))))
    (setq org-directory selection
          org-attach-id-dir (concat org-directory "/.attach")
          org-roam-directory (concat org-directory "/roam")
          org-roam-db-location (concat org-directory "/org-roam.db"))))

(provide 'rewrite)
