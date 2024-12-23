(defun tuxikus/ymir ()
  (interactive)
  (let* ((applications (tuxikus/get-applications))
	 (selected-application (completing-read "App: " applications)))
    (shell-command (concat selected-application " > /dev/null 2>&1 & disown") nil nil)))

(defun tuxikus/get-applications ()
  (setq applications
	(mapcar (lambda (item)
		  (replace-regexp-in-string ".desktop" "" item))
		(directory-files "/run/current-system/sw/share/applications"))))
