(defun tuxikus/ymir ()
  (interactive)
  (let* ((applications (tuxikus/get-applications))
	 (selected-application (completing-read "App: " applications)))
    (shell-command (format "xdg-open /run/current-system/sw/share/applications/%s" selected-application) nil nil)))

(defun tuxikus/get-applications ()
  (directory-files "/run/current-system/sw/share/applications"))
