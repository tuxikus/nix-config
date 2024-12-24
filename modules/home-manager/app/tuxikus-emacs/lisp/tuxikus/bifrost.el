(defun tuxikus/bifrost ()
  (interactive)
  (let* ((applications (tuxikus/bifrost--get-dapps))
         (os-options (tuxikus/bifrost--get-os-control-options))
         (all-options (append
                       (mapcar (lambda (app) (concat "[DAPP] " app)) applications)
                       (mapcar (lambda (opt) (concat "[OS] " opt)) os-options))))

    (let* ((selected-option (completing-read "Select an option: " all-options nil t)))
      (cond
       ((string-prefix-p "[DAPP] " selected-option)
        (let* ((app-name (substring selected-option 7)))
	  (tuxikus/bifrost--launch-dapp app-name)))
       ((string-prefix-p "[OS] " selected-option)
        (let* ((os-action (substring selected-option 5)))
          (tuxikus/bifrost--os-control os-action)))))))

;; (defun tuxikus/bifrost-frame ()
;;   (interactive)
;;   (with-current-buffer (get-buffer-create "*bifrost*")
;;     (let ((frame (make-frame '((name . "bifrost")
;; 			       (auto-raise . t)
;; 			       (height . 15)
;; 			       (internal-border-width . 25)
;; 			       (left . 0.33)
;; 			       (left-fringe . 0)
;; 			       (line-spacing . 3)
;; 			       (menu-bar-lines . 0)
;; 			       (right-fringe . 0)
;; 			       (tool-bar-lines . 0)
;; 			       (top . 48)
;; 			       (minibuffer . only)
;; 			       (undecorated . nil)
;; 			       (unsplittable . t)
;; 			       (vertical-scroll-bars . nil)
;; 			       (width . 110)))))

;;       (let* ((applications (tuxikus/bifrost--get-dapps))
;;              (os-options (tuxikus/bifrost--get-os-control-options))
;;              (all-options (append
;;                            (mapcar (lambda (app) (concat "[DAPP] " app)) applications)
;;                            (mapcar (lambda (opt) (concat "[OS] " opt)) os-options))))

;;         (let* ((selected-option (completing-read "Select an option: " all-options nil t)))
;;           (cond
;;            ((string-prefix-p "[DAPP] " selected-option)
;;             (let* ((app-name (substring selected-option 7)))
;; 	      (tuxikus/bifrost--launch-dapp app-name)))
;;            ((string-prefix-p "[OS] " selected-option)
;;             (let* ((os-action (substring selected-option 5)))
;;               (tuxikus/bifrost--os-control os-action))))))
      
;;       (delete-frame frame)
;;       (kill-buffer "*bifrost*"))))


;; launch desktop application
(setq tuxikus/bifrost-dapps-directory "/run/current-system/sw/share/applications")

(defun tuxikus/bifrost--get-dapps ()
  (directory-files tuxikus/bifrost-dapps-directory nil "\\.desktop$"))

(defun tuxikus/bifrost-launch-dapp ()
  (interactive)
  (shell-command (format "dex /run/current-system/sw/share/applications/%s > /dev/null 2>&1 & disown" (completing-read "-> " (tuxikus/bifrost--get-dapps)) nil nil)))

(defun tuxikus/bifrost--launch-dapp (dapp)
  (shell-command (format "dex /run/current-system/sw/share/applications/%s > /dev/null 2>&1 & disown" dapp) nil nil))

;; os control
(setq tuxikus/bifrost--os-control-options '("poweroff" "reboot"))

(defun tuxikus/bifrost--get-os-control-options ()
  tuxikus/bifrost--os-control-options)

(defun tuxikus/bifrost-os-control ()
  (interactive)
  (shell-command (format "systemctl %s & disown" (completing-read "-> " (tuxikus/bifrost--get-os-control-options)) nil nil)))

(defun tuxikus/bifrost--os-control (opt)
  (interactive)
  (shell-command (format "systemctl %s & disown" opt) nil nil))
