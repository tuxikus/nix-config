(defun tuxikus/ymir (app)
  (interactive "sApp: ")
  (shell-command (concat app " > /dev/null 2>&1 & disown") nil nil))
