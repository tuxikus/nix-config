;;; init-dashboard.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package dashboard
  :config
  (setq dashboard-projects-backend 'project-el)

  (setq dashboard-items '((recents   . 10)
                        (bookmarks . 10)
                        (projects  . 10)
                        (agenda    . 10)
                        (registers . 10)))

  (setq dashboard-item-shortcuts '((recents   . "r")
                                 (bookmarks . "m")
                                 (projects  . "p")
                                 (agenda    . "a")
                                 (registers . "e")))

  (setq initial-buffer-choice (lambda () (get-buffer-create dashboard-buffer-name)))

  (dashboard-setup-startup-hook))

(provide 'init-dashboard)

;;; init-dashboard.el ends here
