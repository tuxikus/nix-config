;; config-ui.el	--- -*- lexical-binding: t -*-

;; disable ui elements
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

;; clock in status bar
(setq display-time-day-and-date t
      display-time-24hr-format t)
(display-time)

;; disable startup screen
(setq inhibit-startup-message t)
(setq inhibit-startup-screen t)
(setq initial-scratch-message ";; Emacs is fun")

;; theme
(load-theme 'modus-vivendi t)

;; font
(set-face-attribute 'default nil
                    :family "Iosevka Nerd Font"
                    :height 150
                    :weight 'normal
                    :width 'normal)

(provide 'config-ui)

;; config-ui.el ends here
