;; init-ui.el	--- -*- lexical-binding: t -*-

;; disable ui elements
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

;; theme
(load-theme 'almost-mono-white t)

;; clock in status bar
(setq display-time-day-and-date t
      display-time-24hr-format t)
(display-time)

;; disable startup screen
(setq inhibit-startup-message t)
(setq inhibit-startup-screen t)
(setq initial-scratch-message ";; Emacs is fun")


;; font
(set-face-attribute 'default nil
                    :family "Iosevka Nerd Font"
                    :height 150
                    :weight 'light
                    :width 'normal)

(set-face-attribute 'bold nil
                    :family "Iosevka Nerd Font"
                    :weight 'light)

(set-face-attribute 'italic nil
                    :family "Iosevka Nerd Font"
                    :slant 'italic
                    :weight 'light)

(set-face-attribute 'bold-italic nil
                    :family "Iosevka Nerd Font"
                    :weight 'light
                    :slant 'italic)

;; window divider
(setq window-divider-default-right-width 5
      window-divider-default-bottom-width 5
      window-divider-default-places t)

(window-divider-mode 1)

(set-fringe-mode 10)

(provide 'init-ui)

;; init-ui.el ends here
