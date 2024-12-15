;; config-keys.el --- -*- lexical-binding: t -*-

;; basic file operations
(keymap-global-set "C-c f f" 'find-file)
(keymap-global-set "C-c f r" 'recentf)
(keymap-global-set "C-c f s" 'save-buffer)

;; buffer
(keymap-global-set "C-c b i" 'ibuffer)
(keymap-global-set "C-c b k" 'kill-buffer)
(keymap-global-set "C-c b d" 'kill-buffer)

;; magit
(keymap-global-set "C-c g g" 'magit-status)

;; ace-window
(global-set-key (kbd "M-o") 'ace-window)
(setq aw-keys '(?a ?o ?e ?u ?h ?t ?n ?s ?f))

;; better undo / redo
(global-set-key (kbd "C-z") 'undo)
(global-set-key (kbd "C-S-z") 'undo-redo)

(provide 'config-keys)

;; config-keys.el ends here
