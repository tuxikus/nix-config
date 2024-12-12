;; config-keys.el --- -*- lexical-binding: t -*-

;; font zoom hydra
(defhydra hydra-zoom (global-map "C-c z")
  "zoom"
  ("i" text-scale-increase "increase")
  ("d" text-scale-decrease "decrease")
  ("q" nil "quit"))

;; window management hydra
(defhydra hydra-window (global-map "C-c w")
  "window management"
  ("h" split-window-below "split horizontal")
  ("v" split-window-right "split vertical")
  ("d" delete-window "delete")
  ("<left>" windmove-left "move left")
  ("<right>" windmove-right "move right")
  ("<up>" windmove-up "move up")
  ("<down>" windmove-down "move down")
  ("q" nil "quit"))

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
