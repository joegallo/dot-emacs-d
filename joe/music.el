;; === emms ===
(when (fboundp 'emms-standard)
  (setq emms-player-mpg321-command-name "mpg123")
  (emms-standard)
  (emms-default-players)
  (setq emms-playlist-buffer-name "*Music*")
  (setq emms-source-file-default-directory "~/Music/")
  (define-key emms-playlist-mode-map "\M-a" 'emms-add-file)
  (define-key emms-playlist-mode-map "\M-f" 'emms-add-find))
