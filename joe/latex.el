(require 'tex-mode)
(define-key latex-mode-map (kbd "C-c C-k") 'compile)
(setq compilation-read-command nil) ; TODO: applies outside latex-mode too...
;; Close the compilation window if there was no error at all.
(setq compilation-exit-message-function
      (lambda (status code msg)
        ;; If M-x compile exists with a 0
        (when (and (eq status 'exit) (zerop code))
          ;; then bury the *compilation* buffer, so that C-x
          ;; b doesn't go there
          (bury-buffer "*compilation*")
          ;; and return to whatever were looking at before
          ;;(replace-buffer-in-windows "*compilation*")
          ;; delete it instead
          (delete-window (get-buffer-window (get-buffer "*compilation*"))))
        ;; Always return the anticipated result of
        ;; compilation-exit-message-function
        (cons msg code)))
