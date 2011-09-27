(let ((sonian-nav-file "~/Code/sa-safe/.elisp/sonian-navigation.el"))
  (when (file-exists-p sonian-nav-file)
    (load (expand-file-name sonian-nav-file))))

(defun switch-to-slime (prefix)
  "Switch to *slime-repl clojure* if it exists, otherwise run
 (swank-clojure-project).  Use this instead
 of (swank-clojure-project) because that doesn't play nice with
 switch-to-previous-buffer."
  (interactive "p")
  (let ((b (get-buffer "*slime-repl clojure*")))
    (if (and b (slime-connected-p) (= prefix 1))
        (switch-to-buffer b)
      (call-interactively 'lein-swank))))
(global-set-key [f5] 'switch-to-slime)

(defun switch-to-shell ()
  "If *shell* exists, switch to it, otherwise run (shell).
Use this instead of (shell) because that doesn't play nice with
switch-to-previous-buffer."
  (interactive)
  (let ((b (get-buffer "*shell*")))
    (if b
        (switch-to-buffer b)
      (shell))))
(global-set-key [f6] 'switch-to-shell)

(setenv "LD_LIBRARY_PATH" "/opt/ibm/lotus/notes/")
