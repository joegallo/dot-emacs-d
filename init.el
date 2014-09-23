;; === bootstrap packages ===
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
             '("melpa-stable" . "http://melpa-stable.milkbox.net/packages/") t)
(package-initialize)

(defvar my-packages '(better-defaults
                      cider
                      color-theme
                      color-theme-twilight
                      elpy
                      gist
                      idle-highlight-mode
                      ido-ubiquitous
                      magit
                      markdown-mode
                      paredit
                      smex
                      yaml-mode))

(when (null package-archive-contents)
  (package-refresh-contents))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; === un-annoy ===
(global-hl-line-mode 1)
(setq auto-save-default nil
      ido-use-virtual-buffers nil
      inhibit-startup-message t
      initial-scratch-message nil
      make-backup-files nil
      require-final-newline t
      visible-bell nil)

;; === colors ===
(defun plist-to-alist (the-plist)
  (defun get-tuple-from-plist (the-plist)
    (when the-plist
      (cons (car the-plist) (cadr the-plist))))

  (let ((alist '()))
    (while the-plist
      (add-to-list 'alist (get-tuple-from-plist the-plist))
      (setq the-plist (cddr the-plist)))
    alist))

(require 'color-theme)
(add-to-list 'load-path "~/.emacs.d/elpa/color-theme-twilight-0.1")
(autoload 'color-theme-twilight "color-theme-twilight" nil t)
(when (>= (display-color-cells) 256)
  (color-theme-twilight))

(eval-after-load 'magit
  '(progn
     (set-face-foreground 'magit-diff-add "green3")
     (set-face-foreground 'magit-diff-del "red3")
     (when (not window-system)
       (set-face-background 'magit-item-highlight "gray13"))))

;; === useful keys ===
(global-set-key (kbd "C-c g") 'magit-status)

(defun cleanup-buffer ()
  (interactive)
  (untabify (point-min) (point-max))
  (indent-region (point-min) (point-max))
  (delete-trailing-whitespace))

(global-set-key (kbd "C-c n") 'cleanup-buffer)

;; === find file in project ===
(setq ffip-patterns ".*")
(setq ffip-regexp ".*")

;; === qrr is better than query-replace-regexp ===
(defalias 'qrr 'query-replace-regexp)

;; === remapping paredit keys, because emacs is a heart-breaker ===
(require 'paredit)

(eval-after-load 'paredit
  '(progn
     (define-key paredit-mode-map (kbd "M-)") 'paredit-forward-slurp-sexp)
     (define-key paredit-mode-map (kbd "C-o C-r") 'paredit-forward-slurp-sexp)
     (define-key paredit-mode-map (kbd "C-o M-r") 'paredit-forward-barf-sexp)
     (define-key paredit-mode-map (kbd "C-o C-l") 'paredit-backward-slurp-sexp)
     (define-key paredit-mode-map (kbd "C-o M-l") 'paredit-backward-barf-sexp)))

;; === transpose buffers
(defun transpose-buffers (arg)
  "Transpose the buffers shown in two windows."
  (interactive "p")
  (let ((selector (if (>= arg 0) 'next-window 'previous-window)))
    (while (/= arg 0)
      (let ((this-win (window-buffer))
            (next-win (window-buffer (funcall selector))))
        (set-window-buffer (selected-window) next-win)
        (set-window-buffer (funcall selector) this-win)
        (select-window (funcall selector)))
      (setq arg (if (plusp arg) (1- arg) (1+ arg))))))
(global-set-key (kbd "C-x 4 t") 'transpose-buffers)

;; === more like esk-ugly-fn ===
(remove-hook 'clojure-mode-hook 'esk-pretty-fn)

;; === fix for silly clojure-jack-in ballsness ===
(remove-hook 'slime-indentation-update-hooks 'put-clojure-indent)

;; === don't echo my commands in shell mode ===
(defun comint-stfu ()
  (setq comint-process-echoes t))
(add-hook 'comint-mode-hook 'comint-stfu)

;; === shell dir-tracking ===
(defun track-shell-directory/procfs ()
  (shell-dirtrack-mode 0)
  (add-hook 'comint-preoutput-filter-functions
            (lambda (str)
              (prog1 str
                (when (string-match comint-prompt-regexp str)
                  (cd (file-symlink-p
                       (format "/proc/%s/cwd" (process-id
                                               (get-buffer-process
                                                (current-buffer)))))))))
            nil t))

(when (file-exists-p "/proc")
  (add-hook 'shell-mode-hook 'track-shell-directory/procfs))

;; === ssh agent fix ===
(defun agent-path ()
  (if (eq system-type 'darwin)
      "*launch*/Listeners"
    "*ssh*/agent\.*"))

(defun find-agent ()
  (let* ((path-clause (format "-path \"%s\"" (agent-path)))
         (find-command (format "$(find -L /tmp -uid $UID %s -type s 2> /dev/null)"
                               path-clause)))
    (first (split-string
            (shell-command-to-string
             (format "/bin/ls -t1 %s | head -1" find-command))))))

(defun fix-agent ()
  (interactive)
  (let ((agent (find-agent)))
    (setenv "SSH_AUTH_SOCK" agent)
    (message agent)))

;; === awesomely reenable paredit when conflicts are gone
(defvar reenable-paredit-modes
  '(emacs-lisp-mode clojure-mode lisp-mode)
  "Modes to automatically re-enable paredit for after fixing version-control conflict markers")

(put 'unresolved-conflict 'error-conditions '(unresolved-conflict error))
(put 'unresolved-conflict 'error-message "Unresolved conflict markers in file")

(defun mark-git-conflict-resolved ()
  (interactive)
  (when (apply #'derived-mode-p reenable-paredit-modes)
    (paredit-mode t)) ;; will fail and abort if parens are unbalanced
  (if (save-excursion
        (beginning-of-buffer)
        (re-search-forward "[<=>]\\{7\\}" nil t))
      (let ((error-location (match-beginning 0)))
        (goto-char error-location)
        (signal 'unresolved-conflict (list error-location)))
    (save-buffer)
    (shell-command (concat "git add " (buffer-file-name)))))

(global-set-key (kbd "C-c a") 'mark-git-conflict-resolved)

;; === nrepl and stuff, not quite finished here
(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
(add-hook 'cider-mode-hook 'subword-mode)
(add-hook 'cider-repl-mode-hook 'subword-mode)
(add-hook 'cider-repl-mode-hook 'paredit-mode)

;; (require 'auto-complete-config)
;; (ac-config-default)

;; (require 'ac-nrepl)
;; (add-hook 'nrepl-mode-hook 'ac-nrepl-setup)
;; (add-hook 'nrepl-interaction-mode-hook 'ac-nrepl-setup)

;; (define-key nrepl-interaction-mode-map (kbd "C-c M-p") 'nrepl-set-ns)

;; (define-key nrepl-mode-map (kbd "C-c C-d") 'ac-nrepl-popup-doc)
;; (define-key nrepl-interaction-mode-map (kbd "C-c C-d") 'ac-nrepl-popup-doc)

;; (eval-after-load "auto-complete"
;;   '(add-to-list 'ac-modes 'nrepl-mode))

;; (defun set-auto-complete-as-completion-at-point-function ()
;;   (setq completion-at-point-functions '(auto-complete)))
;; (add-hook 'auto-complete-mode-hook 'set-auto-complete-as-completion-at-point-function)

;; (add-hook 'nrepl-mode-hook 'set-auto-complete-as-completion-at-point-function)
;; (add-hook 'nrepl-interaction-mode-hook 'set-auto-complete-as-completion-at-point-function)

;; === uhhhh python stuff i guess ===
(elpy-enable)
(elpy-use-ipython)

;; === work stuff ===
(let ((sonian-nav-file "~/Code/sa-safe/.elisp/sonian-navigation.el"))
  (when (file-exists-p sonian-nav-file)
    (load (expand-file-name sonian-nav-file))))

;; === ledger stuff ===
(let ((ledger-file "~/Code/ledger/lisp/ledger-mode.el"))
  (when (file-exists-p ledger-file)
    (setq ledger-post-account-alignment-column 2)
    (custom-set-faces '(ledger-font-xact-highlight-face ((t (:background "color-234")))))
    (add-to-list 'load-path "~/Code/ledger/lisp")
    (require 'ledger-mode)))
