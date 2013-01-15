;; === bootstrap the starter kit ===
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

(defvar my-packages '(starter-kit starter-kit-lisp starter-kit-bindings
                                  color-theme color-theme-twilight
                                  clojure-mode clojure-test-mode gist
                                  markdown-mode))

(when (null package-archive-contents)
  (package-refresh-contents))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; === PHILLLLLLLL!!!! ===
(setq ido-ubiquitous-enabled nil)
(setq ido-use-virtual-buffers nil)

;; === appearance (font, menu, size) ===
;; (set-face-attribute 'default nil :height 100)
;; (add-to-list 'default-frame-alist '(height . 36))
;; (add-to-list 'default-frame-alist '(width . 120))
(global-hl-line-mode 1)
(setq initial-scratch-message nil)
(setq visible-bell nil)
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

;; === no back up files ===
(setq make-backup-files nil)
(setq auto-save-default nil)

;; === just do it, okay ===
(setq require-final-newline t)

;; === find file in project ===
(setq ffip-patterns ".*")
(setq ffip-regexp ".*")

;; === qrr is better than query-replace-regexp ===
(defalias 'qrr 'query-replace-regexp)

;; === remapping paredit keys, because emacs is a heart-breaker ===
(require 'paredit)
(define-key paredit-mode-map (kbd "C-o C-r") 'paredit-forward-slurp-sexp)
(define-key paredit-mode-map (kbd "C-o M-r") 'paredit-forward-barf-sexp)
(define-key paredit-mode-map (kbd "C-o C-l") 'paredit-backward-slurp-sexp)
(define-key paredit-mode-map (kbd "C-o M-l") 'paredit-backward-barf-sexp)

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

;; === stfu ===
(eval-after-load 'slime '(setq slime-protocol-version 'ignore))

;; === clojure repls should be awesome ===
(setq slime-net-coding-system 'utf-8-unix)

;; === more like esk-ugly-fn ===
(remove-hook 'clojure-mode-hook 'esk-pretty-fn)

;; === fix for silly clojure-jack-in ballsness ===
(remove-hook 'slime-indentation-update-hooks 'put-clojure-indent)

;; === don't echo my commands in shell mode ===
(defun comint-stfu ()
  (setq comint-process-echoes t))
(add-hook 'comint-mode-hook 'comint-stfu)

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
