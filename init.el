;; -- mode: elisp --

;; User info
(setq user-full-name "Michael Winsor")



;; Enable use-package
;;(eval-when-compile
;;  (require 'use-package))
;;(require 'diminish)
;;(require 'bind-key)


;; == Straight Package Manager ==
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))



;; == UI Tweaks ==
;; Disable splash screen
(setq inhibit-splash-screen t)

;; Enable transient mark mode
(transient-mark-mode 1)

;; Set font size
(set-face-attribute 'default nil :height 110)

;; Set theme
(load-theme 'misterioso)


;; == Diminish Mode ==
(use-package diminish :straight t)


;; == Evil Mode ==
(use-package evil
  :straight t
  :diminish undo-tree-mode
  :init (setq evil-want-C-i-jump nil)
  :config (evil-mode 1)

  ;; Resolve sentence jumping issue
  (setq sentence-end-double-space nil))


;; == Helm ==
(use-package helm
  :straight t)

(setq helm-split-window-in-side-p t
      helm-move-to-line-cycle-in-source t)

(helm-mode 1)

;; List buffers (emacs way)
(global-set-key (kbd "C-x b") 'helm-buffers-list)
;; List buffers (vim way)
(define-key evil-ex-map "b" 'helm-buffers-list)

;; Helm bookmarks
(global-set-key (kbd "C-x r b") 'helm-bookmarks)

;; Helm find files
(global-set-key (kbd "C-x C-f") 'helm-find-files)

;; Helm M-x menu
(global-set-key (kbd "M-x") 'helm-M-x)

;; Helm search
(global-set-key (kbd "C-s") 'helm-occur)

;; Helm calculator
(global-set-key (kbd "M-c") 'helm-calcul-expression)

;; Helm kill ring (pick something to paste)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)


;; == Org Mode ==
;; Enable org mode
(require 'org)

;; Make org mode work with files ending in .org
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

;; Org TODO keywords
(setq org-todo-keywords
      '((sequence "TODO(t)" "|" "DONE(d)")
	(sequence "NEXT(n)" "WAITING(w)" "SOMEDAY(s)" "|" "CANCELED(c)")))

;; Org auto indent
(setq org-startup-indented t)

;; Org refile targets
(setq org-refile-targets '((org-agenda-files :maxlevel . 3)))
(setq org-refile-use-outline-path 'file)
(setq org-refile-allow-creating-parent-nodes 'confirm)

;; Org auto-log completion time
(setq org-log-done 'time)

;; Org Agenda Files
(setq org-agenda-files '("~/org"))


;; ==Custom set variables==
(custom-set-variables
 '(org-capture-templates
   '(("b" "Book" entry
      (file "read.org")
      (file "/Users/mw/org/templates/book.org"))))
 '(org-enforce-todo-dependencies t))
 ;;'(package-selected-packages '(evil diminish use-package)))


;; ==Key mappings==
(define-key global-map "\C-ca" 'org-agenda)
(global-set-key (kbd "<f6>") 'org-capture)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
