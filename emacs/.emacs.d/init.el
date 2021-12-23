(setq inhibit-startup-message t)

(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 10)        ; Give some breathing room

(menu-bar-mode -1)            ; Disable the menu bar

(defconst user-init-dir
  (cond ((boundp 'user-emacs-directory)
         user-emacs-directory)
        ((boundp 'user-init-directory)
         user-init-directory)
        (t "~/.emacs.d/")))


(defun load-user-file (file)
  (interactive "f")
  "Load a file in current user's configuration directory"
  (load-file (expand-file-name file user-init-dir)))

;; Initialize package sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
 (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
   (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(setq quelpa-update-melpa-p nil)

(use-package ivy
  :bind(("C-s" . swiper)
	 :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)	
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))

;; (load-theme 'gruvbox-light-hard t)

;; (use-package doom-themes
;;   :init (load-theme 'gruvbox-light-hard t)
;;   :config
;;   (setq doom-themes-enable-bold t))

(use-package theme-changer)
(setq calendar-location-name "Split, Croatia")
(setq calendar-latitude 43.508133)
(setq calendar-longitude 16.440193)

(change-theme 'gruvbox-light-hard 'gruvbox-dark-hard)

(column-number-mode 0)
(global-display-line-numbers-mode 'relative)
(setq display-line-numbers-type 'relative)
(add-hook 'prog-mode-hook (lambda () (setq display-line-numbers 'relative)))

(set-face-attribute 'default nil :font "Iosevka Custom Extended" :height 130)
(set-face-attribute 'font-lock-comment-face nil  :font "Iosevka Custom Extended Italic" :height 130)
;; (set-face-attribute 'default nil :font "Cascadia Code PL" :height 130)
;; (set-face-attribute 'font-lock-comment-face nil  :font "Cascadia Code PL Italic" :height 130)

(add-hook 'server-after-make-frame-hook (lambda () (set-face-attribute 'default nil :font "Iosevka Custom Extended" :height 130)))
(add-hook 'server-after-make-frame-hook (lambda () (set-face-attribute 'font-lock-comment-face nil  :font "Iosevka Custom Extended Italic" :height 130)))
;; (add-hook 'server-after-make-frame-hook (lambda () (set-face-attribute 'default nil :font "Cascadia Code PL" :height 130)))
;; (add-hook 'server-after-make-frame-hook (lambda () (set-face-attribute 'font-lock-comment-face nil  :font "Cascadia Code PL Italic" :height 130)))

;;(defvar default-font "Iosevka Custom Extended")
;;(defvar default-font-italic "Iosevka Custom Extended Italic")
;;(add-to-list 'default-frame-alist `(font . ,default-font))
;;(add-to-list 'default-frame-alist `(font-lock-comment-face . ,default-font-italic))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config (setq which-key-idle-delay 1))

(use-package counsel
  :bind (("M-x" . counsel-M-x)
	 ("C-x b" . counsel-ibuffer)
	 ("C-x C-f" . counsel-find-file)
	 :map minibuffer-local-map
	 ("C-r" . counsel-minibuffer-history)))
  
(use-package ivy-rich
  :init (ivy-rich-mode 1))

(global-set-key (kbd "C-M-j") 'ivy-switch-buffer)

(setq evil-want-keybinding nil)

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-C-u-scroll t)
  (setq evil-respect-visual-line-mode t)
  (setq evil-undo-system 'undo-tree)
  (setq evil-undo-tree-autosave-history t)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-normal-state-map (kbd "K") 'man)
  ;(define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)
  
  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

;; (use-package general
;;   :config
;;   (general-evil-setup t)

;;   (general-create-definer jakov/leader-key-def
;; 			  :keymaps '(normal insert visual emacs)
;; 			  :prefix "SPC"
;; 			  :global-prefix "C-SPC"))


(use-package helpful
  :ensure t
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

(use-package all-the-icons)


(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (when (file-directory-p "~/.local/src")
    (setq projectile-project-search-path '("~/.local/src")))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package counsel-projectile
  :config (counsel-projectile-mode))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package magit
  :ensure t
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package org)



(use-package quelpa)
(quelpa '(ligature :repo "mickeynp/ligature.el" :fetcher github))

(use-package ligature
  :config
  (ligature-set-ligatures 't '("->" "<-" "-->" "<--" "!=" "<=" ">=" "=>" "==>" "|=" "=="))
  (global-ligature-mode t))

(use-package undo-tree)
(global-undo-tree-mode)

(setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo")))

(setq undo-tree-auto-save-history t)

(use-package tree-sitter-langs)
(use-package tree-sitter)
(global-tree-sitter-mode)
;(add-hook 'prog-mode-hook #'tree-sitter-mode)
(add-hook 'c-mode-hook '(tree-sitter-hl-mode))
(add-hook 'c++-mode-hook '(tree-sitter-hl-mode))
;; (use-package tree-sitter-indent
;;   :config
;;   (setq c-indent-offset 4)
;;   (setq tree-sitter-indent-c-scopes 4))

;(setq evil--jumps-buffer-targets ".*")

(setq global-magit-auto-revert-mode t)

;MAIL
;;(require 'mu4e)
(setq auth-sources '(password-store))
(auth-source-pass-enable)

(use-package smtpmail)

(use-package mu4e
  :ensure nil
  :config
  (setq mu4e-change-filenames-when-moving t)
  (setq mu4e-get-mail-command "mbsync -a"))

(load-user-file "mail.el")

;; I have my "default" parameters from Gmail
(setq mu4e-sent-folder "/sent"
      ;; mu4e-sent-messages-behavior 'delete ;; Unsure how this should be configured
      mu4e-drafts-folder "/Drafts"
      user-mail-address "jspahi00@fesb.hr"
      smtpmail-default-smtp-server "smtp-mail.outlook.com"
      smtpmail-smtp-server "smtp-mail.outlook.com"
      smtpmail-smtp-service 587)

;;Now I set a list of 
(defvar my-mu4e-account-alist
  '(("Outlook"
     (mu4e-sent-folder "/Sent")
     (user-mail-address "jspahi00@fesb.hr")
     (smtpmail-smtp-user "jspahi00")
     (smtpmail-local-domain "outlook.com")
     (smtpmail-default-smtp-server "smtp-mail.outlook.com")
     (smtpmail-smtp-server "smtp-mail.outlook.com")
     (smtpmail-smtp-service 587)
     )
     ;; Include any other accounts here ...
    ))


(defun my-mu4e-set-account ()
  "Set the account for composing a message.
   This function is taken from: 
     https://www.djcbsoftware.nl/code/mu/mu4e/Multiple-accounts.html"
  (let* ((account
    (if mu4e-compose-parent-message
        (let ((maildir (mu4e-message-field mu4e-compose-parent-message :maildir)))
    (string-match "/\\(.*?\\)/" maildir)
    (match-string 1 maildir))
      (completing-read (format "Compose with account: (%s) "
             (mapconcat #'(lambda (var) (car var))
            my-mu4e-account-alist "/"))
           (mapcar #'(lambda (var) (car var)) my-mu4e-account-alist)
           nil t nil nil (caar my-mu4e-account-alist))))
   (account-vars (cdr (assoc account my-mu4e-account-alist))))
    (if account-vars
  (mapc #'(lambda (var)
      (set (car var) (cadr var)))
        account-vars)
      (error "No email account found"))))
(add-hook 'mu4e-compose-pre-hook 'my-mu4e-set-account)



;MATLAB
(add-to-list 'load-path "/usr/share/emacs/site-lisp/matlab/matlab.el")
(add-to-list 'auto-mode-alist '("\\.m$" . matlab-mode))
(setq matlab-indent-function t)
(setq matlab-shell-command "matlab -nodisplay")

(use-package yasnippet)

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :init
  (setq lsp-keymap-prefix "C-l")
  :config
  (lsp-enable-which-key-integration t))

(use-package company
  :after lsp-mode
  :hook (lsp-mode . company-mode)
  :bind (:map company-active-map
	      ("<tab>" . company-complete-selection))
  (:map lsp-mode-map
	("<tab>" . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 2)
  (company-idle-delay 0.0))


(use-package lsp-treemacs
  :after lsp)

(use-package company-box
  :hook (company-mode . company-box-mode))

(use-package evil-commentary)
(evil-commentary-mode)

(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1))

(add-hook 'c-mode-hook 'lsp)
(add-hook 'c++-mode-hook 'lsp)

(setq garbage-collection-messages t)
(setq gc-cons-threshold (* 100 1024 1024)
      read-process-output-max (* 1024 1024)
      treemacs-space-between-root-nodes nil
      company-idle-delay 0.0
      company-minimum-prefix-length 2
      lsp-idle-delay 0.1)  ;; clangd is fast

(with-eval-after-load 'lsp-mode
  (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration)
  (require 'dap-cpptools)
  (yas-global-mode))

(use-package dap-mode)
(require 'dap-utils)
(dap-mode 1)
(dap-ui-mode 1)
(require 'dap-gdb-lldb)

(use-package exec-path-from-shell)

(setenv "TEXINPUTS" "~/Documents/Latex/LatexPresets/programming/" t)

(add-hook 'yas-minor-mode-hook(lambda () (yas-activate-extra-mode 'fundamental-mode)))

(electric-pair-mode)
(setq-default tab-width 4)

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory "~/Notes")
  :bind
  (("C-c n l" . org-roam-buffer-toggle)
  ("C-c n f" . org-roam-node-find)
  ("C-c n i" . org-roam-node-insert)
  :map org-mode-map
  ("C-M-i" . completion-at-point))
  :config
  (org-roam-setup))

(use-package elfeed)

;; (use-package elfeed-org
;;   :ensure t
;;   :config
;;   (setq elfeed-show-entry-switch 'display-buffer)
;;   (setq rmh-elfeed-org-files (list "~/.emacs.d/elfeed.org")))

(setq elfeed-feeds
	  '(("https://lukesmith.xyz/rss.xml")
		("https://national-justice.com/feeds/feedly")
		("http://thirdrail88.libsyn.com/rss")
		))


;; transparency
;; (set-frame-parameter (selected-frame) 'alpha '(<active> . <inactive>))
;; (set-frame-parameter (selected-frame) 'alpha <both>)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(c-basic-offset 4)
 '(custom-safe-themes
   '("78c4238956c3000f977300c8a079a3a8a8d4d9fee2e68bad91123b58a4aa8588" default))
 '(dap-debug-template-configurations
   '(("LLDB::Run" :type "lldb" :request "launch" :name "LLDB::Run" :target "" :program "${workspaceFolder}/x11" :cwd "${workspaceFolder}")
	 ("GDBServer Connect Configuration" :type "gdbserver" :name "GDBServer::Connect" :target nil :cwd nil :executable nil :autorun nil :debugger_args nil :env nil :showDevDebugOutput :json-false :printCalls :json-false)
	 ("GDB Run Configuration" :type "gdb" :request "launch" :name "GDB::Run" :target nil :cwd nil)
	 ("cpptools::Run Configuration" :type "cppdbg" :request "launch" :name "cpptools::Run Configuration" :MIMode "gdb" :program "${workspaceFolder}/ replace with your binary" :cwd "${workspaceFolder}")))
 '(dashboard-items
   '((projects . 5)
	 (recents . 5)
	 (bookmarks . 5)
	 (agenda . 5)))
 '(ediff-diff-options "-w")
 '(ediff-split-window-function 'split-window-horizontally)
 '(ediff-window-setup-function 'ediff-setup-windows-plain)
 '(package-selected-packages
   '(evil-numbers rmsbolt compiler-explorer beacon-color beacon dashboard lldb-vscode cpptools dap-cpptools cpp-tools evil-surround evil-commentary evil-leader dap-utils dap-lldb tree-sitter-indent elisp-tree-sitter emacs-tree-sitter tree-sitter-core tree-sitter-langs yasnippet xclip which-key use-package undo-tree tree-sitter theme-changer quelpa org-roam matlab-mode magit lsp-ui linum-relative ligature ivy-rich indent-info helpful gruvbox-theme general flycheck exec-path-from-shell evil-nerd-commenter evil-collection elfeed-org doom-themes doom-modeline dap-mode counsel-projectile company-box command-log-mode auctex)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(setq-default indent-tabs-mode t)
(setq-default tab-width 4) ; Assuming you want your tabs to be four spaces wide
(defvaralias 'c-basic-offset 'tab-width)
(setq-default indent-tabs-mode t)

(use-package dashboard
  :ensure t
  :config
  (setq dashboard-projects-backend 'projectile)
  (dashboard-setup-startup-hook))

;; (defun show-projects ()
;;   (interactive)
;;   (switch-to-buffer "*projects*")
;;   (org-mode)
;;   (insert "#+TITLE: Projects\n\n")
;;   (dolist (project (projectile-relevant-known-projects))
;;     (insert (concat "* " project " [[" project "]] " "\n")))
;;   (goto-char (point-min)))

(setq initial-buffer-choice (lambda () (switch-to-buffer "*dashboard*")))

(define-key prog-mode-map (kbd "<f5>") 'projectile-run-project)
(setq compilation-read-command t)

(use-package beacon)
(beacon-mode 1)



;; (use-package compiler-explorer)
(use-package rmsbolt)

(use-package evil-numbers)
(global-unset-key (kbd "C-M-a"))
(define-key global-map (kbd "C-a") 'evil-numbers/inc-at-pt-incremental)
(define-key global-map (kbd "C-M-a") 'evil-numbers/dec-at-pt-incremental)
