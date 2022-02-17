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
(use-package quelpa)

(load-user-file "style.el")


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

(use-package counsel
  :bind (("M-x" . counsel-M-x)
	 ("C-x b" . counsel-ibuffer)
	 ("C-x C-f" . counsel-find-file)
	 :map minibuffer-local-map
	 ("C-r" . counsel-minibuffer-history)))
  
(use-package ivy-rich
  :init (ivy-rich-mode 1))

(global-set-key (kbd "C-M-j") 'ivy-switch-buffer)

(load-user-file "evilsetup.el")

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

(defun projectile-run-project-save-files (arg)
  "Run project run command.

Normally you'll be prompted for a compilation command, unless
variable `compilation-read-command'.  You can force the prompt
with a prefix ARG."
  (interactive "P")
  (let ((command (projectile-run-command (projectile-compilation-dir))))
    (projectile--run-project-cmd command projectile-run-cmd-map
								 :save-buffers 't
                                 :use-comint-mode projectile-run-use-comint-mode)))


(defun projectile-run-project-without-prompt (&optional prompt)
  (interactive "P")
  (let ((compilation-read-command
         (or (not (projectile-run-command (projectile-compilation-dir)))
             prompt)))
    (projectile-run-project prompt)))

(define-key prog-mode-map (kbd "<f5>") 'projectile-run-project-without-prompt)
(define-key dired-mode-map (kbd "<f5>") 'projectile-run-project-without-prompt)
(define-key compilation-mode-map (kbd "<f5>") 'projectile-run-project-without-prompt)

(use-package magit
  :ensure t
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(setq global-magit-auto-revert-mode t)

(load-user-file "lspdap.el")

(setq garbage-collection-messages t)
(setq gc-cons-threshold (* 100 1024 1024)
      read-process-output-max (* 1024 1024)
      treemacs-space-between-root-nodes nil
      company-idle-delay 0.0
      company-minimum-prefix-length 2
      lsp-idle-delay 0.1)  ;; clangd is fast


(use-package yasnippet)
(add-hook 'yas-minor-mode-hook (lambda () (yas-activate-extra-mode 'fundamental-mode)))

(load-user-file "mail.el")

(use-package org)
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


(load-user-file "news.el")

(global-set-key (kbd "C-x o") 'ace-window)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("78c4238956c3000f977300c8a079a3a8a8d4d9fee2e68bad91123b58a4aa8588" default))
 '(dap-debug-template-configurations
   '(("LLDB::Run" :type "lldb" :request "launch" :name "LLDB::Run" :target "" :program "${workspaceFolder}/x11" :cwd "${workspaceFolder}")
	 ("GDBServer Connect Configuration" :type "gdbserver" :name "GDBServer::Connect" :target nil :cwd nil :executable nil :autorun nil :debugger_args nil :env nil :showDevDebugOutput :json-false :printCalls :json-false)
	 ("GDB Run Configuration" :type "gdb" :request "launch" :name "GDB::Run" :target nil :cwd nil)
	 ("cpptools::Run Configuration" :type "cppdbg" :request "launch" :name "cpptools::Run Configuration" :MIMode "gdb" :program "${workspaceFolder}/ replace with your binary" :cwd "${workspaceFolder}")))
 '(dap-ui-many-windows-mode t)
 '(dashboard-items
   '((projects . 5)
	 (recents . 5)
	 (bookmarks . 5)
	 (agenda . 5)))
 '(ediff-diff-options "-w")
 '(ediff-split-window-function 'split-window-horizontally)
 '(ediff-window-setup-function 'ediff-setup-windows-plain)
 '(gdb-many-windows t)
 '(lsp-vetur-format-options-tab-size 4)
 '(package-selected-packages
   '(doremi doremi-frm frame-cmds bzg-big-fringe hl-todo adaptive-wrap indent-guide highlight-indent-guides elcast evil-visualstar pinentry pinentry-emacs mstmp-oauth2 msmtp-oauth2 oauth2 evil-numbers rmsbolt compiler-explorer beacon-color beacon dashboard lldb-vscode cpptools dap-cpptools cpp-tools evil-surround evil-commentary evil-leader dap-utils dap-lldb tree-sitter-indent elisp-tree-sitter emacs-tree-sitter tree-sitter-core tree-sitter-langs yasnippet xclip which-key use-package undo-tree tree-sitter theme-changer quelpa org-roam matlab-mode magit lsp-ui linum-relative ligature ivy-rich indent-info helpful gruvbox-theme general flycheck exec-path-from-shell evil-nerd-commenter evil-collection elfeed-org doom-themes doom-modeline dap-mode counsel-projectile company-box command-log-mode auctex))
 '(tab-width 4))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(hl-line ((t (:extend t :underline t)))))
