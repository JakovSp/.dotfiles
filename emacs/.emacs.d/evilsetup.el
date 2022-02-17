;;(require 'mu4e)
(setq auth-sources '(password-store))
(auth-source-pass-enable)

(use-package smtpmail)

(use-package mu4e
  :ensure nil
  :config
  (setq mu4e-change-filenames-when-moving t)
  (setq mu4e-get-mail-command "mbsync -a"))

(setq evil-want-keybinding nil)

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-C-u-scroll t)
  (setq evil-respect-visual-line-mode t)
  (setq evil-undo-system 'undo-tree)
  (setq evil-undo-tree-autosave-history t)
  (setq evil-symbol-word-search 'symbol)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-normal-state-map (kbd "K") 'man)
  (define-key evil-normal-state-map (kbd "s") 'evil-surround-edit)
  (define-key evil-visual-state-map (kbd "s") 'evil-surround-region)
  (define-key evil-normal-state-map (kbd "z z") 'save-buffer)
  ;(define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)
  
  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))
(add-hook 'after-change-major-mode-hook (lambda () (modify-syntax-entry ?_ "w")))

;; (use-package general
;;   :config
;;   (general-evil-setup t)

;;   (general-create-definer jakov/leader-key-def
;; 			  :keymaps '(normal insert visual emacs)
;; 			  :prefix "SPC"
;; 			  :global-prefix "C-SPC"))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))


(use-package evil-commentary)
(evil-commentary-mode)

(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1))

(use-package undo-tree)
(global-undo-tree-mode)
(setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo")))
(setq undo-tree-auto-save-history t)

(use-package evil-numbers)
(global-unset-key (kbd "C-M-a"))
(define-key global-map (kbd "C-a") 'evil-numbers/inc-at-pt-incremental)
(define-key global-map (kbd "C-M-a") 'evil-numbers/dec-at-pt-incremental)

(use-package evil-visualstar)
(global-evil-visualstar-mode)
(setq evil-visualstar/persistent 't)


;; test_test test_test
