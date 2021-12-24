(setq inhibit-startup-message t)

(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 10)        ; Give some breathing room

(menu-bar-mode -1)            ; Disable the menu bar

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))

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

(setq-default indent-tabs-mode t)
(setq-default tab-width 4) ; Assuming you want your tabs to be four spaces wide
(defvaralias 'c-basic-offset 'tab-width)
(setq-default indent-tabs-mode t)

(use-package dashboard
  :ensure t
  :config
  (setq dashboard-projects-backend 'projectile)
  (dashboard-setup-startup-hook))

(setq initial-buffer-choice (lambda () (switch-to-buffer "*dashboard*")))

(use-package beacon)
(beacon-mode 1)

(electric-pair-mode)
