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

(add-hook 'c-mode-hook 'lsp)
(add-hook 'c++-mode-hook 'lsp)

(with-eval-after-load 'lsp-mode
  (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration)
  (require 'dap-cpptools)
  (yas-global-mode))

(use-package dap-mode)
(require 'dap-utils)
(dap-mode 1)
(dap-ui-mode 1)
(require 'dap-gdb-lldb)

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

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

(use-package rmsbolt)
