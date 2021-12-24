(use-package elfeed)

(use-package elfeed-org
  :ensure t
  :config
  (setq elfeed-show-entry-switch 'display-buffer)
  (setq elfeed-db-directory "~/.cache/elfeed"))

(elfeed-org)
(setq rmh-elfeed-org-files (list "~/.emacs.d/elfeed.org"))
