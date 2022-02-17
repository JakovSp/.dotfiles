(use-package elfeed)

(use-package elfeed-org
  :ensure t
  :config
  (setq elfeed-show-entry-switch 'switch-to-buffer)
  (setq elfeed-db-directory "~/.cache/elfeed"))

(elfeed-org)
(setq rmh-elfeed-org-files (list "~/.emacs.d/elfeed.org"))

(defun ga/play-with-mpv (start end)
"Play the link in the region with mpv"
(interactive "r")
(shell-command (concat "mpv " (buffer-substring start end) "\&")))

(define-key elfeed-show-mode-map (kbd "C-c o") 'ga/play-with-mpv)

(quelpa '(elcast :repo "douglasdavis/elcast" :fetcher github))
(use-package elcast)

(add-hook 'emacs-startup-hook (lambda () (run-at-time 5 (* 1 60) 'elfeed-update)))
