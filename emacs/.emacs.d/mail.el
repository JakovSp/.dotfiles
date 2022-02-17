;;(require 'mu4e)
(use-package pinentry)
(setq epa-pinentry-mode 'loopback)
(use-package auth-source-pass)
(setq auth-sources '(password-store))
(auth-source-pass-enable)

(use-package smtpmail)

(use-package mu4e
  :ensure nil
  :config
  (setq mu4e-change-filenames-when-moving t)
  (setq mu4e-get-mail-command "mbsync -a"))

(require 'org-mu4e)
(require 'mu4e-contrib)
(require 'smtpmail)

(auth-source-pass-enable)
(setq auth-source-debug t)
(setq auth-source-do-cache nil)
(setq mu4e-update-interval 180)
(setq auth-sources '(password-store))
(setq message-kill-buffer-on-exit t)
(setq message-send-mail-function 'smtpmail-send-it)
(setq send-mail-function 'smtpmail-send-it)
(setq mu4e-attachment-dir "~/Documents")
(setq mu4e-change-filenames-when-moving t)
(setq mu4e-completing-read-function 'completing-read)
(setq mu4e-compose-complete-addresses t)
(setq mu4e-compose-context-policy nil)
(setq mu4e-compose-dont-reply-to-self t)
(setq mu4e-compose-keep-self-cc nil)
(setq mu4e-context-policy 'pick-first)
(setq mu4e-get-mail-command "mbsync -a")
(setq mu4e-headers-include-related t)
(setq mu4e-sent-messages-behavior 'delete)
(setq mu4e-view-show-addresses t)
(setq mu4e-view-show-images t)
(setq smtpmail-debug-info t)
(setq smtpmail-stream-type 'starttls)
(setq mm-sign-option 'guided)
(setq send-mail-program "/usr/bin/msmtp")

;; (when (fboundp 'imagemagick-register-types)
;; (imagemagick-register-types))

;; (defun sign-or-encrypt-message ()
;; (let ((answer (read-from-minibuffer "Sign or encrypt?\nEmpty to do nothing.\n[s/e]: ")))
;;   (cond
;;    ((string-equal answer "s") (progn
;; 								(message "Signing message.")
;; 								(mml-secure-message-sign-pgpmime)))
;;    ((string-equal answer "e") (progn
;; 								(message "Encrypt and signing message.")
;; 								(mml-secure-message-encrypt-pgpmime)))
;;    (t (progn
;; 		(message "Dont signing or encrypting message.")
;; 		nil)))))

;; (add-hook 'message-send-hook 'sign-or-encrypt-message)

(setq mu4e-contexts
	`( ,(make-mu4e-context
			:name "fesb"
			:enter-func (lambda ()
						(mu4e-message "Entering fesb context")
						(when (string-match-p (buffer-name (current-buffer)) "mu4e-main")
							(revert-buffer)))
			:leave-func (lambda ()
						(mu4e-message "Leaving fesb context")
						(when (string-match-p (buffer-name (current-buffer)) "mu4e-main")
							(revert-buffer)))
			:match-func (lambda (msg)
						(when msg
							(or (mu4e-message-contact-field-matches msg :to "jspahi00@fesb.hr")
								(mu4e-message-contact-field-matches msg :from "jspahi00@fesb.hr")
								(mu4e-message-contact-field-matches msg :cc "jspahi00@fesb.hr")
								(mu4e-message-contact-field-matches msg :bcc "jspahi00@fesb.hr")
								(string-match-p "^/fesb/" (mu4e-message-field msg :maildir)))))
			:vars '( ( user-mail-address            . "jspahi00@fesb.hr" )
					( smtpmail-smtp-user           . "jspahi00@fesb.hr" )
					( smtpmail-smtp-server         . "smtp-mail.outlook.com" )
					( smtpmail-smtp-service        . 587 )
					( mu4e-bookmarks
					.
					(( :name  "Unread messages"
								:query "maildir:/fesb/INBOX AND flag:unread AND NOT flag:trashed AND NOT outdoorexperten"
								:key ?u)
						( :name "Today's messages"
								:query "maildir:/fesb/INBOX AND date:today..now"
								:key ?t)
						( :name "Last 7 days"
								:query "maildir:/fesb/INBOX AND date:7d..now"
								:hide-unread t
								:key ?w)
						( :name "Deleted"
								:query "flag:trashed"
								:key ?d)
						( :name "Possibly garbage"
								:query "bokio OR outdoorexperten"
								:key ?g)))))
		))


(defvar my-mu4e-account-alist
  '(("Outlook"
     (mu4e-sent-folder "/Sent")
     (user-mail-address "jspahi00@fesb.hr")
     (smtpmail-smtp-user "jspahi00@fesb.hr")
     (smtpmail-local-domain "outlook.com")
     (smtpmail-smtp-server "smtp-mail.outlook.com")
     (smtpmail-default-smtp-server "smtp-mail.outlook.com")
     (smtpmail-starttls-credentials '(("smtp-mail.outlook.com" 587 nil nil)))
     (smtpmail-auth-credentials '(("smtp-mail.outlook.com" 587 "jspahi00@fesb.hr" nil)))
     (smtpmail-stream-type starttls)
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

