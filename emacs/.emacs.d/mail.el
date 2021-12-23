(require 'mu4e)
(require 'org-mu4e)
(require 'mu4e-contrib)
(require 'smtpmail)

(auth-source-pass-enable)
(setq auth-source-debug t)
(setq auth-source-do-cache nil)
(setq auth-sources '(password-store))
(setq message-kill-buffer-on-exit t)
(setq message-send-mail-function 'smtpmail-send-it)
(setq mu4e-attachment-dir "~/Downloads")
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

(when (fboundp 'imagemagick-register-types)
(imagemagick-register-types))

(defun sign-or-encrypt-message ()
(let ((answer (read-from-minibuffer "Sign or encrypt?\nEmpty to do nothing.\n[s/e]: ")))
  (cond
   ((string-equal answer "s") (progn
								(message "Signing message.")
								(mml-secure-message-sign-pgpmime)))
   ((string-equal answer "e") (progn
								(message "Encrypt and signing message.")
								(mml-secure-message-encrypt-pgpmime)))
   (t (progn
		(message "Dont signing or encrypting message.")
		nil)))))

(add-hook 'message-send-hook 'sign-or-encrypt-message)
(setq mu4e-contexts
	`( ,(make-mu4e-context
			:name "fesb"
			:enter-func (lambda ()
						(mu4e-message "Entering fesb context")
						(when (string-match-p (buffer-name (current-buffer)) "mu4e-main")
							(revert-buffer)))
			:leave-func (lambda ()
						(mu4e-message "Leaving gmail context")
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
								:query "maildir:/gmail/Inbox AND flag:unread AND NOT flag:trashed AND NOT outdoorexperten"
								:key ?u)
						( :name "Today's messages"
								:query "maildir:/gmail/Inbox AND date:today..now"
								:key ?t)
						( :name "Last 7 days"
								:query "maildir:/gmail/Inbox AND date:7d..now"
								:hide-unread t
								:key ?w)
						( :name "Deleted"
								:query "flag:trashed"
								:key ?d)
						( :name "Possibly garbage"
								:query "bokio OR outdoorexperten"
								:key ?g)))))
		))
