;;; private/notmuch/config.el -*- lexical-binding: t; -*-

(load! +bindings)

(defvar maildir "~/mail")

(def-package! smtpmail
  :config
  (setq smtpmail-default-smtp-server "nix.aminb.org"
        smtpmail-local-domain "aminb.org"
        smtpmail-smtp-server "nix.aminb.org"
        smtpmail-stream-type 'starttls
        smtpmail-smtp-service 587))

(def-package! sendmail
  :config
  (setq send-mail-function 'smtpmail-send-it))

(def-package! message
  :config
  (setq message-kill-buffer-on-exit t
        message-directory "drafts"
        message-user-fqdn "aminb.org")
  (add-hook 'message-mode-hook
            (lambda () (setq fill-column 65
                        message-fill-column 65)))
  (add-hook 'message-mode-hook
            #'flyspell-mode)
  ;; TODO: is there a way to only run this when replying and not composing?
  (add-hook 'notmuch-message-mode-hook
            (lambda () (progn
                    (newline)
                    (newline)
                    (forward-line -1)
                    (forward-line -1))))
  ;; (add-hook 'message-setup-hook
  ;;           #'mml-secure-message-sign-pgpmime)
  )

(after! mml-sec
  (setq mml-secure-openpgp-encrypt-to-self t
        mml-secure-openpgp-sign-with-sender t))

(def-package! notmuch
  :config
  (setq notmuch-hello-sections
        '(notmuch-hello-insert-header
          notmuch-hello-insert-saved-searches
          ;; notmuch-hello-insert-search
          notmuch-hello-insert-alltags)
        notmuch-search-oldest-first nil
        notmuch-show-all-tags-list t
        notmuch-hello-thousands-separator ","
        notmuch-fcc-dirs
        '(("amin@aminb.org" . "amin/Sent")
          ("aminb@gnu.org"  . "gnu/Sent")
          (".*"             . "sent")))
  (add-hook 'visual-fill-column-mode-hook
            (lambda ()
              (when (string= major-mode 'notmuch-message-mode)
                (setq visual-fill-column-width 70))))
  (set! :evil-state 'notmuch-message-mode 'insert)
  (advice-add #'notmuch-bury-or-kill-this-buffer
              :override #'kill-this-buffer))

(def-package! counsel-notmuch
  :commands counsel-notmuch)

(after! notmuch-crypto
  (setq notmuch-crypto-process-mime t))

(after! evil
  (mapc (lambda (str) (evil-set-initial-state (car str) (cdr str)))
        '((notmuch-hello-mode . emacs)
          (notmuch-search-mode . emacs)
          (notmuch-tree-mode . emacs))))

(after! recentf
  (add-to-list 'recentf-exclude (expand-file-name maildir)))

;; notmuch's buffers should be real
(defun notmuch-buffer-p (buffer)
  (string-match-p "^\\*notmuch-" (buffer-name buffer)))
(push #'notmuch-buffer-p doom-real-buffer-functions)
