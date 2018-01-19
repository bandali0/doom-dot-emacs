;;; private/notmuch/+bindings.el -*- lexical-binding: t; -*-

(defun aminb-notmuch ()
  "Delete other windows, then launch `notmuch'."
  (interactive)
  (require 'notmuch)
  (delete-other-windows)
  (notmuch))

(map!
 :leader
 :desc "notmuch" :n "m" #'aminb-notmuch
 (:desc "search" :prefix "/"
   :desc "notmuch" :n "m" #'counsel-notmuch))

(map!
 (:after notmuch-hello
   :map notmuch-hello-mode-map
   "g" #'notmuch-poll-and-refresh-this-buffer
   "i" (lambda ()
         "Search for `inbox' tagged messages."
         (interactive)
         (notmuch-hello-search "tag:inbox"))
   "u" (lambda ()
         "Search for `unread' tagged messages."
         (interactive)
         (notmuch-hello-search "tag:unread")))
 (:after notmuch
   (:map notmuch-search-mode-map
     "g" #'notmuch-poll-and-refresh-this-buffer
     "k" (lambda ()
           "Mark message read."
           (interactive)
           (notmuch-search-tag '("-unread"))
           ;; (notmuch-search-archive-thread)
           (notmuch-search-next-thread))
     "u" (lambda ()
           "Mark message unread."
           (interactive)
           (notmuch-search-tag '("+unread"))
           (notmuch-search-next-thread))
     "K" (lambda ()
           "Mark message deleted."
           (interactive)
           (notmuch-search-tag '("-unread" "-inbox" "+deleted"))
           (notmuch-search-archive-thread))
     "S" (lambda ()
           "Mark message as spam"
           (interactive)
           (notmuch-search-tag '("-unread" "-inbox" "+spam"))
           (notmuch-search-archive-thread)))
   (:map notmuch-tree-mode-map  ; additional bindings
     "S" (lambda ()
      "Mark message as spam"
      (interactive)
      (notmuch-tree-tag '("-unread" "-inbox" "+spam"))
      (notmuch-tree-archive-thread)))))
