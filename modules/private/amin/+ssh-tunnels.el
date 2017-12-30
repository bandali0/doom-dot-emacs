;;; private/amin/+ssh-tunnels.el -*- lexical-binding: t; -*-

(defun ssh-tunnel-command (tunnel command)
  (let* ((name (plist-get tunnel ':name))
         (local-port (plist-get tunnel ':local-port))
         (remote-port (plist-get tunnel ':remote-port))
         (host (plist-get tunnel ':host))
         (login (plist-get tunnel ':login))
         (args (cond ((eq command :run)
                      (list "-M" "-f" "-N" "-T"
                            "-L" (format "%s:%s:%s" local-port host remote-port)))
                     ((eq command :kill)
                      (list "-O" "exit"))
                     ((eq command :check)
                      (list "-O" "check"))
                     (t (error "Unknown ssh-tunnel command '%s'" command)))))
    (apply 'call-process "ssh" nil nil nil
           "-S" (shell-quote-argument name)
           (append args
                   (list login)))))

;; (defun weechat-tunnel (command)
;;   (call-process "weechat-tunnel" nil nil nil command))

(defun ssh-tunnel-check (tunnel)
  (eql 0 (ssh-tunnel-command tunnel :check)))

(defun ssh-tunnel-start (tunnel)
  (interactive "P")
  (let ((name (plist-get tunnel ':name)))
    (when (not (ssh-tunnel-check tunnel))
      (message "Starting tunnel...")
      (ssh-tunnel-command tunnel :run)
      (message "Tunnel '%s' started." name))))

(defun ssh-tunnel-stop (tunnel)
  (interactive)
  (let ((name (plist-get tunnel ':name)))
    (when (ssh-tunnel-check tunnel)
      (message "Stopping tunnel...")
      (ssh-tunnel-command tunnel :kill)
      (message "Tunnel '%s' stopped." name))))
