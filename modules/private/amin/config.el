;;; private/amin/config.el -*- lexical-binding: t; -*-


;;
;; Global config
;;

(setq auth-sources (list "~/.authinfo.gpg")
      +doom-modeline-buffer-file-name-style 'relative-from-project  ; 'file-name
      tramp-default-method "ssh")

(setq-default show-trailing-whitespace t)
(add-hook! '(minibuffer-setup-hook doom-popup-mode-hook)
  (setq-local show-trailing-whitespace nil))


;;
;; Keybindings
;;

(load! +hydras)

(map!
 :ne "M-b" #'backward-word
 :ne "M-f" #'forward-word
 :ne "M-q" #'fill-paragraph
 "M-Q"     #'doom/delete-frame

 "C-c <left>"  #'winner-undo
 "C-c <right>" #'winner-redo
 "C-s"         #'swiper
 "C-x C-b"     #'ibuffer
 [C-S-return]  #'recompile
 :e "C-S-l"    #'recenter
 :n "/"        #'swiper
 ;; "C-x C-0"     #'zoom-in/out
 ;; "M--"         #'zoom-out
 ;; "M-="         #'zoom-in
 :n "\\"       #'recentf-open-files

 (:leader
   :desc "Dashboard"    :n "d" #'+doom-dashboard/open
   :desc "Centered"     :n "C" #'centered-window-mode
   ;; :desc "Recent files" :n "k" #'recentf-open-files

   (:desc "buffer" :prefix "b"
     :desc "Ibuffer" :n "i" #'ibuffer)

   (:desc "git" :prefix "g"
     :desc "Git status"          :n "SPC" #'magit-status
     :desc "Git status (prefix)" :n "S"   #'magit-status-prefix
     :desc "Git blame"           :n "B"   #'magit-blame
     :desc "Git clone"           :n "C"   #'magit-clone
     :desc "Git fetch"           :n "f"   #'magit-fetch
     :desc "Git pull"            :n "F"   #'magit-pull
     :desc "Git push"            :n "P"   #'magit-push
     (:desc "Git commit" :prefix "c"
       :desc "commit" :n "c" #'magit-commit
       :desc "amend"  :n "a" #'magit-commit-amend)
     :n "b" nil  ; unbind SPC g b
     (:desc "Git branch" :prefix "b"
       :desc "checkout" :n "b" #'magit-checkout
       :desc "create"   :n "c" #'magit-branch))

   (:desc "project" :prefix "p"
     :desc "Search project with rg" :n "SPC" #'counsel-projectile-rg)

   (:desc "dumb-jump" :prefix "j"
     :desc "Go"   :n "j" #'dumb-jump-go
     :desc "Back" :n "k" #'dumb-jump-back)

   (:desc "toggle" :prefix "t"
     :desc "Distraction-free" :n "d" #'visual-fill-column-mode)

   (:desc "app" :prefix "a"
     (:desc "irc (weechat)" :prefix "i"
       :desc "connect"    :n "c" #'aminb-irc-connect
       :desc "disconnect" :n "d" #'aminb-irc-disconnect)
     (:desc "shell" :prefix "s"
       :desc "eshell"     :n "e" #'+eshell:run
       :desc "multi-term" :n "m" #'multi-term))

   :n "cq" #'fill-paragraph
   :n "ca" #'auto-fill-mode
   :n "cc" #'count-words-region)

 (:after emmet
   (:map emmet-mode-keymap
     :i [C-return] #'emmet-expand-yas))

 (:after ibuffer
   :map ibuffer-mode-map
   "." #'hydra-ibuffer-main/body)

 (:map evil-window-map
   "." #'doom@window-nav/body)

 (:after weechat
   :map weechat-mode-map
   "C-c C-d" #'aminb-irc-disconnect))


;;
;; Modules
;;

(after! smartparens
  ;; Auto-close more conservatively and expand braces on RET
  (let ((unless-list '(sp-point-before-word-p
                       sp-point-after-word-p
                       sp-point-before-same-p)))
    (sp-pair "'"  nil :unless unless-list)
    (sp-pair "\"" nil :unless unless-list))
  (sp-pair "{" nil :post-handlers '(("||\n[i]" "RET") ("| " " "))
           :unless '(sp-point-before-word-p sp-point-before-same-p))
  (sp-pair "(" nil :post-handlers '(("||\n[i]" "RET") ("| " " "))
           :unless '(sp-point-before-word-p sp-point-before-same-p))
  (sp-pair "[" nil :post-handlers '(("| " " "))
           :unless '(sp-point-before-word-p sp-point-before-same-p)))

;; feature/evil
(after! evil-mc
  ;; Make evil-mc resume its cursors when I switch to insert mode
  (add-hook! 'evil-mc-before-cursors-created
    (add-hook 'evil-insert-state-entry-hook #'evil-mc-resume-cursors nil t))
  (add-hook! 'evil-mc-after-cursors-deleted
    (remove-hook 'evil-insert-state-entry-hook #'evil-mc-resume-cursors t)))

;; completion/helm
(after! helm
  ;; Hide header lines in helm
  (set-face-attribute 'helm-source-header nil :height 0.1))

;; lang/org
(after! org-bullets
  ;; The standard unicode characters are usually misaligned depending on the
  ;; font. This bugs me. Personally, markdown #-marks for headlines are more
  ;; elegant, so we use those.
  (setq org-bullets-bullet-list '("#")))

;; core/core-ui
;; disable line numbers
(remove-hook! (prog-mode text-mode conf-mode) #'doom|enable-line-numbers)
;; disable blinking cursor
(remove-hook! 'doom-post-init-hook #'blink-cursor-mode)

;; lang/latex
(setq TeX-PDF-mode t     ; use PDFTeX
      TeX-engine 'xetex  ; use pdflatex instead of latex

      TeX-view-program-selection  ; support zathura for TeX view
      '(((output-dvi style-pstricks)
         "dvips and gv")
        (output-dvi "xdvi")
        (output-pdf "Zathura")
        (output-html "xdg-open")))

(def-package! auctex-latexmk  ; latexmk command for AUCTeX
  :after latex
  :config
  (auctex-latexmk-setup)

  (if (< emacs-major-version 24)
    (defun file-name-base (&optional filename)
      "Return the base name of the FILENAME: no directory, no extension.
FILENAME defaults to `buffer-file-name'."
      (file-name-sans-extension
       (file-name-nondirectory (or filename (buffer-file-name))))))

  ;; Recompile with `latexmk -xelatex' on save
  ;; (add-hook 'after-save-hook
  ;;           (lambda ()
  ;;             (when (string= major-mode 'latex-mode)
  ;;               (TeX-run-latexmk
  ;;                "LaTeX"
  ;;                (format "latexmk -xelatex %s" (buffer-file-name))
  ;;                (file-name-base (buffer-file-name))))))
  )

;; feature/jump
(after! dumb-jump
  (setq dumb-jump-force-searcher 'rg))

;; ;; core/core-ui -- distraction-free and centered editing
(def-package! visual-fill-column
  :init
  (dolist (hook '(prog-mode-hook
                  text-mode-hook
                  notmuch-message-mode-hook
                  ;; notmuch-show-hook
                  ;; haskell-cabal-mode-hook
                  ))
    (add-hook hook #'visual-fill-column-mode))
  :config
  (setq-default visual-fill-column-width (+ fill-column 5)
                visual-fill-column-center-text t
                visual-fill-column-fringes-outside-margins nil)
  ;; Split windows vertically despite large margins, because Emacs otherwise
  ;; refuses to vertically split windows with large margins
  (setq split-window-preferred-function
        #'visual-fill-column-split-window-sensibly))

;; lang/org
(after! org-mode
  (remove-hook 'org-mode-hook #'visual-line-mode))

;; (after! org
;;   (setq-default org-indent-indentation-per-level 4))

;; lang/web
(after! web-mode
  (defun my-web-mode-hook ()
  "Hook for Web mode."
  (setq web-mode-markup-indent-offset 1
        web-mode-code-indent-offset 1
        web-mode-css-indent-offset 1))
  (add-hook 'web-mode-hook  'my-web-mode-hook))

(add-to-list 'magic-mode-alist '("<!doctype html>" . web-mode))

;; lang/haskell
(defun haskell-style ()
  "Sets the current buffer to use Haskell Style. Meant to be
  added to `haskell-mode-hook'"
  (interactive)
  (setq tab-width 2
        haskell-indentation-layout-offset 2
        haskell-indentation-left-offset 2
        haskell-indentation-ifte-offset 2))

(add-hook 'haskell-mode-hook #'haskell-style)

;; core/core-os
(def-package! exec-path-from-shell
  :defer 1
  :init
  (setq exec-path-from-shell-check-startup-files nil)
  :config
  (exec-path-from-shell-copy-env "SSH_AGENT_PID")
  (exec-path-from-shell-copy-env "SSH_AUTH_SOCK"))

;; completion/ivy
(after! ivy
  (setq ivy-magic-slash-non-match-action 'ivy-magic-slash-non-match-cd-selected))

;; core/core-ui
(def-package! page-break-lines
  :config (global-page-break-lines-mode))

;; feature/version-control
(def-package! magit
  :commands magit-clone)

(after! magit
  ;; Set Magit's repo dirs for `magit-status' from Projectile's known projects.
  ;; Initialize the `magit-repository-directories' immediately after Projectile
  ;; was loaded, and update it every time we switched projects, because the new
  ;; project might have been unknown before.
  (defun magit-repo-dirs-from-projectile ()
    "Set `magit-repo-dirs' from known Projectile projects."
    (let ((project-dirs (bound-and-true-p projectile-known-projects)))
      ;; Remove trailing slashes from project directories, because Magit adds
      ;; trailing slashes again, which breaks the presentation in the Magit
      ;; prompt.
      (setq magit-repository-directories
            (mapcar #'directory-file-name project-dirs))))

  (after! projectile
    (magit-repo-dirs-from-projectile))

  (add-hook! 'projectile-switch-project-hook
    #'magit-repo-dirs-from-projectile))

(defun magit-status-prefix ()
  "Always call `magit-status' with prefix arg. This will cause
magit to ask for a repository even if the current directory is
already a repository. This is handy when wanting to switch over
to another project."
  (interactive)
  (let ((current-prefix-arg t))
    (call-interactively 'magit-status)))


;; lang/nix
(def-package! nix-mode)

(def-package! dired+
  :config (diredp-toggle-find-file-reuse-dir 1))


;; ibuffer
(after! ibuffer
  (setq ibuffer-show-empty-filter-groups nil)
  ;; Human-readable Size column
  ;; from https://www.emacswiki.org/emacs/IbufferMode#toc12
  (define-ibuffer-column size-h
    (:name "Size" :inline t)
    (cond
     ((> (buffer-size) 1000000) (format "%7.1fM" (/ (buffer-size) 1000000.0)))
     ((> (buffer-size) 100000) (format "%7.0fk" (/ (buffer-size) 1000.0)))
     ((> (buffer-size) 1000) (format "%7.1fk" (/ (buffer-size) 1000.0)))
     (t (format "%8d" (buffer-size)))))

  (setq ibuffer-formats
        '((mark modified read-only " "
                (name 18 18 :left :elide)
                " "
                (size-h 9 -1 :right)
                " "
                (mode 16 16 :left :elide)
                " "
                filename-and-process))))

(def-package! ibuffer-vc
  :config
  (add-hook
   'ibuffer-hook
   (lambda ()
     (ibuffer-vc-set-filter-groups-by-vc-root)
     (unless (eq ibuffer-sorting-mode 'alphabetic)
       (ibuffer-do-sort-by-alphabetic)))))


;; weechat.el

(defvar +irc--workspace-name "irc")

(def-package! weechat
  :commands
  (weechat-connect
   weechat-disconnect
   weechat-monitor-buffer)
  :config
  (setq weechat-host-default "localhost"
        weechat-port-default 49000
        weechat-more-lines-amount 50
        weechat-buffer-line-limit 4000
        weechat-notification-mode t
        weechat-auto-monitor-buffers t
        weechat-buffer-kill-buffers-on-disconnect t
        weechat-completing-read-function 'weechat--try-ivy)
  (set! :evil-state 'weechat-mode 'insert)
  ;; weechat's buffers should be real
  (push #'weechat-buffer-p doom-real-buffer-functions))

(defun weechat-tunnel (command)
  (call-process "weechat-tunnel" nil nil nil command))

(defun aminb-irc-connect ()
  "Connect to my WeeChat relay"
  (interactive)
  (progn
    (message "Tunneling...")
    (weechat-tunnel "start")
    (message "Tunnel established")
    (+workspace-switch +irc--workspace-name t)
    (weechat-connect)))

(defun aminb-irc-disconnect ()
  "Disconnect from my WeeChat relay"
  (interactive)
  (progn
    (weechat-disconnect)
    (weechat-tunnel "stop")
    (message "Tunnel stopped")
    (+workspace/delete +irc--workspace-name)))


;;
;; Themes
;;

;; (require 'gruvbox-theme)
;; (require 'gruvbox-light-hard-theme)
;; (require 'gruvbox-dark-medium-theme)

(require 'tao-theme)
(require 'tao-yang-theme)
;; (require 'tao-yin-theme)

;; (require 'flatui-theme)
