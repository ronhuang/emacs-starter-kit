;; ELPA, may need to invoke starter-kit-elpa-install.
(add-to-list 'starter-kit-packages 'iresize t)
(add-to-list 'starter-kit-packages 'smex t)
(add-to-list 'starter-kit-packages 'color-theme t)
(add-to-list 'starter-kit-packages 'markdown-mode t)
(add-to-list 'starter-kit-packages 'zenburn t)
(add-to-list 'starter-kit-packages 'ruby-electric t)
(add-to-list 'starter-kit-packages 'htmlize t)
(add-to-list 'starter-kit-packages 'erc-highlight-nicknames t)

;; color-theme
(color-theme-solarized-dark)

;; show trailing whitespace
(setq-default show-trailing-whitespace t)

;; set tabs to 4 spaces
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)

;; auto-fill
(setq fill-column 78)

;; prevent emacs from breaking hard link
(setq backup-by-copying-when-linked t)

;; platform-specific stuff
(when (eq system-type 'darwin)
  ;; use command key as alt key
  (setq mac-command-modifier 'meta)
  (setq exec-path (cons "/usr/texbin" exec-path))
  (setenv "PATH" (concat "/usr/texbin:" (getenv "PATH")))
  (setq exec-path (cons "/usr/local/bin" exec-path))
  (setenv "PATH" (concat "/usr/local/bin:" (getenv "PATH")))
  (set-frame-font
   "-apple-Monaco-medium-normal-normal-*-14-*-*-*-m-0-iso10646-1"))
(when (eq system-type 'gnu/linux)
  ;; use chrome as default browser
  (setq browse-url-browser-function 'browse-url-generic
        browse-url-generic-program "google-chrome")
  (set-frame-font
   "-microsoft-Consolas-normal-normal-normal-*-18-*-*-*-m-0-iso10646-1"))
(when (eq system-type 'windows-nt)
  (set-frame-font
   "-outline-Consolas-normal-r-normal-normal-14-97-96-96-c-*-iso8859-1"))

;; c-mode
(add-hook 'c-mode-common-hook
          '(lambda ()
             (c-set-style "stroustrup")))

;; flyspell-mode
(setq-default ispell-program-name "aspell")

;; column-number-mode
(column-number-mode t)

;; term-mode
(add-hook 'term-mode-hook
          (lambda ()
            (setq show-trailing-whitespace nil)))

;; goto-last-change
(autoload 'goto-last-change "goto-last-change"
  "Set point to the position of the last change." t)
(global-set-key "\C-x\C-\\" 'goto-last-change)

;; apropos-url
(require 'apropos-url)

;; twittering-mode
(require 'twittering-mode)
(setq twittering-username "ronhuang")
(add-hook 'twittering-mode-hook
          (lambda ()
            (setq show-trailing-whitespace nil)))

;; lua-mode
(setq auto-mode-alist (cons '("\\.lua$" . lua-mode) auto-mode-alist))
(autoload 'lua-mode "lua-mode" "Lua editing mode." t)

;; php-mode
(autoload 'php-mode "php-mode" "Major mode for editing php code." t)
(add-to-list 'auto-mode-alist '("\\.php$" . php-mode))
(add-to-list 'auto-mode-alist '("\\.inc$" . php-mode))

;; cmake
(require 'cmake-mode)
(setq auto-mode-alist
      (append '(("CMakeLists\\.txt\\'" . cmake-mode)
                ("\\.cmake\\'" . cmake-mode))
              auto-mode-alist))

;; multi-term
(require 'multi-term)
(setq multi-term-program "/bin/bash")
(global-set-key (kbd "C-c t") 'multi-term-next)
(global-set-key (kbd "C-c T") 'multi-term) ;; create a new one

;; shell-pop
(require 'shell-pop)
(add-to-list 'shell-pop-internal-mode-list
             '("multi-term" "*terminal<1>*" '(lambda () (multi-term))))
(shell-pop-set-internal-mode "multi-term")
(shell-pop-set-internal-mode-shell "/bin/bash")
(shell-pop-set-window-height 40)
(shell-pop-set-window-position "bottom")
(global-set-key [f8] 'shell-pop)

;; rcirc
(eval-after-load 'rcirc '(require 'rcirc-notify))

;; desktop save
(desktop-save-mode 1)

;; winner
(winner-mode 1)

;; iresize
(require 'iresize)

;; yaml mode
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

;; smex
(require 'smex)
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;; obj-j
(require 'objj-mode)

;; nxhtml-mode
(load "~/.emacs.d/ronhuang/nxhtml/autostart.el")
(setq mumamo-chunk-coloring 4)

;; erc
(setq erc-autojoin-channels-alist
      '(("freenode.net" "#emacs" "#node.js" "#git" "#chromium" "#tossug" "#ubuntu-tw")
        ("debian.org" "#dot")))

;; css-mode
(add-hook 'css-mode-hook
          (lambda ()
            (setq css-indent-level 2)))

;; aHg: An Emacs front-end for the Mercurial SCM
(require 'ahg)
(add-hook 'ahg-short-log-mode-hook
          (lambda ()
            (setq show-trailing-whitespace nil)))
(add-hook 'ahg-diff-mode-hook
          (lambda ()
            (setq show-trailing-whitespace nil)))

;; obj-c mode
(add-to-list 'auto-mode-alist '("\\.mm$" . objc-mode))

;; go mode
(require 'go-mode-load)

;; reloads the current file
(defun reload-file ()
  "Reloads the current file."
  (interactive)
  (find-file (buffer-name)))

;; print a ascii table
(defun ascii-table ()
  "Print the ascii table."
  (interactive)
  (switch-to-buffer "*ASCII*")
  (erase-buffer)
  (insert (format "ASCII characters up to number %d.\n" 254))
  (let ((i 0))
    (while (< i 254)
      (setq i (+ i 1))
      (insert (format "%4d %c\n" i i))))
  (beginning-of-buffer))

;; convert from DOS > UNIX
(defun dos2unix ()
  (interactive)
  (goto-char (point-min))
  (while (search-forward "\r" nil t) (replace-match "")))

;; convert from UNIX > DOS
(defun unix2dos ()
  (interactive)
  (goto-char (point-min))
  (while (search-forward "\n" nil t) (replace-match "\r\n")))

;; word count
;; combine of http://www.emacswiki.org/emacs/WordCount and
;; http://www.neverfriday.com/sweetfriday/2008/06/emacs-tip-word-counting-with-a.html
(defun wc ()
  (interactive)
  (let ((start (if mark-active (region-beginning) (point-min)))
        (end (if mark-active (region-end) (point-max))))
    (message "Word count: %s" (how-many "\\w+" start end))))

;; toggle show trailing whitespace
;; from http://www.emacswiki.org/emacs/DaveBenjamin
(defun toggle-show-trailing-whitespace ()
  "Toggles the highlighting of trailing whitespace."
  (interactive)
  (set-variable 'show-trailing-whitespace (not show-trailing-whitespace)))

;; keyboard shortcuts
(global-set-key "\C-x\C-r" 'recentf-open-files)
(global-set-key (kbd "M-<return>") 'complete-tag)
(global-set-key (kbd "C-x t") 'ansi-term)
(when (eq system-type 'darwin)
  (global-set-key [f11] 'ns-toggle-fullscreen))
(when (eq system-type 'gnu/linux)
  (global-set-key [f11] 'toggle-fullscreen))

;; daemon
(server-start)
