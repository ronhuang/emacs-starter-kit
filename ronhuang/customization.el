;; load path
(when (eq system-type 'gnu/linux)
  (setq load-path (cons "~/share/emacs/site-lisp/w3m" load-path)))

;; color-theme
(color-theme-wombat)

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
  (setq exec-path (cons "/usr/local/bin" exec-path))
  (setenv "PATH" (concat "/usr/local/bin:" (getenv "PATH"))))
(when (eq system-type 'gnu/linux)
  ;; use chrome as default browser
  (setq browse-url-browser-function 'browse-url-generic
        browse-url-generic-program "google-chrome"))

;; flyspell-mode
(setq-default ispell-program-name "aspell")

;; EMMS
(require 'emms-setup)
(emms-standard)
(emms-default-players)

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

;; cmake
(require 'cmake-mode)
(setq auto-mode-alist
      (append '(("CMakeLists\\.txt\\'" . cmake-mode)
                ("\\.cmake\\'" . cmake-mode))
              auto-mode-alist))

;; shell-pop
(require 'shell-pop)
(shell-pop-set-internal-mode "ansi-term")
(shell-pop-set-internal-mode-shell "/bin/bash")
(shell-pop-set-window-height 40)
(shell-pop-set-window-position "bottom")
(global-set-key [f8] 'shell-pop)

;; rcirc
(eval-after-load 'rcirc '(require 'rcirc-notify))

;; w3m
(require 'w3m-load)
(setq w3m-use-cookies t)

;; desktop save
(desktop-save-mode 1)

;; winner
(winner-mode 1)

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

;; Full screen on Linux
(defun switch-full-screen ()
  (interactive)
  (shell-command "wmctrl -r :ACTIVE: -btoggle,fullscreen"))

;; keyboard shortcuts
(global-set-key "\C-x\C-r" 'recentf-open-files)
(global-set-key (kbd "M-<return>") 'complete-tag)
(global-set-key (kbd "C-x t") 'ansi-term)
(when (eq system-type 'darwin)
  (global-set-key [f11] 'ns-toggle-fullscreen))
(when (eq system-type 'gnu/linux)
  (global-set-key [f11] 'switch-full-screen))

;; daemon
(server-start)
