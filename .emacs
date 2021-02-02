(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

;;(let ((default-directory "~/.emacs.d/site-lisp/"))
;;  (normal-top-level-add-subdirs-to-load-path))
(package-initialize)

(eval-when-compile
  ;; Following line is not needed if use-package.el is in ~/.emacs.d
;;  (add-to-list 'load-path "<path where use-package is installed>")
  (require 'use-package))

(defun ido-remove-tramp-from-cache nil
    "Remove any TRAMP entries from `ido-dir-file-cache'.
    This stops tramp from trying to connect to remote hosts on emacs startup,
    which can be very annoying."
    (interactive)
    (setq ido-dir-file-cache
	  (cl-remove-if
	   (lambda (x)
	     (string-match "/\\(rsh\\|ssh\\|telnet\\|su\\|sudo\\|sshx\\|krlogin\\|ksu\\|rcp\\|scp\\|rsync\\|scpx\\|fcp\\|nc\\|ftp\\|smb\\|adb\\):" (car x)))
	   ido-dir-file-cache)))
  ;; redefine `ido-kill-emacs-hook' so that cache is cleaned before being saved
  (defun ido-kill-emacs-hook ()
    (ido-remove-tramp-from-cache)
    (ido-save-history))

;; fix a bug in emacs 25 and python https://github.com/syl20bnr/spacemacs/issues/8797
(setq python-shell-completion-native-enable nil)

;;:(add-to-list 'load-path "C:\\Users\\bdulauroy\\Appdata\Roaming\\.emacs.d\\neotree-20170522.758")
;;(set-default-font "Consolas 10")
(setq-default line-spacing 1)
;;(let ((default-directory "~/.emacs.d/site-lisp/"))
;;  (normal-top-level-add-subdirs-to-load-path))
;;(package-initialize)

;; (add-to-list 'custom-theme-load-path "C:/Users/bdulauroy/locuments/Work/Apps/emacs/share/emacs/27.1/themes/color-theme-molokai-0.1")
;; (add-to-list 'custom-theme-load-path "C:/Users/bdulauroy/Documents/Work/Apps/emacs/share/emacs/27.1/themes/monokai-theme-20180402.221")
(add-to-list 'custom-theme-load-path "C:/Users/bdulauroy/Documents/Work/Apps/emacs/share/emacs/27.1/themes/dracula-theme-20201120.758")
;; (add-to-list 'custom-theme-load-path "C:/Users/bdulauroy/Documents/Work/Apps/emacs/share/emacs/27.1/themes/gruvbox-theme-20200807.855")
;; (add-to-list 'custom-theme-load-path "C:/Users/bdulauroy/Documents/Work/Apps/emacs/share/emacs/27.1/themes/spacemacs-theme-20200825.1818")
;; (add-to-list 'custom-theme-load-path "C:/Users/bdulauroy/Documents/Work/Apps/emacs/share/emacs/27.1/themes/monokai-pro-theme-20200525.1430")

;;(load-theme 'monokai-alt)
;;(load-theme 'monokai)
(load-theme 'dracula t)
;;(load-theme 'spacemacs-dark t)
;;(load-theme 'monokai-pro t)

(tool-bar-mode -1)
(scroll-bar-mode -1)
(set-default 'truncate-lines t)
(setq inhibit-startup-screen t)

(require 'evil)
(evil-mode 1)

(require 'powerline)
(powerline-default-theme)

(require 'neotree)
(global-set-key [f8] 'neotree-toggle)

(recentf-mode 1)
(setq recentf-max-menu-items 25)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)

(require 'minimap)
(global-set-key [f9] 'minimap-mode)
;; enable minimap at startup
;; (minimap-mode 1)

;;(require 'ess-site)

(require 'indent-guide)
(indent-guide-global-mode)

(require 'auto-complete)
(eval-after-load 'auto-complete '(global-auto-complete-mode 1))

(global-prettify-symbols-mode +1)

;; gnuplot-mode
;; move the files gnuplot.el to someplace in your lisp load-path or
;; use a line like
;;(setq load-path (append (list "~/.emacs.d/site-lisp/gnuplot") load-path))
;; these lines enable the use of gnuplot mode
  (autoload 'gnuplot-mode "gnuplot" "gnuplot major mode" t)
  (autoload 'gnuplot-make-buffer "gnuplot" "open a buffer in gnuplot mode" t)
;; this line automatically causes all files with the .gp extension to
;; be loaded into gnuplot mode
  (setq auto-mode-alist (append '(("\\.gp$" . gnuplot-mode)) auto-mode-alist))
  (setq auto-mode-alist (append '(("\\.plt$" . gnuplot-mode)) 
auto-mode-alist))
;; This line binds the function-7 key so that it opens a buffer into
;; gnuplot mode 
  (global-set-key [(f7)] 'gnuplot-make-buffer)
  (setq gnuplot-gnuplot-buffer "plot.plt") ; name of a new gnuplot file

;; ansys mode
(add-to-list 'auto-mode-alist '("\\.mac\\'" . ansys-mode))
(add-to-list 'auto-mode-alist '("\\.inp\\'" . ansys-mode))
(add-to-list 'auto-mode-alist '("\\.anf$" . ansys-mode))
(autoload 'ansys-mode "ansys-mode" nil t)

(add-hook 'neotree-mode-hook
            (lambda ()
              (define-key evil-normal-state-local-map (kbd "TAB") 'neotree-enter)
              (define-key evil-normal-state-local-map (kbd "SPC") 'neotree-quick-look)
              (define-key evil-normal-state-local-map (kbd "q") 'neotree-hide)
              (define-key evil-normal-state-local-map (kbd "RET") 'neotree-enter)))
(setq-default neo-show-hidden-files t)

;;(latex-preview-pane-enable)

;; rpn calculator
(require 'rpn-calc)

;; split vetically by default
;; (split-window-right)
(setq split-height-threshold nil)
(setq split-width-threshold 100)

(global-set-key (kbd "C-x C-a") 'ibuffer)

;; use spaces instead of tabs when indenting
(setq-default indent-tabs-mode nil)

(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)

;; default font (can be set in ~/.Xdefaults too)
(add-to-list 'default-frame-alist '(font . "Consolas 10"))
;;(setq default-frame-alist '((font . "DejaVu Sans Mono-8")))
;;(set-frame-font "DejaVu Sans Mono-8")
;;(set-face-attribute 'default (selected-frame) :font "DejaVu Sans Mono-8")

(require 'multiple-cursors)
(global-set-key (kbd "C-x j") 'mc/edit-lines)

;; Set Frame width/height (can be set in ~/.Xdefaults too) size depends if ETX or others
(setq default-frame-alist
      '((top . 40) (left . 225) (width . 200) (height . 55)))

(require 'sunrise-commander)
;;(require 'sunrise-x-buttons)
(require 'sunrise-x-modeline)

(add-hook 'prog-mode-hook 'linum-mode)
(add-hook 'find-file-hook 'linum-mode)

;; copy by default to the other window (dired)
(setq dired-dwim-target t)

(with-eval-after-load 'dired  (require 'dired-filetype-face))

;; dos2unix conversion

(defun dos2unix ()
  "Remove ^M at end of line in the whole buffer."
  (interactive)
  (save-match-data
    (save-excursion
      (let ((remove-count 0))
        (goto-char (point-min))
        (while (re-search-forward (concat (char-to-string 13) "$") (point-max) t)
          (setq remove-count (+ remove-count 1))
          (replace-match "" nil nil))
        (message (format "%d ^M removed from buffer." remove-count))))))
        
;; clean buffer list        
(require 'midnight)
(midnight-delay-set 'midnight-delay "4:30am")

;; vdiff compares two or three buffers
(require 'vdiff)
(define-key vdiff-mode-map (kbd "C-c") vdiff-mode-prefix-map)

(setq ls-lisp-dirs-first t)
(setq dired-listing-switches "-laGh1v")

;; org-mode
(require 'org)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)

;; kwustoss mode
(require 'kwu-mode)
(add-to-list 'auto-mode-alist '("\\TAPE60\\'" . kwu-mode))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Consolas" :foundry "outline" :slant normal :weight normal :height 98 :width normal))))
 '(vdiff-subtraction-face ((t (:inherit diff-added)))))

(setq-default line-spacing 0.1)

(setq frame-title-format '(buffer-file-name "Emacs: %b (%f)" "Emacs: %b"))

;; Custom splitting functions ;;
(defun vsplit-last-buffer ()
  (interactive)
  (split-window-vertically)
  (other-window 1 nil)
  (switch-to-next-buffer)
  )
(defun hsplit-last-buffer ()
  (interactive)
   (split-window-horizontally)
  (other-window 1 nil)
  (switch-to-next-buffer)
  )

(global-set-key (kbd "C-x 2") 'vsplit-last-buffer)
(global-set-key (kbd "C-x 3") 'hsplit-last-buffer)

(define-key isearch-mode-map (kbd "<down>") 'isearch-ring-advance)
(define-key isearch-mode-map (kbd "<up>") 'isearch-ring-retreat)
;; use evil like search
(evil-select-search-module 'evil-search-module 'evil-search)
;; insure evil is used for ibuffer
(setq evil-emacs-state-modes (delq 'ibuffer-mode evil-emacs-state-modes))


;; rsync files
(defun ora-dired-rsync (dest)
(interactive
(list (expand-file-name
(read-file-name "Rsync to:" (dired-dwim-target-directory)))))
;; store all selected files into "files" list
(let* ((files (dired-get-marked-files nil current-prefix-arg))
;; the rsync command
(tmtxt/rsync-command "rsync -arvzu --delete --progress ")
(ssh_prefix "")
)
;; add all selected file names as arguments to the rsync command
(dolist (file files)
(setq tmtxt/rsync-command
(concat tmtxt/rsync-command
(if (string-match "^/ssh:\\(.*:\\)\\(.*\\)$" file)
(progn
(if (string= ssh_prefix "")
(progn
(setq ssh_prefix (format " -e ssh \"%s%s\"" (match-string 1 file) (shell-quote-argument (match-string 2 file))))
(format " -e ssh \"%s%s\"" (match-string 1 file) (shell-quote-argument (match-string 2 file))))
;; rsync want only filenames for second source files
(format "\"%s\"" (nth 2 (s-split ":" file)))))
(shell-quote-argument file)) " ")))
(when (not (string= ssh_prefix ""))
;; Need convert command from:
;; rsync -arvzu --delete --progress -e ssh "root@10.63.200.24:/root/files" "/root/roles" /Users/bravo/temp/rsync/
;; to:
;; rsync -arvzu --delete --progress -e ssh "root@10.63.200.24:/root/files /root/roles" /Users/bravo/temp/rsync/
(setq tmtxt/rsync-command (replace-regexp-in-string "\" \"" " " tmtxt/rsync-command))
)
;; append the destination
(setq tmtxt/rsync-command
(concat tmtxt/rsync-command
(if (string-match "^/ssh:\\(.*\\)$" dest)
(format " -e ssh %s" (match-string 1 dest))
(shell-quote-argument dest))))
;; run the async shell command
(let ((default-directory (expand-file-name "~")))
(async-shell-command tmtxt/rsync-command))
(message tmtxt/rsync-command)
;; finally, switch to that window
(other-window 1)))

(define-key dired-mode-map "Y" 'ora-dired-rsync)

;; disable scrollbar also for client
(defun my/disable-scroll-bars (frame)
  (modify-frame-parameters frame
                           '((vertical-scroll-bars . nil)
                             (horizontal-scroll-bars . nil))))
(add-hook 'after-make-frame-functions 'my/disable-scroll-bars)

;; refresh file if changed
(global-auto-revert-mode t)
;; ediff control panel in same buffer
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
;; split windows horizontally
(setq ediff-split-window-function 'split-window-horizontally)
;; ignore whitespace
(setq ediff-diff-options "-w")
;; highlight in details
;;(setq-default ediff-highlight-all-diffs 't)
(setq-default ediff-forward-word-function 'forward-char)

(defun ora-ediff-hook ()
(ediff-setup-keymap)
(define-key ediff-mode-map "j" 'ediff-next-difference)
(define-key ediff-mode-map "k" 'ediff-previous-difference))
(add-hook 'ediff-mode-hook 'ora-ediff-hook)
;; restore windows when quit ediff
(winner-mode)
(add-hook 'ediff-after-quit-hook-internal 'winner-undo)

;; use e in dired to call ediff
(define-key dired-mode-map "e" 'ora-ediff-files)
(defun ora-ediff-files ()
  (interactive)
  (let ((files (dired-get-marked-files))
        (wnd (current-window-configuration)))
    (if (<= (length files) 2)
        (let ((file1 (car files))
              (file2 (if (cdr files)
                         (cadr files)
                       (read-file-name
                        "file: "
                        (dired-dwim-target-directory)))))
          (if (file-newer-than-file-p file1 file2)
              (ediff-files file2 file1)
            (ediff-files file1 file2))
          (add-hook 'ediff-after-quit-hook-internal
                    (lambda ()
                      (setq ediff-after-quit-hook-internal nil)
                      (set-window-configuration wnd))))
      (error "no more than 2 files should be marked"))))
(defun with-face (str &rest face-plist)
    (propertize str 'face face-plist))

;; eshell prompt with dracula colors
;; (defun shk-eshell-prompt ()
;; (let ((header-bg "#282a36"))
;;     (concat
;;     (with-face (concat (eshell/pwd) " ") :background header-bg)
;;     (with-face (format-time-string "(%Y-%m-%d %H:%M) " (current-time)) :background header-bg :foreground "#888")
;;     (with-face
;;     (or (ignore-errors (format "(%s)" (vc-responsible-backend default-directory))) "")
;;     :background header-bg)
;;     (with-face "\n" :background header-bg)
;;     (with-face user-login-name :foreground "#8be9fd")
;;     "@"
;;     (with-face "localhost" :foreground "#50fa7b")
;;     (if (= (user-uid) 0)
;;         (with-face " #" :foreground "#ff5555")
;;         " $")
;;     " ")))
;; (setq eshell-prompt-function 'shk-eshell-prompt)

;; allows use of name == main with python-mode

;; one prompt only for recursive
(setq dired-recursive-copies 'always)
(setq dired-recursive-deletes 'always)

(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

(require 'python)
(define-key python-mode-map (kbd "C-c C-c")
  (lambda () (interactive) (python-shell-send-buffer t)))

;; help python inferior mode to scroll
(add-hook 'inferior-python-mode-hook
          (lambda ()
            (setq comint-move-point-for-output t)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-files '("c:/Users/bdulauroy/work.org"))
 '(package-selected-packages
   '(popup-kill-ring elpy latex-preview-pane helm-swoop dired-hacks-utils evil-ediff multi use-package org-bullets projectile rainbow-delimiters emms rainbow-mode mark-multiple vdiff ## sunrise-x-tabs sunrise-x-modeline sunrise-x-buttons spacemacs-theme rpn-calc powerline openwith neotree multiple-cursors monokai-theme monokai-pro-theme minimap magit indent-guide hydra helm gruvbox-theme gnuplot-mode evil ess dracula-theme company auto-complete auctex)))

(require 'popup)
(require 'pos-tip)
(require 'popup-kill-ring)

(global-set-key "\C-cy" 'popup-kill-ring) ;
;; display time
(setq display-time-24hr-format t)
(setq display-time-format "%H:%M - %d %B %Y")
(display-time-mode 1)

;; highlight line
(when window-system (add-hook 'prog-mode-hook 'hl-line-mode))

;; don't auto backup
(setq make-backup-files nil)
(setq auto-save-default nil)

;; yes replaced by y, no by n
(defalias 'yes-or-no-p 'y-or-n-p)

;; This extension allows you to quickly mark the next occurence of a region and edit them all at once. Wow!
(require 'mark-more-like-this)
(global-set-key (kbd "C-c p") 'mark-previous-like-this)
(global-set-key (kbd "C-c q") 'mark-next-like-this)
(global-set-key (kbd "C-M-m") 'mark-more-like-this) ; like the other two, but takes an argument (negative is previous)
(global-set-key (kbd "C-*") 'mark-all-like-this)

;;Every time emacs encounters a hexadecimal code that resembles a color, it will automatically highlight it in the appropriate color.
(use-package rainbow-mode
 ;; :ensure t
  :init
    (add-hook 'prog-mode-hook 'rainbow-mode))

(use-package rainbow-delimiters
 ;; :ensure t
  :init
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package org-bullets
 ;; :ensure t
  :config
    (add-hook 'org-mode-hook (lambda () (org-bullets-mode))))

(setq scroll-conservatively 100)

;; (use-package helm
;; ;;  :ensure t
;;   :bind
;;   ("C-x C-f" . 'helm-find-files)
;;   ("C-x C-b" . 'helm-buffers-list)
;;   ("M-x" . 'helm-M-x)
;;   :config
;;   (defun daedreth/helm-hide-minibuffer ()
;;     (when (with-helm-buffer helm-echo-input-in-header-line)
;;       (let ((ov (make-overlay (point-min) (point-max) nil nil t)))
;;         (overlay-put ov 'window (selected-window))
;;         (overlay-put ov 'face
;;                      (let ((bg-color (face-background 'default nil)))
;;                        `(:background ,bg-color :foreground ,bg-color)))
;;         (setq-local cursor-type nil))))
;;   (add-hook 'helm-minibuffer-set-up-hook 'daedreth/helm-hide-minibuffer)
;;   (setq helm-autoresize-max-height 0
;;         helm-autoresize-min-height 40
;;         helm-M-x-fuzzy-match t
;;         helm-buffers-fuzzy-matching t
;;         helm-recentf-fuzzy-match t
;;         helm-semantic-fuzzy-match t
;;         helm-imenu-fuzzy-match t
;;         helm-split-window-in-side-p nil
;;         helm-move-to-line-cycle-in-source nil
;;         helm-ff-search-library-in-sexp t
;;         helm-scroll-amount 8
;;         helm-echo-input-in-header-line t)
;;   :init
;;   ;;(helm-mode 1)
;;   )

;; (require 'helm-config)    
;;  (helm-autoresize-mode 1)
;; (define-key helm-find-files-map (kbd "C-b") 'helm-find-files-up-one-level)
;; (define-key helm-find-files-map (kbd "C-f") 'helm-execute-persistent-action)

;; disable python indetn warning
(setq python-indent-guess-indent-offset-verbose nil)

;; (require 'openwith)
;; (openwith-mode t)
;; (setq openwith-associations '(
;;   ("\\.pdf\\'" "acrobat" (file))
;;   ("\\.mp3\\'" "xmms" (file))
;;   ("\\.\\(?:mpe?g\\|avi\\|wmv\\|mkv\\|mp4\\|mov\\)\\'" "vlc" (file))
;;   ("\\.\\(?:doc\\|docx\\|docm\\)\\'" "winword" (file))
;;   ("\\.\\(?:xls\\|xlsx\\|xlsm\\)\\'" "excel" (file))
;;   ("\\.\\(?:ppt\\|pptx\\|pptm\\)\\'" "powerpnt" (file))
;;   ))

(add-to-list 'exec-path "C:/Users/bdulauroy/Documents/work/Apps/emacs/libexec/emacs/27.1/x86_64-w64-mingw32")
(setenv "SHELL" "cmdproxy.exe")
(setq using-unix-filesystems t) 
(setq shell-file-name "cmdproxy") 
(setq explicit-shell-file-name "cmdproxy.exe")
;;(setq explicit-shell-file-name "bash.exe") 
(setq shell-command-switch "/C") 
(setq exec-path (append exec-path '("C:/Windows/System32/OpenSSH")))
