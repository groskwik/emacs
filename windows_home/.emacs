(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  ;; Following line is not needed if use-package.el is in ~/.emacs.d
  ;;  (add-to-list 'load-path "<path where use-package is installed>")
  (require 'use-package))

(setq byte-compile-warnings '(cl-functions))

;;https://www.reddit.com/r/emacs/comments/sv2ys8/emacs_noob_here_how_do_i_get_redo_functionality/
;;(evil-set-undo-system 'undo-redo)
;;https://github.com/syl20bnr/spacemacs/issues/14036   
;;https://gitlab.com/ideasman42/emacs-undo-fu
;;(global-undo-tree-mode)
;;(evil-set-undo-system 'undo-tree)

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

;;(with-eval-after-load 'dired (require 'dired-filetype-face))

;; Details toggling is bound to "(" in `dired-mode' by default
(setq diredp-hide-details-initially-flag nil)
;;(setq diredp-hide-details-propagate-flag nil)
(require 'dired+)
(use-package dired+
  :init)

(global-set-key [f5] 'uncomment-region)
(global-set-key [f6] 'comment-region)
(defun my-random-theme ()
  "Load a random theme from the list at startup."
  (interactive)
  (let ((themes-list '(twilight-bright
                       modus-operandi
                       modus-vivendi
                       spacemacs-light
                       spacemacs-dark
                       solarized-light
                       solarized-dark
                       monokai-pro
                       sanityinc-tomorrow-night
                       anti-zenburn
                       doom-solarized-dark
                       doom-solarized-light
                       doom-city-lights
                       doom-earl-grey
                       doom-feather-light
                       doom-flatwhite
                       doom-one-light
                       doom-tomorrow-day
                       doom-spacegrey
                       doom-nord-light
                       dracula
                       material-light
                       material
                       zenburn
                       gruvbox-light-medium
                       doom-dracula
                       doom-opera
                       doom-opera-light
                       doom-monokai-pro
                       doom-zenburn
                       moe-dark
                       moe-light
                       leuven
                       doom-one
                       gruvbox
                       )))
    (load-theme (nth (random (length themes-list)) themes-list) t)))

(my-random-theme)

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

;; code completion
;;(eval-after-load 'auto-complete '(global-auto-complete-mode 1))
(add-hook 'after-init-hook 'global-company-mode)

(global-prettify-symbols-mode +1)

;; gnuplot-mode
(autoload 'gnuplot-mode "gnuplot" "Gnuplot major mode" t)
(autoload 'gnuplot-make-buffer "gnuplot" "open a buffer in gnuplot-mode" t)
(setq auto-mode-alist (append '(("\\.gp$" . gnuplot-mode)) auto-mode-alist))
(setq auto-mode-alist (append '(("\\.plt$" . gnuplot-mode)) auto-mode-alist))
(setq auto-mode-alist (append '(("\\.dem$" . gnuplot-mode)) auto-mode-alist))

;; ;; This line binds the function-7 key so that it opens a buffer into
;; ;; gnuplot mode
;;   (global-set-key [(f7)] 'gnuplot-make-buffer)
;; (setq gnuplot-gnuplot-buffer "plot.plt") ; name of a new gnuplot file

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
(setq-default tab-width 4)
(setq indent-line-function 'insert-tab)

(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)

(require 'multiple-cursors)
(global-set-key (kbd "C-x j") 'mc/edit-lines)

;; (require 'sunrise)
;; (require 'sunrise-buttons)
;; (require 'sunrise-modeline)
;; (require 'sunrise-loop)
;; (require 'sunrise-tabs)
;; (require 'sunrise-tree)
;; (require 'sunrise-w32)

;;(add-hook 'prog-mode-hook 'linum-mode)
;;(add-hook 'find-file-hook 'linum-mode)

(global-display-line-numbers-mode 1)

(defun my-turn-off-line-numbers ()
  "Disable line numbering in the current buffer."
  (display-line-numbers-mode -1))

(setq gc-cons-threshold (* 511 1024 1024))
(setq gc-cons-percentage 0.5)
(run-with-idle-timer 5 t #'garbage-collect)
(setq garbage-collection-messages t)

(add-hook 'pdf-view-mode-hook #'my-turn-off-line-numbers)

;; copy by default to the other window (dired)
(setq dired-dwim-target t)
;; ask one time to delete / copy
(setq dired-recursive-deletes 'always)
(setq dired-recursive-copies 'always)

;; (with-eval-after-load 'dired (require 'dired-filetype-face))

(defun dos2unix-m ()
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

(defun dos2unix ()
  "Not exactly but it's easier to remember"
  (interactive)
  (set-buffer-file-coding-system 'unix 't) )

;;(defun dos2unix2-m (buffer)
;;      "Automate M-% C-q C-m RET C-q C-j RET / use if ^/M can be seen"
;;      (interactive "*b")
;;      (save-excursion
;;        (goto-char (point-min))
;;        (while (search-forward (string ?\C-m) nil t)
;;          (replace-match (string ?\C-j) nil t))))

;; clean buffer list        
(require 'midnight)
(midnight-delay-set 'midnight-delay "4:30am")

;; vdiff
(setq-default vdiff-auto-refine 'on)
(global-set-key [f12] 'vdiff-buffers)
;; vdiff compares two or three buffers
(require 'vdiff)
(define-key vdiff-mode-map (kbd "C-c") vdiff-mode-prefix-map)

(setq ls-lisp-dirs-first t)
(setq dired-listing-switches "-lAoGhv")

;; org-mode
(require 'org)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)

;; kwustoss mode
(require 'kwu-mode)
(add-to-list 'auto-mode-alist '("\\TAPE60\\'" . kwu-mode))
(require 'kwu5-mode)
(add-to-list 'auto-mode-alist '("\\tape1\\'" . kwu5-mode))


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

;; compress all marked files
;(defun my-compress-folder ()
(defun my-compress-folder ()
  "Compress the marked folder(s) using 7zip."
  (interactive)
  (let ((folders (dired-get-marked-files)))
    (if folders
        (let* ((output-name (file-name-base (car folders)))
               (quoted-folders (mapconcat (lambda (folder) (format "\"%s\"" folder)) folders " "))
               (command (format "7z a %s %s" (concat output-name ".7z") quoted-folders)))
          (message "Running 7zip command: %s" command)
          (async-shell-command command "*7zip-output*"))
      (message "No folder selected"))))
    
(define-key dired-mode-map (kbd "C-c C-z") 'my-compress-folder)

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
(setq-default ediff-highlight-all-diffs 't)
(setq-default ediff-forward-word-function 'forward-char)
;;(setq-default ediff-auto-refine 'on) ;; does not seem to change anything

;; show all regions wordwise
(eval `(defun ediff-buffers-wordwise (buffer-A buffer-B &optional startup-hooks job-name)
         ,(concat (documentation 'ediff-buffers) "\nComparison is done word-wise.")
         ,(interactive-form 'ediff-buffers)
         (setq bufA (get-buffer buffer-A)
               bufB (get-buffer buffer-B)
               job-name (or job-name 'ediff-buffers-wordwise))
         (cl-assert bufA nil
                    "Not a live buffer: %s" buffer-A)
         (cl-assert bufB nil
                    "Not a live buffer: %s" buffer-B)
         (ediff-regions-internal bufA
                                 (with-current-buffer bufA
                                   (point-min))
                                 (with-current-buffer bufA
                                   (point-max))
                                 bufB
                                 (with-current-buffer bufB
                                   (point-min))
                                 (with-current-buffer bufB
                                   (point-max))
                                 startup-hooks
                                 job-name
                                 'word-mode
                                 nil)))

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

(setq eshell-prompt-function (lambda nil
                               (concat
                                (propertize (eshell/pwd) 'face `(:foreground "#8be9fd"))
                                (propertize " $ " 'face `(:foreground "#50fa7b")))))
(setq eshell-highlight-prompt nil)
;; one prompt only for recursive
(setq dired-recursive-copies 'always)
(setq dired-recursive-deletes 'always)

;;(require 'org-bullets)
;;(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

;; python setup
;; -----------------------------------------------

;; setup for python mode
;; ********************************************
;; fix a bug in emacs 25 and python https://github.com/syl20bnr/spacemacs/issues/8797
(setq python-shell-completion-native-enable nil)

;; allows use of name == main with python-mode
(require 'python)
(define-key python-mode-map (kbd "C-c C-c")
  (lambda () (interactive) (python-shell-send-buffer t)))

;; help python inferior mode to scroll
;; (add-hook 'inferior-python-mode-hook
;;           (lambda ()
;;             (setq comint-move-point-for-output t)
;;             (setq indent-tabs-mode nil)
;;             (infer-indentation-style)))

(use-package pyvenv
  :ensure t
  :config
  (pyvenv-activate "C:/Users/bdulauroy/AppData/Roaming/Anaconda3/envs/python3.8/")
  (pyvenv-mode 1))

(defun r-activate ()
  (interactive)
  (pyvenv-activate "C:/Users/bdulauroy/AppData/Roaming/Anaconda3/envs/r_env/"))

(global-set-key (kbd "C-c C-r") 'r-activate)

;; https://www.joseferben.com/posts/switching_from_elpy_to_anaconda_mode/
;; ********************************************

;; (use-package python-black
;;   :ensure t
;;   :bind (("C-c b" . python-black-buffer)))

;; (use-package anaconda-mode
;;   :ensure t
;;   :bind (("C-c C-x" . next-error))
;;   :config
;;   (require 'pyvenv)
;;   (add-hook 'python-mode-hook 'anaconda-mode))

;; (use-package company-anaconda
;;   :ensure t
;;   :config
;;   (eval-after-load "company"
;;    '(add-to-list 'company-backends '(company-anaconda :with company-capf))))

;; (use-package highlight-indent-guides
;;   :ensure t
;;   :config
;;   (add-hook 'python-mode-hook 'highlight-indent-guides-mode)
;;   (setq highlight-indent-guides-method 'character))

;; (use-package flycheck
;;   :ensure t
;;   :init (global-flycheck-mode))
;; *******************

;; elpy python ide
;; (use-package elpy
;;   :ensure t
;;   :init
;;   (elpy-enable))

(add-to-list 'process-coding-system-alist '("python" . (utf-8 . utf-8)))
(setq elpy-rpc-python-command "python")
;; -----------------------------------------------

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(bmkp-last-as-first-bookmark-file "c:/Users/bdulauroy/AppData/Roaming/.emacs.d/bookmarks")
 '(custom-safe-themes
   '("78e6be576f4a526d212d5f9a8798e5706990216e9be10174e3f3b015b8662e27" default))
 '(org-agenda-files '("c:/Users/bdulauroy/work.org"))
 '(package-selected-packages
   '(ivy org-bullets csv-mode anaconda-mode color-theme-sanityinc-tomorrow moe-theme modus-themes doom-themes solarized-theme leuven-theme anti-zenburn-theme twilight-bright-theme zenburn-theme melancholy-theme which-key keepass-mode jupyter gnuplot-mode dir-treeview beacon elpygen modus-operandi-theme highlight pdf-tools focus dired-rainbow material-theme conda pyvenv-auto disk-usage gnuplot dired-rsync popup-kill-ring elpy latex-preview-pane helm-swoop dired-hacks-utils evil-ediff multi use-package projectile rainbow-delimiters rainbow-mode vdiff ## sunrise-x-tabs sunrise-x-modeline sunrise-x-buttons spacemacs-theme rpn-calc powerline openwith neotree multiple-cursors monokai-pro-theme minimap magit indent-guide hydra helm gruvbox-theme evil ess dracula-theme company auto-complete auctex)))

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
;; (require 'mark-more-like-this)
;; (global-set-key (kbd "C-c p") 'mark-previous-like-this)
;; (global-set-key (kbd "C-c q") 'mark-next-like-this)
;; (global-set-key (kbd "C-M-m") 'mark-more-like-this) ; like the other two, but takes an argument (negative is previous)
;; (global-set-key (kbd "C-*") 'mark-all-like-this)

;;Every time emacs encounters a hexadecimal code that resembles a color, it will automatically highlight it in the appropriate color.
(use-package rainbow-mode
  ;; :ensure t
  :init
  (add-hook 'python-mode 'rainbow-mode))

(use-package rainbow-delimiters
  ;; :ensure t
  :init
  (add-hook 'python-mode #'rainbow-delimiters-mode))

;;(use-package org-bullets
  ;; :ensure t
;;  :config
;; (add-hook 'org-mode-hook (lambda () (org-bullets-mode))))

(setq scroll-conservatively 100)


;; disable python indetn warning
(setq python-indent-guess-indent-offset-verbose nil)

;; from chatGPT
;; yes, you can disable the prompt asking whether to use a new buffer or not when running a new async command with async-shell-command in Emacs. You can achieve this by customizing the variable async-shell-command-buffer to always use a new buffer.
(setq async-shell-command-buffer 'new-buffer)

(add-to-list 'display-buffer-alist '("*Async Shell Command*" display-buffer-no-window (nil)))
(defun async-shell-command-no-window
    (command)
  (interactive)
  (let
      ((display-buffer-alist
        (list
         (cons
          "\\*Async Shell Command\\*.*"
          (cons #'display-buffer-no-window nil)))))
    (async-shell-command
     command)))

;; bug in windows with plink but works with scp and ftp (slower for both with async)
(autoload 'dired-async-mode "dired-async.el" nil t)
(dired-async-mode 0)
;; (dired-async-mode 1)
(global-set-key (kbd "C-x C-y") 'dired-async-do-copy)

(defun shell-command-on-buffer ()
  "Asks for a command and executes it in inferior shell with current buffer
as input."
  (interactive)
  (shell-command-on-region
   (point-min) (point-max)
   (read-shell-command "Shell command on buffer: ")))
(global-set-key (kbd "C-x C-t") 'shell-command-on-buffer)

(defun cpm/show-and-copy-buffer-filename ()
  "Show the full path to the current file in the minibuffer and copy to clipboard."
  (interactive)
  (let ((file-name (buffer-file-name)))
    (if file-name
        (progn
          (message file-name)
          (kill-new file-name))
      (error "Buffer not visiting a file"))))

;;(evil-set-initial-state 'ibuffer-mode 'normal)
;;(evil-set-initial-state 'bookmark-bmenu-mode 'normal)
(evil-set-initial-state 'sunrise-mode 'emacs)
(evil-set-initial-state 'image-mode 'emacs)

;; windmove. With its default keybindings, it allows switching
;; to the window next to the currently active one.
;; you can then switch to neighbouring windows using the following keys
;; (where the arrow used intuitively defines the direction in which you move):
;; S-<left>, S-<right>, S-<up>, S-<down>.
(windmove-default-keybindings)

(setq use-package-compute-statistics t)

(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(add-to-list 'exec-path "c:/PortableApps/ImageMagick")
(add-to-list 'exec-path "C:/Program Files (x86)/Gnuplot/bin")
(add-to-list 'exec-path "C:/Program Files/Gnuplot/bin")
;;(add-to-list 'exec-path "C:/Users/benoi/anaconda3/Scripts")
;; if not set in the environment variable on the system
;; (setq shell-file-name explicit-shell-file-name)
;; (add-to-list 'exec-path "C:/cygwin64/bin")
(add-to-list 'exec-path "c:/PortableApps/emacs/libexec/emacs/29.4/x86_64-w64-mingw32")
;;(add-to-list 'exec-path "C:/Apps/bin")
(setenv "SHELL" "cmdproxy.exe")
(setq using-unix-filesystems t)
;(setq shell-file-name "cmdproxy")
;(setq explicit-shell-file-name "cmdproxy.exe")
;;(setq explicit-shell-file-name "bash.exe")
(setq shell-command-switch "/C")
(setq exec-path (append exec-path '("C:/Windows/System32/OpenSSH")))

(setq shell-file-name "C:/PortableApps/emacs/libexec/emacs/29.4/x86_64-w64-mingw32/cmdproxy.exe")
(setq explicit-shell-file-name shell-file-name)
(setq explicit-sh-args '("--noediting" "--login" "-i"))

(defun paste-windows-path (pth) (interactive "*sWindows path:") (insert (replace-regexp-in-string "\\\\" "\\\\\\\\" pth)))

(load "auctex.el" nil t t)

(use-package helm
  :ensure t
  :bind
  ("C-x C-f" . 'helm-find-files)
  ("C-x C-b" . 'helm-buffers-list)
  ("M-x" . 'helm-M-x)
  :config
  (defun daedreth/helm-hide-minibuffer ()
    (when (with-helm-buffer helm-echo-input-in-header-line)
      (let ((ov (make-overlay (point-min) (point-max) nil nil t)))
        (overlay-put ov 'window (selected-window))
        (overlay-put ov 'face
                     (let ((bg-color (face-background 'default nil)))
                       `(:background ,bg-color :foreground ,bg-color)))
        (setq-local cursor-type nil))))
  (add-hook 'helm-minibuffer-set-up-hook 'daedreth/helm-hide-minibuffer)
  (setq helm-autoresize-max-height 0
        helm-autoresize-min-height 40
        helm-M-x-fuzzy-match t
        helm-buffers-fuzzy-matching t
        helm-recentf-fuzzy-match t
        helm-semantic-fuzzy-match t
        helm-imenu-fuzzy-match t
        helm-split-window-in-side-p nil
        helm-move-to-line-cycle-in-source nil
        helm-ff-search-library-in-sexp t
        helm-scroll-amount 8
        helm-echo-input-in-header-line t)
  :init
  ;;(helm-mode 1)
  )

(helm-autoresize-mode 1)

;; https://emacs.stackexchange.com/questions/36133/split-helm-window-in-different-directions
(defun my-helm-buffers-list ()
  (interactive)
  (let ((helm-split-window-default-side 'left))
    (helm-buffers-list)))
(global-set-key (kbd "C-x C-b") 'my-helm-buffers-list)

;;Every time emacs encounters a hexadecimal code that resembles a color, it will automatically highlight it in the appropriate color.
(use-package rainbow-mode
  :ensure t
  :init
  (add-hook 'prog-mode-hook 'rainbow-mode))

(use-package rainbow-delimiters
  :ensure t
  :init
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

;; (use-package dmenu
;;   :ensure t
;;   :bind
;;  ("S-SPC" . 'dmenu))

(setq scroll-conservatively 100)

(use-package dired-rsync
  :config
  (bind-key "C-c C-r" 'dired-rsync dired-mode-map))

(setq w32-recognize-altgr nil)

;;(global-set-key (kbd "C-' a") 'emacspeak-wizards-execute-asynchronously)

;; accelerate tramp
(setq remote-file-name-inhibit-cache nil)
(setq vc-ignore-dir-regexp
      (format "%s\\|%s"
              vc-ignore-dir-regexp
              tramp-file-name-regexp))
(setq tramp-verbose 1)

(require 'disk-usage)

;;(setq ange-ftp-ftp-program-name "C:/PortableApps/emacs/libexec/emacs/27.1/x86_64-w64-mingw32/ftp.exe")
;;(setq ange-ftp-ftp-program-name "C:/Windows/System32/ftp.exe")
(add-hook 'dired-after-readin-hook 'hl-line-mode)

;; check marck color in dired+
(set-face-attribute 'diredp-flag-mark nil 
                    :foreground "#f1fa8c")
(require 'dired-sort-menu)
(add-hook 'dired-load-hook
          (lambda () (require 'dired-sort-menu)))

;;https://github.com/Fuco1/dired-hacks/issues/148
(setq dired-hacks-datetime-regexp "\\(?:[0-9][0-9]-[0-9][0-9] [0-9][0-9]:[0-9][0-9]\\|[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\)")

(require 'dired-rainbow)

(use-package dired-rainbow
  :config
  (progn
    (dired-rainbow-define-chmod directory "#6cb2eb" "d.*")
    (dired-rainbow-define html "#eb5286" ("css" "less" "sass" "scss" "htm" "html" "jhtm" "mht" "eml" "mustache" "xhtml"))
    (dired-rainbow-define xml "#f2d024" ("xml" "xsd" "xsl" "xslt" "wsdl" "bib" "json" "msg" "pgn" "rss" "yaml" "yml" "rdata"))
    (dired-rainbow-define document "#9561e2" ("docm" "doc" "docx" "odb" "odt" "pdb" "pdf" "ps" "rtf" "djvu" "epub" "odp" "ppt" "pptx"))
    ;; for dark color scheme
    ;; (dired-rainbow-define markdown "#ffed4a" ("org" "etx" "info" "markdown" "md" "mkd" "nfo" "pod" "rst" "tex" "textfile" "txt"))
    ;; for light color scheme
    (dired-rainbow-define markdown "#2f4a31" ("org" "etx" "info" "markdown" "md" "mkd" "nfo" "pod" "rst" "tex" "textfile" "txt"))
    (dired-rainbow-define database "#6574cd" ("xlsx" "xls" "csv" "accdb" "db" "mdb" "sqlite" "nc"))
    (dired-rainbow-define media "#de751f" ("mp3" "mp4" "MP3" "MP4" "avi" "mpeg" "mpg" "flv" "ogg" "mov" "mid" "midi" "wav" "aiff" "flac"))
    (dired-rainbow-define image "#f66d9b" ("tiff" "tif" "cdr" "gif" "ico" "jpeg" "jpg" "png" "psd" "eps" "svg" "JPG"))
    (dired-rainbow-define log "#c17d11" ("log"))
    (dired-rainbow-define shell "#f6993f" ("awk" "bash" "bat" "sed" "sh" "zsh" "vim"))
    (dired-rainbow-define interpreted "#38c172" ("py" "ipynb" "rb" "pl" "t" "msql" "mysql" "pgsql" "sql" "r" "clj" "cljs" "scala" "js"))
    (dired-rainbow-define compiled "#4dc0b5" ("asm" "cl" "lisp" "el" "c" "h" "c++" "h++" "hpp" "hxx" "m" "cc" "cs" "cp" "cpp" "go" "f" "for" "ftn" "f90" "f95" "f03" "f08" "s" "rs" "hi" "hs" "pyc" ".java"))
    (dired-rainbow-define executable "#8cc4ff" ("exe" "msi"))
    (dired-rainbow-define compressed "#51d88a" ("7z" "zip" "bz2" "tgz" "txz" "gz" "xz" "z" "Z" "jar" "war" "ear" "rar" "sar" "xpi" "apk" "xz" "tar"))
    (dired-rainbow-define packaged "#faad63" ("deb" "rpm" "apk" "jad" "jar" "cab" "pak" "pk3" "vdf" "vpk" "bsp"))
    (dired-rainbow-define encrypted "#ffed4a" ("gpg" "pgp" "asc" "bfe" "enc" "signature" "sig" "p12" "pem"))
    (dired-rainbow-define fonts "#6cb2eb" ("afm" "fon" "fnt" "pfb" "pfm" "ttf" "otf"))
    (dired-rainbow-define partition "#e3342f" ("dmg" "iso" "bin" "nrg" "qcow" "toast" "vcd" "vmdk" "bak"))
    (dired-rainbow-define vc "#0074d9" ("git" "gitignore" "gitattributes" "gitmodules"))
    (dired-rainbow-define kwubin "#f66d9b" ("TAPE4" "tape8" "tape4" "TAPE10" "TAPE20"))
    (dired-rainbow-define kwutext "#0074d9" ("tape2" "tape61" "tape1" "TAPE60"))
    (dired-rainbow-define kwuhide "#2f4a31" ("TAPE5" "TAPE7" "TAPE82"))
    (dired-rainbow-define-chmod executable-unix "#38c172" "-.*x.*")
    )) 

;;(require 'auto-package-update)
;;(auto-package-update-maybe)

(require 'ace-window)
(global-set-key (kbd "M-o") 'ace-window)

(require 'beacon)
(beacon-mode 1)

(defun kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (mapc 'kill-buffer 
        (delq (current-buffer) 
              (remove-if-not 'buffer-file-name (buffer-list)))))

(set-face-foreground 'vertical-border "grey")

;; windows specific

;; robocopy files
;; (defun my-dired-copy-to-other-window ()
;;   "Copy the content of the current directory to the other dired window using robocopy with async."
;;   (interactive)
;;   (let* ((source (file-truename default-directory))
;;          (destination (if (eq major-mode 'dired-mode)
;;                           (file-truename (dired-dwim-target-directory))
;;                         (read-directory-name "Target directory: ")))
;;          (robocopy-options (read-string "Robocopy options (e.g., /e /w:0 /r:0): " "/e /purge /w:0 /r:0"))
;;          (command (format "robocopy \"%s\" \"%s\" %s" source destination robocopy-options))
;;          (confirmation (y-or-n-p (format "Execute the following command?\n%s" command))))
;;     (when confirmation
;;       (async-shell-command command "*Robocopy Output*"))))
;; (defun my-dired-copy-to-other-window ()
;;   "Copy the content of the current directory to the other dired window using robocopy with async."
;;   (interactive)
;;   (let* ((source (file-truename default-directory))
;;          (destination (if (eq major-mode 'dired-mode)
;;                           (file-truename (dired-dwim-target-directory))
;;                         (read-directory-name "Target directory: ")))
;;          (robocopy-options (read-string "Robocopy options (e.g., /e /w:0 /r:0): " "/e /purge /w:0 /r:0"))
;;          (command (format "robocopy \"%s\" \"%s\" %s" source destination robocopy-options))
;;          ;; Generate a unique buffer name
;;          (buffer-name (generate-new-buffer-name
;;                        (format "*Robocopy from %s to %s*"
;;                                (file-name-nondirectory (directory-file-name source))
;;                                (file-name-nondirectory (directory-file-name destination)))))
;;          (confirmation (y-or-n-p (format "Execute the following command?\n%s" command))))
;;     (when confirmation
;;       ;; Execute the command with the unique buffer name
;;       (async-shell-command command buffer-name)
;;       ;; Optionally, you might want to automatically display this buffer
;;       (display-buffer buffer-name))))

(defun my-dired-copy-to-other-window ()
  "Copy the content of the current directory to the other dired window using robocopy with async."
  (interactive)
  (let* ((source (file-truename default-directory))
         (destination (if (eq major-mode 'dired-mode)
                          (file-truename (dired-dwim-target-directory))
                        (read-directory-name "Target directory: ")))
         (robocopy-options (read-string "Robocopy options (e.g., /e /w:0 /r:0): " "/e /purge /w:0 /r:0"))
         (command (format "robocopy \"%s\" \"%s\" %s" source destination robocopy-options))
         ;; Generate a unique buffer name
         (buffer-name (generate-new-buffer-name
                       (format "*Robocopy from %s to %s*"
                               (file-name-nondirectory (directory-file-name source))
                               (file-name-nondirectory (directory-file-name destination)))))
         (confirmation (y-or-n-p (format "Execute the following command?\n%s" command))))
    (when confirmation
      ;; Print debug information
      (message "Command: %s" command)
      (message "Buffer name: %s" buffer-name)
      (message "Shell file name: %s" shell-file-name)
      ;; Execute the command with the unique buffer name
      (condition-case err
          (async-shell-command command buffer-name)
        (error (message "Error: %s" err)))
      ;; Optionally, you might want to automatically display this buffer
      (display-buffer buffer-name))))

;; Bind the function to the key T
(define-key dired-mode-map "T" 'my-dired-copy-to-other-window)


;; ;; Bind the function to the key T
;; (define-key dired-mode-map "T" 'my-dired-copy-to-other-window)

;; robocopy files old code derived from rsync code (kept for legacy)
;; (defun ora-dired-robocopy (dest)
;;   (interactive
;;    (list (expand-file-name
;;           (read-file-name "Robocopy /e to: " (dired-dwim-target-directory)))))
;;   ;; store all selected files into "files" list
;;   (let* ((files (dired-get-marked-files nil current-prefix-arg))
;;          ;; the robocopy command
;;          (tmtxt/rsync-command "robocopy /e /log:backup_log.txt ")
;;          (ssh_prefix "")
;;          )
;;     ;; add all selected file names as arguments to the rsync command
;;     (dolist (file files)
;;       (setq tmtxt/rsync-command
;;             (concat tmtxt/rsync-command
;;                     (if (string-match "^/ssh:\\(.*:\\)\\(.*\\)$" file)
;;                         (progn
;;                           (if (string= ssh_prefix "")
;;                               (progn
;;                                 (setq ssh_prefix (format " -e ssh \"%s%s\"" (match-string 1 file) (shell-quote-argument (match-string 2 file))))
;;                                 (format " -e ssh \"%s%s\"" (match-string 1 file) (shell-quote-argument (match-string 2 file))))
;;                             ;; only filenames for second source files
;;                             (format "\"%s\"" (nth 2 (s-split ":" file)))))
;;                       (shell-quote-argument file)) " ")))
;;     (when (not (string= ssh_prefix ""))
;;       (setq tmtxt/rsync-command (replace-regexp-in-string "\" \"" " " tmtxt/rsync-command))
;;       )
;;     ;; append the destination
;;     (setq tmtxt/rsync-command
;;           (concat tmtxt/rsync-command
;;                   (if (string-match "^/ssh:\\(.*\\)$" dest)
;;                       (format " -e ssh %s" (match-string 1 dest))
;;                     (shell-quote-argument dest))))
;;     ;; run the async shell command
;;     (let ((default-directory (expand-file-name "~")))
;;       (async-shell-command tmtxt/rsync-command))
;;     (message tmtxt/rsync-command)
;;     ;; finally, switch to that window
;;     (other-window 1)))
    
;;(define-key dired-mode-map "Y" 'ora-dired-robocopy)

(add-to-list 'default-frame-alist '(font . "Consolas 10"))

(setq dired-guess-shell-alist-user
      '(("\\.pdf\\'" "inkscape ? &" "gsview ? &")
        ("\\.emf\\'" "inkscape ? &")
        ("\\.mp3\\'" "vlc ? &")
        ("\\.mp4\\'" "vlc ? &")
        ("\\.png\\'" "C:/Users/bdulauroy/Documents/work/Apps/Pixia/pixia.exe ? &" "gimp ? &")
        ("\\.jpe?g\\'" "C:/Users/bdulauroy/Documents/work/Apps/Pixia/pixia.exe ? &" "gimp ? &")
        ("\\.plt\\'" "gnuplot ? &")
        ("\\.dem\\'" "gnuplot ? &")
        ("\\.gp\\'" "gnuplot ? &")
        ("\\.svg\\'" "inkscape ? &")
        ("\\.eps\\'" "inkscape ? &")
        ("\\.ps\\'" "inkscape ? &")
        ("\\.zip\\'" "C:/Program Files/7-zip/7zfm.exe ? &")))

(add-to-list 'Info-default-directory-list "c:/Program Files (X86)/gnuplot/bin/gnuplot.exe")

;; Set Frame width/height (can be set in ~/.Xdefaults too) size depends if ETX or others
(setq default-frame-alist
      '((top . 40) (left . 225) (width . 200) (height . 55)))

;; (defun copy-windows-path-to-clipboard ()
;;   "Copy the current file's path in Windows format to the clipboard."
;;   (interactive)
;;   (kill-new (replace-regexp-in-string "/" "\\\\" (dired-file-name-at-point)))
;;   (message "Windows path copied to clipboard"))

(defun copy-windows-path-to-clipboard ()
  "Copy the current file's path in Windows format to the clipboard."
  (interactive)
  (let* ((user-name (user-login-name))
         (user-home (concat "c:/Users/" user-name "/AppData/Roaming"))
         (file-path (replace-regexp-in-string
                     "~" user-home (dired-file-name-at-point))))
    (kill-new (replace-regexp-in-string "/" "\\\\" file-path))
    (message "Windows path copied to clipboard")))

(define-key dired-mode-map (kbd "0 w") 'copy-windows-path-to-clipboard)

(defun copy-directory-path ()
  "Copy the path of the current directory in Dired mode to the clipboard."
  (interactive)
  (let ((dir (dired-current-directory)))
    (kill-new (expand-file-name dir))
    (message "Directory path copied to clipboard: %s" dir)))

(define-key dired-mode-map (kbd "0 q") 'copy-directory-path)

(defun copy-remote-to-local-lftp ()
  (interactive)
  (let* ((remote-file (dired-get-filename))
         (remote-path (tramp-file-name-localname (tramp-dissect-file-name remote-file)))
         (local-buffer (window-buffer (next-window)))
         (local-dir (file-name-as-directory (with-current-buffer local-buffer default-directory)))
         (local-file (concat local-dir (file-name-nondirectory remote-path)))
         (buffer-name (generate-new-buffer-name
                       (format "*Copy Remote to Local: %s to %s*"
                               (file-name-nondirectory remote-path)
                               (file-name-nondirectory local-file))))
         (lftp-command (format "lftp -e \"set ftps:initial-prot ''; set ftp:ssl-force true; set ftp:ssl-protect-data true; set ftp:passive-mode yes; set ssl:verify-certificate no; open ftp://groskwik@pan.usbx.me; get %s -o %s; bye\""
                               remote-path local-file)))

    (select-window (next-window)) ; Switch to the Tramp window
    (async-shell-command lftp-command buffer-name)  ; Execute lftp asynchronously
    (select-window (get-buffer-window local-buffer))  ; Switch back to the local window
    (pop-to-buffer buffer-name)))  ; Display the new buffer

(global-set-key (kbd "C-x c") 'copy-remote-to-local-lftp)

;; insure the shell is opened in the current frame
(setq shell-pop-full-span t)

(defun my-sh-send-command (command)
  "Send command to the current shell process.
    See URL `https://stackoverflow.com/a/7053298/5065796' 
    
  Creates a new shell process if none exists."
    
  (let ((proc (get-process "shell"))
        pbuf)
    (unless proc
      (let ((currbuff (current-buffer)))
        (shell)
        (switch-to-buffer currbuff)
        (setq proc (get-process "shell"))))
    (setq pbuff (process-buffer proc))
    (setq command-and-go (concat command "\n"))
    (with-current-buffer pbuff
      (goto-char (process-mark proc))
      (insert command-and-go)
      (move-marker (process-mark proc) (point)))
    (process-send-string proc command-and-go)
    (switch-to-buffer pbuff))) ; Replace the current buffer with the shell buffer
    
(defun conda ()
      (interactive)
      (my-sh-send-command (concat "conda_activate.bat" )))
    
(global-set-key (kbd "<S-f7>") 'conda)

;; Open Dired with a specific directory at startup
(defun my/open-dired-at-startup ()
  (dired "c:/Users/bdulauroy/Downloads")) ; Replace "~/path/to/directory" with your desired directory

(add-hook 'emacs-startup-hook #'my/open-dired-at-startup)

(defun create-script-with-shebang (file-name)
  "Create a new script file with a specified shebang line and open in Emacs."
  (interactive "sEnter script name: ")
  (let* ((file-path (expand-file-name file-name))
         (language (completing-read "Choose shebang (Bash [b] | Python [p] | Gnuplot [g]): "
                                    '(("b" "Bash") ("p" "Python"))))
         (shebang-line (cond ((string= language "b") "#!/bin/bash")
                             ((string= language "p") "#!/usr/bin/env python")
                             ((string= language "g") "#!/usr/bin/env gnuplot")
                             (t ""))))
    (if (file-exists-p file-path)
        (message "File already exists!")
      (with-temp-file file-path
        (insert shebang-line "\n"))
      (find-file file-path)
      (message "Script %s created with shebang line: %s" file-name shebang-line))))

(global-set-key (kbd "C-c j") 'create-script-with-shebang)

(add-hook 'doc-view-mode-hook (lambda () (doc-view-fit-height-to-window)))

(defun eplot (files gnuplot-filename xlabel ylabel title y-column skip-lines)
  "Run a Gnuplot script with specified parameters for multiple files."
  (interactive
   (list (dired-get-marked-files) ; Get a list of marked files in Dired
         (file-name-nondirectory (read-file-name "Enter Gnuplot filename (without extension, press Enter for default 'plot'): " nil nil nil ""))
         (read-string "Enter xlabel: ")
         (read-string "Enter ylabel: ")
         (read-string "Enter title: ")
         (read-string "Enter Y column (press Enter for default [2]): ")
         (read-string "Enter number of lines to skip (press Enter for default [0]): ")))

  (let* ((ch (mapconcat (lambda (file) (file-name-nondirectory file)) files " "))
         (labels (mapconcat (lambda (file)
                              (format "'%s'" (replace-regexp-in-string "_" " " (file-name-base file))))
                            files " "))
         (datafile-sep (read-string "Enter datafile separator (press Enter for default whitespace): "))
         (xrange-min (read-string "Enter xrange min (press Enter for automatic scale): "))
         (xrange-max (read-string "Enter xrange max (press Enter for automatic scale): "))
         (yrange-min (read-string "Enter yrange min (press Enter for automatic scale): "))
         (yrange-max (read-string "Enter yrange max (press Enter for automatic scale): "))
         (gnuplot-filename (if (equal gnuplot-filename "") "plot" gnuplot-filename)) ; Use default 'plot' if blank
         (script-content
          (format "#!/usr/bin/env gnuplot
set datafile separator %s
set style line 1 lc rgb '#377eb8' pt 7 ps 1 lt 1 lw 3
set style line 2 lc rgb '#e41a1c' pt 7 ps 1 lt 1 lw 3
set style line 11 lc rgb '#808080' lt 1
set border 3 back ls 11
set tics nomirror

set style line 12 lc rgb '#808080' lt 0 lw 1
set key top right font \",13\" tc rgb '#606060'
set xtics font \",13\" 
set ytics font \",13\"

set style line 13 lw 2 lt 1 pt 7 lc rgb '#0072bd' # blue
set style line 14 lw 2 lt 1 pt 7 lc rgb '#d95319' # orange
set style line 15 lw 2 lt 1 pt 7 lc rgb '#edb120' # yellow
set style line 16 lw 2 lt 1 pt 7 lc rgb '#7e2f8e' # purple
set style line 17 lw 2 lt 1 pt 7 lc rgb '#77ac30' # green
set style line 18 lw 2 lt 1 pt 7 lc rgb '#4dbeee' # light-blue
set style line 19 lw 2 lt 1 pt 7 lc rgb '#c06c84' # pink
set style line 20 lw 2 lt 1 pt 7 lc rgb '#7f4e34' # brown
set style line 21 lw 2 lt 1 pt 7 lc rgb '#606060' # grey

set xlabel '%s' font \",13\" tc rgb '#606060'
set ylabel '%s' font \",13\" tc rgb '#606060'
set title '%s' font ',13' tc rgb '#606060'

ch = \"%s\"
labels = \"%s\"

set xrange [%s:%s]
set yrange [%s:%s]

plot for [j=1:words(ch)] sprintf(\"%%s\", word(ch, j)) using 1:%s every ::%s w l ls j+12 lw 2 title word(labels,j)
pause 10

set term svg
set output '%s.svg'
replot
    \n"
                  (cond
                  ((string= datafile-sep "") "whitespace")
                  ((string= datafile-sep ",") "','")
                  ((string= datafile-sep ";") "';'")
                  (t datafile-sep)) ; Default case, returns datafile-sep as is if no other conditions match
                  xlabel ylabel title ch labels
                  (if (string= xrange-min "") "*" xrange-min)
                  (if (string= xrange-max "") "*" xrange-max)
                  (if (string= yrange-min "") "*" yrange-min)
                  (if (string= yrange-max "") "*" yrange-max)
                  (if (string= y-column "") "2" y-column)
                  (if (string= skip-lines "") "0" skip-lines)
                  gnuplot-filename))
         (script-file (concat (file-name-directory (car files)) (concat gnuplot-filename ".plt")))
         (default-directory (file-name-directory (car files)))) ; Set the working directory

    (with-temp-file script-file
      (insert script-content))

    (message "Running Gnuplot...")
    (shell-command (format "gnuplot %s &" script-file))
    (run-at-time "3 sec" nil (lambda () (message "The Gnuplot window will close in 10 seconds...")))

    (revert-buffer)  ; Refresh Dired buffe
    (message "Gnuplot process initiated.")
    ))

(global-set-key (kbd "C-c C-g") 'eplot)

(evil-define-key 'normal dired-mode-map
  "gg" 'beginning-of-buffer
  "G" 'end-of-buffer
  "gr" 'revert-buffer)

(defun close-all-buffers ()
  (interactive)
  (mapc 'kill-buffer (buffer-list)))

(global-set-key (kbd "C-x C-k") 'close-all-buffers)

(setq visible-bell t)

;; uodate T480
    
(defun dired-open-with-choice ()
  "Prompt for a program to open the current file in `dired` with a default choice and display all options."
  (interactive)
  (let* ((file (dired-get-file-for-visit))
         (choices (cond
                   ((string-match "\\.pdf\\'" file) '("C:/PortableApps/SumatraPDF/SumatraPDF.exe"
                                                      "C:/PortableApps/Inkscape/bin/inkscape.exe"
                                                      "C:/Program Files/Tracker Software/PDF Editor/PDFXEdit.exe"
                                                      "C:/Program Files/Adobe/Acrobat DC/Acrobat/Acrobat.exe"))
                   ((string-match "\\.mp4\\'" file) '("C:/Program Files/VideoLAN/VLC/vlc.exe"))
                   ((string-match "\\.png\\'" file) '("C:/Program Files/GIMP 2/bin/gimp-2.10.exe"
                                                      "C:/Program Files/Inkscape/inkscape.exe"))
                   ((string-match "\\.jpg\\'" file) '("C:/Program Files/GIMP 2/bin/gimp-2.10.exe"))
                   ((string-match "\\.svg\\'" file) '("C:/Program Files/Inkscape/inkscape.exe"))
                   ((string-match "\\.plt\\'" file) '("c:/PortableApps/gnuplot/bin/gnuplot.exe"))
                   ((string-match "\\.zip\\'" file) '("C:/Program Files/7-Zip/7zFM.exe"))
                   (t nil)))
         (default-program (car choices)) ; Default to the first program
         (program (if choices
                      (helm-comp-read (format "Open with (default: %s): " default-program)
                                      choices
                                      :default default-program
                                      :must-match t)
                    (error "No associated programs for this file type"))))
    (when program
      (start-process "dired-open" nil program file))))

(define-key dired-mode-map (kbd "C-c o") 'dired-open-with-choice)

(defun my/display-disk-space ()
  "Display free disk space using PowerShell."
  (interactive)
  (let ((output (shell-command-to-string "powershell -command \"Get-PSDrive -PSProvider FileSystem\"")))
    (with-output-to-temp-buffer "*Disk Space*"
      (princ output))))

(global-set-key (kbd "C-c d") 'my/display-disk-space)

;; Preload helm at startup
(use-package helm
  :ensure t
  :config
  (run-with-idle-timer 1 nil (lambda () (helm-mode 1))))
