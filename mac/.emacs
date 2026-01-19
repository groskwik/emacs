(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(add-to-list 'exec-path "/usr/local/bin/")

;;(package-initialize)
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(load-theme 'dracula t)
;; start of my config

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
;;(set-default-font "DejaVu Sans Mono for Powerline 9")
(setq-default line-spacing 1)
(let ((default-directory "~/.emacs.d/site-lisp/"))
    (normal-top-level-add-subdirs-to-load-path))
(package-initialize)
;;(add-to-list 'custom-theme-lod-path "/urrnfs01/bdulauro/.emacs.d/theme/monokai-alt-theme-20170630.1348")
;;(add-to-list 'custom-theme-load-path "/urrnfs01/bdulauro/.emacs.d/theme/color-theme-molokai-0.1")
;;(add-to-list 'custom-theme-load-path "/urrnfs01/bdulauro/.emacs.d/theme/monokai-theme-20180402.221")
;;(add-to-list 'custom-theme-load-path "/urrnfs01/bdulauro/.emacs.d/theme/dracula-theme-20201120.758")
;;(add-to-list 'custom-theme-load-path "/urrnfs01/bdulauro/.emacs.d/theme/gruvbox-theme-20200807.855")
;;(add-to-list 'custom-theme-load-path "/urrnfs01/bdulauro/.emacs.d/theme/monokai-pro-theme-20200525.1430")
;;(add-to-list 'custom-theme-load-path "/urrnfs01/bdulauro/.emacs.d/theme/spacemacs-theme-20200825.1818")

;;(load-theme 'monokai-alt)
;;(load-theme 'monokai)
(load-theme 'dracula t)
;;(load-theme 'spacemacs-dark t)
;;(load-theme 'monokai-pro t)

(tool-bar-mode -1)
;;(scroll-bar-mode -1)
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

(require 'ess-site)

(require 'indent-guide)
(indent-guide-global-mode)

(require 'auto-complete)
;;(ac-config-default)
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
;;(add-to-list 'default-frame-alist
;;'(font . "DejaVu Sans Mono for Powerline-9"))
;;'(font . "Roboto Mono for Powerline-9"))
;;(setq default-frame-alist '((font . "DejaVu Sans Mono for Powerline-9")))

(require 'multiple-cursors)
(global-set-key (kbd "C-x j") 'mc/edit-lines)

;; Set Frame width/height (can be set in ~/.Xdefaults too) size depends if ETX or others
(setq default-frame-alist
      '((top . 25) (left . 200) (width . 160) (height . 50)))

;;(require 'sunrise-commander)
;;(require 'sunrise-x-buttons)
;;(require 'sunrise-x-modeline)

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
(setq dired-listing-switches "-laGh")

;; org-mode
(require 'org)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)

;; kwustoss mode
(require 'kwu-mode)
(add-to-list 'auto-mode-alist '("\\TAPE60\\'" . kwu-mode))

;;(require 'helm)

;; disable scrollbar also for client
(defun my/disable-scroll-bars (frame)
  (modify-frame-parameters frame
                           '((vertical-scroll-bars . nil)
                             (horizontal-scroll-bars . nil))))
(add-hook 'after-make-frame-functions 'my/disable-scroll-bars)

;; refresh file if changed
(global-auto-revert-mode t)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(markdown-command "/usr/local/bin/pandoc")
 '(package-selected-packages
   '(company-jedi popup-kill-ring elpy which-key projectile org-bullets rainbow-delimiters rainbow-mode rg mark-multiple avy emms markdown-preview-mode markdown-mode grip-mode ## helm-mode-manager vdiff sunrise-x-tabs sunrise-x-modeline sunrise-x-buttons rpn-calc powerline-evil pdf-tools neotree multiple-cursors minimap latex-preview-pane latex-pretty-symbols indent-guide helm gnuplot-mode ess dracula-theme company auto-complete auctex)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(require 'emms-setup)
(emms-all)
(emms-default-players)
(setq emms-source-file-default-directory "~/Music/") ;; Change to your music folder
(setq emms-info-functions '(emms-info-tinytag))  ;; When using Tinytag
(setq emms-info-functions '(emms-info-exiftool)) ;; When using Exiftool
(setq emms-browser-covers 'emms-browser-cache-thumbnail-async)
;; (require 'emms-setup)
;; (emms-all)
;; (emms-default-players)
;; (setq emms-source-file-default-directory "~/Music/")
;; ;;Tinytag is a Python script, so you will need access to Python and install it with pip install tinytag. 
;; (setq emms-info-functions '(emms-info-tinytag))
;; (setq emms-browser-covers 'emms-browser-cache-thumbnail-async)
;; (setq emms-player-list '(emms-player-mpg321))
;;                         emms-player-mplayer))
;; (setq emms-player-mpg321-parameters '("-o" "alsa"))
;; (emms-player-for '(*track* (type . file)
;;            

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
  (helm-mode 1))

(require 'helm-config)    
(helm-autoresize-mode 1)
(define-key helm-find-files-map (kbd "C-b") 'helm-find-files-up-one-level)
(define-key helm-find-files-map (kbd "C-f") 'helm-execute-persistent-action)

;; This extension allows you to quickly mark the next occurence of a region and edit them all at once. Wow!
(use-package mark-multiple
 :ensure t
  :bind ("C-c q" . 'mark-next-like-this))

;;Every time emacs encounters a hexadecimal code that resembles a color, it will automatically highlight it in the appropriate color.
(use-package rainbow-mode
  :ensure t
  :init
    (add-hook 'prog-mode-hook 'rainbow-mode))

(use-package rainbow-delimiters
  :ensure t
  :init
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package org-bullets
  :ensure t
  :config
    (add-hook 'org-mode-hook (lambda () (org-bullets-mode))))

(use-package dmenu
  :ensure t
  :bind
 ("S-SPC" . 'dmenu))
 
(setq scroll-conservatively 100)

;; (use-package projectile
;;   :ensure t
;;   :init
;;     (projectile-mode 1))
