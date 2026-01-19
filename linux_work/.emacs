(require 'package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(let ((default-directory "~/.emacs.d/site-lisp/"))
      (normal-top-level-add-subdirs-to-load-path))
(package-initialize)

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
  "Load a random theme from the list at startup and print the loaded theme name."
  (interactive)
  (let ((themes-list '(modus-operandi
                       twilight-bright
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
                       gruvbox)))
    (let ((chosen-theme (nth (random (length themes-list)) themes-list)))
      (load-theme chosen-theme t)
      (message "Loaded theme: %s" chosen-theme))))

(my-random-theme)

(tool-bar-mode -1)
(scroll-bar-mode -1)
(set-default 'truncate-lines t)
(setq inhibit-startup-screen t)

(evil-mode 1)
;; (require 'evil)
;; (defun enable-evil-only-in-text-modes ()
;;   "Enable Evil mode only in text-related buffers."
;;   (when (derived-mode-p 'prog-mode 'text-mode)
;;     (evil-local-mode 1)))  ;; Enable Evil only for programming & text modes

;; (add-hook 'after-change-major-mode-hook 'enable-evil-only-in-text-modes)

;;(require 'efar)
;;(efar-do-change-theme)

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

(add-hook 'prog-mode-hook 'linum-mode)
(add-hook 'find-file-hook 'linum-mode)

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
(setq dired-listing-switches "-lAGhv")

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
(define-key dired-mode-map (kbd "C-c C-z") 'tda/zip)

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

(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

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
 '(bmkp-last-as-first-bookmark-file "c:/Users/benoi/AppData/Roaming/.emacs.d/bookmarks")
 '(custom-safe-themes
   '("78e6be576f4a526d212d5f9a8798e5706990216e9be10174e3f3b015b8662e27" default))
 '(org-agenda-files '("c:/Users/bdulauroy/work.org"))
 '(package-selected-packages
   '(csv-mode anaconda-mode color-theme-sanityinc-tomorrow moe-theme modus-themes doom-themes solarized-theme leuven-theme anti-zenburn-theme twilight-bright-theme zenburn-theme melancholy-theme which-key keepass-mode jupyter gnuplot-mode dir-treeview beacon elpygen modus-operandi-theme highlight pdf-tools focus dired-rainbow material-theme conda pyvenv-auto disk-usage gnuplot dired-rsync popup-kill-ring elpy latex-preview-pane helm-swoop dired-hacks-utils evil-ediff multi use-package org-bullets projectile rainbow-delimiters emms rainbow-mode vdiff ## sunrise-x-tabs sunrise-x-modeline sunrise-x-buttons spacemacs-theme rpn-calc powerline openwith neotree multiple-cursors monokai-pro-theme minimap magit indent-guide hydra helm gruvbox-theme evil ess dracula-theme company auto-complete auctex)))

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

(use-package org-bullets
  ;; :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode))))

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
;;(add-to-list 'exec-path "c:/Apps/ImageMagick")
;;(add-to-list 'exec-path "C:/Program Files (x86)/Gnuplot/bin")
;;(add-to-list 'exec-path "C:/Users/benoi/anaconda3/Scripts")
;; if not set in the environment variable on the system
(setq shell-file-name explicit-shell-file-name)
;;(add-to-list 'exec-path "C:/cygwin64/bin")
;;(add-to-list 'exec-path "C:/Apps/emacs/libexec/emacs/27.1/x86_64-w64-mingw32")
;;(add-to-list 'exec-path "C:/Apps/bin")
;;(setenv "SHELL" "cmdproxy.exe")
(setq using-unix-filesystems t)
;;(setq shell-file-name "cmdproxy")
;;(setq explicit-shell-file-name "cmdproxy.exe")
;;(setq explicit-shell-file-name "bash.exe")
;;(setq shell-command-switch "/C")
;;(setq exec-path (append exec-path '("C:/Windows/System32/OpenSSH")))

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

(use-package org-bullets
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode))))

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

;;https://github.com/Fuco1/dired-hacks/issues/14
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
    (dired-rainbow-define markdown "#ffed4a" ("org" "etx" "info" "markdown" "md" "mkd" "nfo" "pod" "rst" "tex" "textfile" "txt"))
    ;; for light color scheme
    ;;(dired-rainbow-define markdown "#2f4a31" ("org" "etx" "info" "markdown" "md" "mkd" "nfo" "pod" "rst" "tex" "textfile" "txt"))
    (dired-rainbow-define database "#6574cd" ("xlsx" "xls" "csv" "accdb" "db" "mdb" "sqlite" "nc"))
    (dired-rainbow-define media "#de751f" ("mp3" "mp4" "MP3" "MP4" "avi" "mpeg" "mpg" "flv" "ogg" "mov" "mid" "midi" "wav" "aiff" "flac"))
    (dired-rainbow-define image "#f66d9b" ("tiff" "tif" "cdr" "gif" "ico" "jpeg" "jpg" "png" "psd" "eps" "svg"))
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

;; Linux specific

;; rsync files
;; (defun ora-dired-rsync (dest)
;;   (interactive
;;    (list (expand-file-name
;;           (read-file-name "Rsync -arvzu --delete to:" (dired-dwim-target-directory)))))
;;   ;; store all selected files into "files" list
;;   (let* ((files (dired-get-marked-files nil current-prefix-arg))
;;          ;; the rsync command
;;          (tmtxt/rsync-command "rsync -arvzu --delete --progress ")
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
;;                             ;; rsync want only filenames for second source files
;;                             (format "\"%s\"" (nth 2 (s-split ":" file)))))
;;                       (shell-quote-argument file)) " ")))
;;     (when (not (string= ssh_prefix ""))
;;       ;; Need convert command from:
;;       ;; rsync -arvzu --delete --progress -e ssh "root@10.63.200.24:/root/files" "/root/roles" /Users/bravo/temp/rsync/
;;       ;; to:
;;       ;; rsync -arvzu --delete --progress -e ssh "root@10.63.200.24:/root/files /root/roles" /Users/bravo/temp/rsync/
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
;; rsync files

(defun ora-dired-rsync (dest)
  (interactive
   (list (expand-file-name
          (read-file-name "Rsync -arvzu to:" (dired-dwim-target-directory)))))
  ;; store all selected files into "files" list
  (let* ((source-dir (dired-current-directory)) ;; Get the current directory as the source
         (files (dired-get-marked-files nil current-prefix-arg))
         ;; initialize the rsync command
         (rsync-command "rsync -arvzu --progress ")
         (ssh_prefix "")
         ;; Generate a unique buffer name based on source and destination
         (buffer-name (generate-new-buffer-name
                       (format "*Rsync from %s to %s*"
                               (file-name-nondirectory (directory-file-name source-dir))
                               (if (string-match "^/ssh:\\(.*\\)$" dest)
                                   (match-string 1 dest)
                                 (file-name-nondirectory (directory-file-name dest))))))
         )
    ;; add all selected file names as arguments to the rsync command
    (dolist (file files)
      (setq rsync-command
            (concat rsync-command
                    (if (string-match "^/ssh:\\(.*:\\)\\(.*\\)$" file)
                        (let ((ssh-dest (match-string 1 file))
                              (ssh-file (match-string 2 file)))
                          (if (string= ssh_prefix "")
                              (setq ssh_prefix (format " -e ssh \"%s%s\"" ssh-dest (shell-quote-argument ssh-file))))
                          (format "\"%s\"" (shell-quote-argument ssh-file)))
                      (shell-quote-argument file)) " ")))
    (when (not (string= ssh_prefix ""))
      (setq rsync-command (replace-regexp-in-string "\" \"" " " rsync-command)))
    ;; append the destination
    (setq rsync-command
          (concat rsync-command
                  (if (string-match "^/ssh:\\(.*\\)$" dest)
                      (format " -e ssh %s" (match-string 1 dest))
                    (shell-quote-argument dest))))
    ;; run the async shell command with a unique buffer name
    (let ((default-directory (expand-file-name "~")))
      (async-shell-command rsync-command buffer-name))
    (message rsync-command)
    ))

(define-key dired-mode-map "Y" 'ora-dired-rsync)

(defun ora-dired-rsync2 (dest)
  (interactive
   (list (expand-file-name
          (read-file-name "Rsync -arv to:" (dired-dwim-target-directory)))))
  (let* ((source-dir (file-name-as-directory (dired-current-directory)))
         (buffer-name (format "*rsync %s -> %s*"
                              (file-name-nondirectory (directory-file-name source-dir))
                              (if (string-match "^/ssh:\\(.*\\)$" dest)
                                  (match-string 1 dest)
                                (file-name-nondirectory (directory-file-name dest)))))
         (tmtxt/rsync-command (format "rsync -arv --progress %s"
                                      (shell-quote-argument source-dir))))
    (if (string-match "^/ssh:\\(.*\\)$" dest)
        (setq tmtxt/rsync-command
              (format "%s -e ssh %s" tmtxt/rsync-command (match-string 1 dest)))
      (setq tmtxt/rsync-command
            (concat tmtxt/rsync-command " " (shell-quote-argument dest))))
    
    ;; Run the async shell command specifying a unique buffer name
    (let ((default-directory (expand-file-name "~")))
      (async-shell-command tmtxt/rsync-command buffer-name))
    (message tmtxt/rsync-command)
    
    ;; After running the command, split the window and switch to the named output buffer
;;    (split-window-below) ;; Split the window vertically
    (other-window 1)     ;; Move to the newly created window
    (switch-to-buffer buffer-name) ;; Switch to the async command's output buffer
    ))

(define-key dired-mode-map "T" 'ora-dired-rsync2)

;; the menu font is set in ~/.xdefaults
;; set 85 for full HD and 180 for 4K
(set-face-attribute 'default nil
                    :family "Liberation Mono"
                    :height 85)


(defun dired-open-with-choice-linux ()
  "Dired helper to open files or perform format conversions."
  (interactive)
  (let* ((file (dired-get-file-for-visit))
         (gif-output (concat (file-name-sans-extension file) ".gif"))
         (video-output (concat (file-name-sans-extension file) ".mp4"))
         (emf-output (concat (file-name-sans-extension file) ".emf"))
         (output-file (concat (file-name-sans-extension file) ".pdf"))
         (choices (cond
                   ;; Images
                   ((string-match "\\.gif\\'" file) '("display" "eog" "gimp" "Convert to Video (mp4)"))
                   ((string-match "\\.svg\\'" file) '("display" "eog" "inkscape" "Convert to EMF"))
                   ;; Video
                   ((string-match "\\.\\(mp4\\|mkv\\|avi\\|m4v\\)\\'" file) '("vlc" "ffplay" "Convert to GIF"))
                   ;; PS/EPS
                   ((string-match "\\.\\(ps\\|eps\\)\\'" file) '("okular" "evince" "Convert to PDF (ps2pdf)" "Convert to PDF (gs)"))
                   ;; Documents
                   ((string-match "\\.pdf\\'" file) '("okular" "evince"))
                   ((string-match "\\.\\(docx\\|docm\\)\\'" file) '("libreoffice --writer"))
                   ((string-match "\\.\\(pptx\\|pptm\\)\\'" file) '("libreoffice --impress"))
                   ((string-match "\\.\\(xlsx\\|xlsm\\|xls\\)\\'" file) '("libreoffice --calc"))
                   ((string-match "\\.csv\\'" file) '("libreoffice --calc" "nedit" "gvim"))
                   ;; Archives
                   ((string-match "\\.\\(zip\\|7z\\|rar\\|tar.gz\\|tar.xz\\|gz\\)\\'" file) '("ark"))
                   ;; Audio
                   ((string-match "\\.\\(mp3\\|wav\\|flac\\)\\'" file) '("vlc" "mpv" "rhythmbox"))
                   ;; Text files
                   ((string-match "\\.\\(txt\\|dat\\|nfo\\|ini\\|md\\|f\\|f90\\)\\'" file) '("gedit" "nedit" "gvim" "kate" "kwrite" "code"))
                   ;; Images
                   ((string-match "\\.\\(png\\|jpg\\|jpeg\\|bmp\\|tif\\)\\'" file) '("display" "eog" "gimp" "gwenview"))))
         (program (helm-comp-read "Open with: " choices :must-match t)))

    (cond
     ;; Convert GIF to MP4
     ((string-equal program "Convert to Video (mp4)")
      (start-process-shell-command "gif-to-video" nil
                                   (format "ffmpeg -i %s %s"
                                           (shell-quote-argument file)
                                           (shell-quote-argument video-output)))
      (message "Converting GIF to MP4..."))

     ;; Convert Video to GIF (Simplified)
     ((string-equal program "Convert to GIF")
      (start-process-shell-command "video-to-gif" nil
                                   (format "ffmpeg -i %s -vf fps=10,scale=-1:-1 %s"
                                           (shell-quote-argument file)
                                           (shell-quote-argument gif-output)))
      (message "Converting Video to GIF..."))

     ;; Convert SVG to EMF
     ((string-equal program "Convert to EMF")
      (start-process-shell-command "svg-to-emf" nil
                                   (format "inkscape %s --export-type=emf --export-filename=%s"
                                           (shell-quote-argument file)
                                           (shell-quote-argument emf-output)))
      (message "Converting SVG to EMF..."))

     ;; Convert PS/EPS to PDF (ps2pdf)
     ((string-equal program "Convert to PDF (ps2pdf)")
      (start-process-shell-command "ps-to-pdf" nil
                                   (format "ps2pdf %s %s"
                                           (shell-quote-argument file)
                                           (shell-quote-argument output-file)))
      (message "Converting PS/EPS to PDF with ps2pdf..."))

     ;; Convert PS/EPS to PDF (Ghostscript)
     ((string-equal program "Convert to PDF (gs)")
      (start-process-shell-command "ps-to-pdf" nil
                                   (format "gs -dNOPAUSE -dBATCH -sDEVICE=pdfwrite -sOutputFile=%s %s"
                                           (shell-quote-argument output-file)
                                           (shell-quote-argument file)))
      (message "Converting PS/EPS to PDF with Ghostscript..."))

     ;; Open normally if no conversion is selected
     (t
      (start-process-shell-command "dired-open" nil
                                   (format "%s %s &" program (shell-quote-argument file)))))))

(define-key dired-mode-map (kbd "!") 'dired-open-with-choice-linux)

(define-key dired-mode-map (kbd "C-c d") 
  (lambda () (interactive) (shell-command "display -resize 70% *.svg &")))


;; Set Frame width/height (can be set in ~/.Xdefaults too) size depends if ETX or others
(setq default-frame-alist
      '((top . 35) (left . 240) (width . 200) (height . 55)))

;; make emacs recognize my bash aliases and functions & use bash as default shell                                                                                              
(setq explicit-shell-file-name "/bin/bash")                                                                                                                                    
(setq shell-file-name "bash")                                                                                                                                                  
(setq explicit-bash.exe-args '("--noediting" "--login" "-ic"))                                                                                                                 
(setq shell-command-switch "-ic")                                                                                                                                              
(setenv "SHELL" shell-file-name)
;; kill terminal bufgfer with cd
(defun term-handle-exit--close-buffer (&rest args)
  (when (null (get-buffer-process (current-buffer)))
    (insert "Press <C-d> to kill the buffer.")
    (use-local-map (let ((map (make-sparse-keymap)))
                     (define-key map (kbd "C-d")
                       (lambda ()
                         (interactive)
                         (kill-buffer (current-buffer))))
                     map))))
(advice-add 'term-handle-exit :after #'term-handle-exit--close-buffer)

;; https://www.emacswiki.org/emacs/EmacsSvg
(auto-image-file-mode 1) 

;; https://emacsnotes.wordpress.com/2018/08/09/222/
;; (with-eval-after-load "doc-view"
;;   (easy-menu-define my-doc-view-menu doc-view-mode-map "Menu for Doc-View Mode."
;;     '("DocView"
;;       ["Switch to a different mode" doc-view-toggle-display :help "Switch to a different mode"]
;;       ["Open Text" doc-view-open-text :help "Display the current doc's contents as text"]
;;       "--"
;;       ("Navigate Doc"
;;        ["Goto Page ..." doc-view-goto-page :help "View the page given by PAGE"]
;;        "--"
;;        ["Scroll Down" doc-view-scroll-down-or-previous-page :help "Scroll page down ARG lines if possible, else goto previous page"]
;;        ["Scroll Up" doc-view-scroll-up-or-next-page :help "Scroll page up ARG lines if possible, else goto next page"]
;;        "--"
;;        ["Next Line" doc-view-next-line-or-next-page :help "Scroll upward by ARG lines if possible, else goto next page"]
;;        ["Previous Line" doc-view-previous-line-or-previous-page :help "Scroll downward by ARG lines if possible, else goto previous page"]
;;        ("Customize"
;;         ["Continuous Off"
;;          (setq doc-view-continuous nil)
;;          :help "Stay put in the current page, when moving past first/last line" :style radio :selected
;;          (eq doc-view-continuous nil)]
;;         ["Continuous On"
;;          (setq doc-view-continuous t)
;;          :help "Goto to the previous/next page, when moving past first/last line" :style radio :selected
;;          (eq doc-view-continuous t)]
;;         "---"
;;         ["Save as Default"
;;          (customize-save-variable 'doc-view-continuous doc-view-continuous)
;;          t])
;;        "--"
;;        ["Next Page" doc-view-next-page :help "Browse ARG pages forward"]
;;        ["Previous Page" doc-view-previous-page :help "Browse ARG pages backward"]
;;        "--"
;;        ["First Page" doc-view-first-page :help "View the first page"]
;;        ["Last Page" doc-view-last-page :help "View the last page"])
;;       "--"
;;       ("Adjust Display"
;;        ["Enlarge" doc-view-enlarge :help "Enlarge the document by FACTOR"]
;;        ["Shrink" doc-view-shrink :help "Shrink the document"]
;;        "--"
;;        ["Fit Width To Window" doc-view-fit-width-to-window :help "Fit the image width to the window width"]
;;        ["Fit Height To Window" doc-view-fit-height-to-window :help "Fit the image height to the window height"]
;;        "--"
;;        ["Fit Page To Window" doc-view-fit-page-to-window :help "Fit the image to the window"]
;;        "--"
;;        ["Set Slice From Bounding Box" doc-view-set-slice-from-bounding-box :help "Set the slice from the document's BoundingBox information"]
;;        ["Set Slice Using Mouse" doc-view-set-slice-using-mouse :help "Set the slice of the images that should be displayed"]
;;        ["Set Slice" doc-view-set-slice :help "Set the slice of the images that should be displayed"]
;;        ["Reset Slice" doc-view-reset-slice :help "Reset the current slice"])
;;       ("Search"
;;        ["New Search ..."
;;         (doc-view-search t)
;;         :help "Jump to the next match or initiate a new search if NEW-QUERY is given"]
;;        "--"
;;        ["Search" doc-view-search :help "Jump to the next match or initiate a new search if NEW-QUERY is given"]
;;        ["Backward" doc-view-search-backward :help "Call `doc-view-search' for backward search"]
;;        "--"
;;        ["Show Tooltip" doc-view-show-tooltip :help nil])
;;       ("Maintain"
;;        ["Reconvert Doc" doc-view-reconvert-doc :help "Reconvert the current document"]
;;        "--"
;;        ["Clear Cache" doc-view-clear-cache :help "Delete the whole cache (`doc-view-cache-directory')"]
;;        ["Dired Cache" doc-view-dired-cache :help "Open `dired' in `doc-view-cache-directory'"]
;;        "--"
;;        ["Revert Buffer" doc-view-revert-buffer :help "Like `revert-buffer', but preserves the buffer's current modes"]
;;        "--"
;;        ["Kill Proc" doc-view-kill-proc :help "Kill the current converter process(es)"]
;;        ["Kill Proc And Buffer" doc-view-kill-proc-and-buffer :help "Kill the current buffer"])
;;       "--"
;;       ["Customize"
;;        (customize-group 'doc-view)]))
;;   (easy-menu-define my-doc-view-minor-mode-menu doc-view-minor-mode-map "Menu for Doc-View Minor Mode."
;;     '("DocView*"
;;       ["Display in DocView Mode" doc-view-toggle-display :help "View"]
;;       ["Exit DocView Mode" doc-view-minor-mode])))

;; remove menu bar
;;(menu-bar-mode -1)

;;change menu bar colors
(set-face-attribute 'menu nil
                    :inverse-video nil
                    :background "#e8e8e7"
                    :foreground "black"
                    :bold t)

;; use csh in Lyon with c-x t key
;;(defun csh ()
;;  (interactive)
;;  (term "/bin/csh"))
;;(global-set-key (kbd "C-x t") 'csh)

(defun my-sh-send-command (command)
  "Send command to the current shell process.
  See URL `https://stackoverflow.com/a/7053298/5065796'
  Creates a new shell process if none exists."
  (let ((proc (get-process "shell"))
        pbuff)
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
    
(defun cstor ()
  (interactive)
  (let* ((current-path (dired-current-directory))
         (pattern-start (string-match "FS1-" current-path)))
    (when pattern-start
      (let* ((pattern (substring current-path (+ pattern-start 4)))
             (first-part (substring pattern 0 3))
             (second-part (substring pattern 3 9))) ; Extract only the first 6 characters of the last group
        (setq new-path (concat "/mecheng/calc/FS1/" first-part "/" second-part))
        (my-sh-send-command (concat "cstor -d " new-path))))))

(global-set-key (kbd "<S-f7>") 'cstor)

(defun compare-files-with-tool ()
  "Compare two files in Emacs using either TKDIFF or DIFFUSE."
  (interactive)
  (let* ((left-file (buffer-file-name (window-buffer (selected-window))))
         (right-file (buffer-file-name (window-buffer (next-window))))
         (tool (completing-read "Choose comparison tool: "
                                '("tkdiff" "diffuse" "kdiff3")
                                nil t "tkdiff")))  ;; "tkdiff" is prefilled as the default
    (if (and left-file right-file)
        (shell-command (format "%s %s %s" tool left-file right-file))
      (message "Please open two files for comparison first."))))

(global-set-key (kbd "C-c C-t") 'compare-files-with-tool)


(defun compare-files-with-gvim ()
  "Compare two files in Emacs using gvim."
  (interactive)
  (let ((left-file (buffer-file-name (window-buffer (selected-window))))
        (right-file (buffer-file-name (window-buffer (next-window))))
        (gvim-command "gvim -d"))
    (if (and left-file right-file)
        (shell-command (format "%s %s %s" gvim-command left-file right-file))
      (message "Please open two files for comparison first."))))

(global-set-key (kbd "C-c C-v") 'compare-files-with-gvim)
    
;; ;; (global-set-key (kbd "C-c j") 'create-script-with-shebang)
;; (defun create-script-with-shebang (file-name)
;;   "Create a new script file with a specified shebang line and open in Emacs."
;;   (interactive "sEnter script name: ")
;;   (let* ((file-path (expand-file-name file-name))
;;          (language (completing-read "Choose shebang (Bash [b] | Python [p] | PBS [pbs] | Gnuplot [g]): "
;;                                     '(("b" "Bash") ("p" "Python") ("pbs" "PBS") ("g" "Gnuplot"))))
;;          (shebang-line (cond ((string= language "b") "#!/bin/bash")
;;                              ((string= language "p") "#!/usr/bin/env python")
;;                              ((string= language "pbs") "#!/bin/bash\n#PBS -l nodes=1:ppn=2\n#PBS -q normal\n#PBS -N processing")
;;                              ((string= language "g") "#!/usr/bin/env gnuplot")
;;                              (t ""))))
;;     (if (file-exists-p file-path)
;;         (message "File already exists!")
;;       (with-temp-file file-path
;;         (insert shebang-line "\n")))
;;       (shell-command (format "chmod +x %s" file-path)) ; Set executable permissions
;;       (find-file file-path)
;;       (message "Script %s created with shebang line: %s"
;;                file-name shebang-line)))

;; (global-set-key (kbd "C-c j") 'create-script-with-shebang)

(defun create-script-with-shebang-and-gnuplot (file-name)
  "Create a new script file with a specified shebang line and open in Emacs.
   Include a sample Gnuplot script in the file."
  (interactive "sEnter script name: ")
  (let* ((file-path (expand-file-name file-name))
         (language (completing-read "Choose shebang (Bash [b] | Python [p] | PBS [pbs] | Gnuplot [g]): "
                                    '(("b" "Bash") ("p" "Python") ("pbs" "PBS") ("g" "Gnuplot"))))
         (shebang-line (cond ((string= language "b") "#!/bin/bash")
                             ((string= language "p") "#!/usr/bin/env python")
                             ((string= language "pbs") "#!/bin/bash\n#PBS -l nodes=1:ppn=2\n#PBS -q normal\n#PBS -N processing")
                             ((string= language "g") "#!/usr/bin/env gnuplot")
                             (t (format "#!/usr/bin/env %s" language))))
         (gnuplot-content (if (string= language "g")
                              (concat "
set xtics font \",13\" 
set ytics font \",13\"
set term svg
set xlabel 'x []' font \",13\" tc rgb '#606060'
set ylabel 'y []' font \",13\" tc rgb '#606060'
# set xrange [0:100]
# set yrange [0:100]
# set ylabel 'PSD [g^2/Hz]' font \"Nimbus,12\" tc rgb '#606060' offset -1.2,0
# set key opaque
# set grid back ls 12
# set grid ytics lc rgb '#C0C0C0'
# set lmargin 14

set style line 13 lt 1 pt 7 lc rgb '#0072bd' # blue
set style line 14 lt 1 pt 7 lc rgb '#d95319' # orange
set style line 15 lt 1 pt 7 lc rgb '#edb120' # yellow
set style line 16 lt 1 pt 7 lc rgb '#7e2f8e' # purple
set style line 17 lt 1 pt 7 lc rgb '#77ac30' # green
set style line 18 lt 1 pt 7 lc rgb '#4dbeee' # light-blue
set style line 19 lt 1 pt 7 lc rgb '#c06c84' # pink
set style line 20 lt 1 pt 7 lc rgb '#7f4e34' # brown
set style line 21 lt 1 pt 7 lc rgb '#606060' # grey

set output \"my_output.svg\"
set title 'my_title' font \",13\" tc rgb '#606060'
plot 'data1.dat' using 1:2  w l ls 13 lw 2 title 'MSR',\\
'data2.dat' using 1:2  w l ls 14 lw 2 title 'HBM: CH1'
    
materials = \"Sorbothane Rubber Neoprene EPDM Super8 'Water Resistant Sorbothane'\" 
directories = \"01_Test_2023-10-16 02_Test_2023-10-18 03_Test_2023-10-20 04_Test_2023-10-23 05_Test_2023-10-24 06_Test_2023-10-24\" 
ch = \"CH1 CH2 CH3 CH4 CH5 CH6\"

do for [i=1:words(ch)] {
    set output sprintf(\"CH%d.svg\", i)
    name =  sprintf(\"CH_%d\", i)
    plot for [j=1:words(materials)] sprintf('./%s/dat/%s.dat', word(directories, j), name) using 1:($2/in**2) w l ls j+12 lw 2 title sprintf(\"%s\",word(materials,j))
}
")
                            ""))
         (script-content (concat shebang-line "\n" gnuplot-content)))
    (if (file-exists-p file-path)
        (message "File already exists!")
      (with-temp-file file-path
        (insert script-content))
      (shell-command (format "chmod +x %s" file-path)) ; Set executable permissions
      (find-file file-path)
      (message "Script %s created with shebang line:\n%s\nand sample content."
               file-name shebang-line))))

(global-set-key (kbd "C-c j") 'create-script-with-shebang-and-gnuplot)


(defun copy-directory-path ()
  "Copy the path of the current directory in Dired mode to the clipboard."
  (interactive)
  (let ((dir (dired-current-directory)))
    (kill-new (expand-file-name dir))
    (message "Directory path copied to clipboard: %s" dir)))

(define-key dired-mode-map (kbd "0 q") 'copy-directory-path)

(defun dired-copy-full-path-to-clipboard ()
  "Copy the full path of the selected file in Dired to the clipboard."
  (interactive)
  (let ((file-path (dired-get-file-for-visit)))
    (if file-path
        (progn
          (kill-new (file-truename file-path))
          (message "Full path copied to clipboard: %s" (file-truename file-path)))
      (message "No file selected in Dired."))))

(define-key dired-mode-map (kbd "0 w") 'dired-copy-full-path-to-clipboard)

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

    (revert-buffer)  ; Refresh Dired buffer
    (message "Gnuplot process initiated.")
    ))

(global-set-key (kbd "C-c C-g") 'eplot)
    
(defun plot-parquet-files ()
  "Plot selected parquet.gzip files in Dired using Python and matplotlib."
  (interactive)
  (let* ((files (dired-get-marked-files)) ;; Get marked files
         (python-script-path "./plot_parquet_files.py") ;; Temporary Python script path
         (python-code (concat
                       "import pandas as pd\n"
                       "import matplotlib.pyplot as plt\n"
                       ;; Read all selected .parquet.gzip files into pandas DataFrames
                       "dfs = [pd.read_parquet('" (mapconcat #'identity files "', engine='pyarrow')\n       pd.read_parquet('") "', engine='pyarrow')]\n"
                       "df = pd.concat(dfs)\n" ;; Concatenate all DataFrames
                       "df.set_index(df.columns[0], inplace=True)\n"
                       "df.plot()\n" ;; Plot the combined DataFrame
                       "plt.show()\n")) ;; Show the plot
         ;; Command to run the Python script asynchronously
         (cmd (format "python %s" python-script-path)))
    ;; Write the Python code to the script file
    (with-temp-file python-script-path
      (insert python-code))
    ;; Execute the Python script asynchronously
    (async-shell-command cmd "*Async Python Plot*")))

(define-key dired-mode-map (kbd "C-c p") 'plot-parquet-files)

(evil-define-key 'normal dired-mode-map
  "gg" 'beginning-of-buffer
  "G" 'end-of-buffer
  "gr" 'revert-buffer)

(eval-after-load 'dired
  '(evil-define-key 'normal dired-mode-map (kbd "H") 'dired-do-kill-lines))

;; Custom function to resize window left
(defun my-shrink-window-horizontally ()
  "Shrink the window horizontally by 20 steps."
  (interactive)
  (shrink-window-horizontally 20))

;; Custom function to resize window right
(defun my-enlarge-window-horizontally ()
  "Enlarge the window horizontally by 10 steps."
  (interactive)
  (enlarge-window-horizontally 20))

;; Rebind C-x { and C-x } to use the new functions
(global-set-key (kbd "C-x {") 'my-shrink-window-horizontally)
(global-set-key (kbd "C-x }") 'my-enlarge-window-horizontally)

;; Define an asynchronous delete function
(defun dired-async-delete ()
  "Asynchronously delete the marked files or directories in Dired."
  (interactive)
  (let ((files (dired-get-marked-files))) ;; Get marked files in Dired
    (if (yes-or-no-p (format "Asynchronously delete %d marked file(s)? " (length files)))
        (async-start
         `(lambda ()
            (require 'dired) ;; Ensure Dired functions are available
            (mapc #'(lambda (file)
                      (if (file-directory-p file)
                          (delete-directory file t) ;; Recursively delete directories
                        (delete-file file)))
                  ',files))
         (lambda (_)
           (message "Asynchronous delete completed.")))
      (message "Async delete canceled."))))

;; Bind the asynchronous delete function to a specific key (e.g., `A` for async-delete)
(define-key dired-mode-map (kbd "A") 'dired-async-delete)

(defun ibuffer-show-shell-buffers ()
  "Open ibuffer showing only shell, eshell, and term buffers."
  (interactive)
  (let ((ibuffer-saved-filter-groups
         '(("shell-buffers"
            ("Shell/Eshell/Term"
             (or (mode . shell-mode)
                 (mode . eshell-mode)
                 (mode . term-mode))))))
        (ibuffer-filter-groups nil))
    (ibuffer nil "*Ibuffer-Shells*")
    (ibuffer-switch-to-saved-filter-groups "shell-buffers")))

;; Bind it to a key combination, e.g., C-x t
(global-set-key (kbd "C-x t") 'ibuffer-show-shell-buffers)
    
(defun dired-open-in-nautilus ()
  "Open the current directory in Nautilus."
  (interactive)
  (let ((dir (dired-current-directory)))
    (start-process "nautilus" nil "nohup" "nautilus" "-w" dir)))

(define-key dired-mode-map (kbd "C-c n") 'dired-open-in-nautilus)

(defun my-dired-display-svg-in-emacs-imagemagick ()
  "Convert SVG to PNG using ImageMagick `convert` only if needed and display inside Emacs."
  (interactive)
  (let* ((file (dired-get-filename nil t))
         (png-file (concat (file-name-sans-extension file) ".png")))
    (if (string-match-p "\\.svg\\'" file)
        (progn
          ;; Only convert if the PNG does not exist or if the SVG is newer
          (when (or (not (file-exists-p png-file))
                    (file-newer-than-file-p file png-file))
            (shell-command (format "convert -background white %s %s"
                                   (shell-quote-argument file)
                                   (shell-quote-argument png-file))))
          ;; Open without asking to reread from disk
          (find-file png-file)
          (revert-buffer-no-confirm))  ;; Prevent Emacs from prompting
      (dired-find-file))))  ;; Open non-SVG files normally

(defun revert-buffer-no-confirm ()
  "Reload the current buffer without confirmation."
  (interactive)
  (let ((inhibit-message t))  ;; Suppress annoying messages
    (revert-buffer :ignore-auto :noconfirm)))

;; Bind RET in dired to use this function
(define-key dired-mode-map (kbd "RET") #'my-dired-display-svg-in-emacs-imagemagick)

(defun kwutape8 ()
  "Prompt for command-line arguments and run kwutape8.py asynchronously in the current directory."
  (interactive)
  (let* ((nodes (read-string "Node number: "))
         ;; Suggest commonly used options for --value
         (value (completing-read "Post-processing value (e.g., x, ax, f_1, mz_i, mz_j, fx_i, fy_i, fx_j, fy_j ...): "
                                 '("x" "ax" "f_1" "mz_i" "mz_j" "fx_i" "fy_i" "fx_j" "fy_j") nil nil))
         (tstart (read-string "Time to start the processing (leave empty if not needed): "))
         (tend (read-string "Time to end the processing (leave empty if not needed): "))
         (noderef (read-string "Reference node (leave empty if not needed): "))
         ;; Select --plot option from a list
         (plot-options '(("s" . "svg") ("d" . "dynamic") ("n" . "no") ("" . "")))
         (plot-choice (completing-read "Choose plot type (s: svg, d: dynamic, n: no, or leave empty): "
                                       (mapcar #'car plot-options)))
         (plot (cdr (assoc plot-choice plot-options)))
         ;; Yes/No flags (grouped at the end)
         (dat (if (y-or-n-p "Include raw data output (--dat)? ") "--dat" ""))
         (parquet (if (y-or-n-p "Generate Parquet file (--parquet)? ") "--parquet" ""))
         (csv (if (y-or-n-p "Generate CSV file (--csv)? ") "--csv" ""))
         (excel (if (y-or-n-p "Generate Excel file (--excel)? ") "--excel" ""))
         (verbose (if (y-or-n-p "Enable verbose output (--verbose)? ") "--verbose" "")))
    ;; Define the output buffer separately (fixes the error)
    (let ((output-buffer "*kwutape8-output*")
          (cmd (concat "python kwutape8.py --nodes " nodes
                       " --value " value
                       (if (not (string-empty-p tstart)) (concat " --tstart " tstart) "")
                       (if (not (string-empty-p tend)) (concat " --tend " tend) "")
                       (if (not (string-empty-p noderef)) (concat " --noderef " noderef) "")
                       (if (not (string-empty-p plot)) (concat " --plot " plot) "")
                       " " dat " " parquet " " csv " " excel " " verbose)))
      ;; Display the generated command in *Messages*
      (message "Running asynchronously: %s" cmd)
      ;; Run asynchronously in a separate process
      (start-process-shell-command "kwutape8-process" output-buffer cmd)
      ;; Switch to the output buffer
      (pop-to-buffer output-buffer))))

(defun my-dired-open-shell ()
  "Open a new shell buffer in the current Dired directory."
  (interactive)
  (let ((dir (dired-current-directory))
        (shell-buffer (generate-new-buffer-name "*shell*")))
    (with-current-buffer (shell shell-buffer)
      (cd dir))))

(define-key dired-mode-map (kbd "C-c s") 'my-dired-open-shell)

(add-hook 'doc-view-mode-hook (lambda () (doc-view-fit-height-to-window)))
(setq doc-view-ghostscript-program "gs")  ;; Use Ghostscript
(setq doc-view-resolution 80)            ;; Lower resolution for faster loading
(setq doc-view-cache-directory "~/.emacs.d/docview-cache")
(setq doc-view-continuous t)

(define-key dired-mode-map (kbd "i") 'image-dired-dired-display-image)

(setq eshell-prompt-function
      (lambda ()
        (concat (propertize "λ " 'face '(:foreground "magenta" :bold t)))))
