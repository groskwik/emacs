(require 'package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

;;(let ((default-directory "~/.emacs.d/site-lisp/"))
;;  (normal-top-level-add-subdirs-to-load-path))
(package-initialize)

(eval-when-compile
  ;; Following line is not needed if use-package.el is in ~/.emacs.d
  ;;  (add-to-list 'load-path "<path where use-package is installed>")
  (require 'use-package))

(setq byte-compile-warnings '(cl-functions))

;; update my emacs packages
(defun my/update-elpa-packages ()
  "Update all installed ELPA packages without prompting."
  (interactive)
  (require 'package)
  (package-initialize)
  (package-refresh-contents)
  (let ((upgraded 0))
    (dolist (pkg package-alist)
      (let* ((name (car pkg))
             (installed (cadr pkg))
             (available (car (cdr (assq name package-archive-contents)))))
        (when (and available
                   (version-list-< (package-desc-version installed)
                                   (package-desc-version available)))
          (package-install available)
          (setq upgraded (1+ upgraded)))))
    (message "ELPA update complete: %d package(s) upgraded" upgraded)))

(setq evil-undo-system 'undo-redo)

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
                       cyberpunk
                       alect-light
                       alect-dark
                       ample
                       ample-flat
                       ample-light
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
                       zenburn
                       material-light
                       material
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

(autoload 'visual-basic-mode "visual-basic-mode" "Visual Basic mode." t)
;; Associate .vb files with visual-basic-mode
(add-to-list 'auto-mode-alist '("\\.\\(vb\\|vbs\\)$" . visual-basic-mode))
;; (require 'sunrise)
;; (require 'sunrise-buttons)
;; (require 'sunrise-modeline)
;; (require 'sunrise-loop)
;; (require 'sunrise-tabs)
;; (require 'sunrise-tree)
;; (require 'sunrise-w32)

;; Modern replacement for linum-mode
(global-display-line-numbers-mode t)

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
(require 'kwu2-mode)
(add-to-list 'auto-mode-alist '("\\TAPE60\\'" . kwu2-mode))

(require 'kwu5-mode)
(add-to-list 'auto-mode-alist '("\\tape1\\'" . kwu5-mode))

(require 'kwutape8-transient)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Consolas" :foundry "outline" :slant normal :weight normal :height 98 :width normal))))
 '(vdiff-subtraction-face ((t (:inherit diff-added)))))

;;(setq-default line-spacing 0.1)

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
    
(define-key dired-mode-map (kbd "C-c z") 'my-compress-folder)
    
(defun extract-7z-archive (archive)
  "Extract ARCHIVE using 7z to a folder with the same name."
  (interactive "fSelect archive: ")
  (let* ((archive-path (expand-file-name archive))
         (archive-dir (file-name-sans-extension archive-path)))
    (unless (file-directory-p archive-dir)
      (make-directory archive-dir t))
    (let ((command (format "7z x \"%s\" -o\"%s\"" archive-path archive-dir)))
      (async-shell-command command))))

(global-set-key (kbd "C-c e") 'extract-7z-archive)

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

(use-package python
  :ensure nil
  :hook ((python-mode . (lambda ()
                          (setq-local python-shell-dedicated 'project))))
  :bind (:map python-mode-map
              ("C-c C-b" . python-shell-send-buffer)
              ("C-c C-r" . python-shell-send-region)
              ("C-c C-f" . python-shell-send-defun)))

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
   '(apdl-mode go-mode org-bullets csv-mode anaconda-mode color-theme-sanityinc-tomorrow moe-theme modus-themes doom-themes solarized-theme leuven-theme anti-zenburn-theme twilight-bright-theme zenburn-theme melancholy-theme which-key keepass-mode jupyter gnuplot-mode dir-treeview beacon elpygen modus-operandi-theme highlight pdf-tools focus dired-rainbow material-theme conda pyvenv-auto disk-usage gnuplot dired-rsync popup-kill-ring elpy latex-preview-pane helm-swoop dired-hacks-utils evil-ediff multi use-package projectile rainbow-delimiters rainbow-mode vdiff ## sunrise-x-tabs sunrise-x-modeline sunrise-x-buttons spacemacs-theme rpn-calc powerline openwith neotree multiple-cursors monokai-pro-theme minimap magit indent-guide hydra helm gruvbox-theme evil ess dracula-theme company auto-complete auctex)))

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


(defun cpm/copy-buffer-file-name (&optional full)
  "Copy the current buffer file name to the clipboard.

With prefix argument FULL (C-u), copy the full absolute path.
Without prefix, copy the abbreviated path (with ~)."
  (interactive "P")
  (let ((file (buffer-file-name)))
    (unless file
      (user-error "Current buffer is not visiting a file"))
    (let* ((path (if full
                     (expand-file-name file)
                   (abbreviate-file-name file))))
      (kill-new path)
      (message "Copied: %s" path))))

(global-set-key (kbd "C-c p f") #'cpm/copy-buffer-file-name)

;(evil-set-initial-state 'sunrise-mode 'emacs)
(evil-set-initial-state 'image-mode 'emacs)
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
(setq using-unix-filesystems t)

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

;; (use-package org-bullets
;;   :ensure t
;;   :config
;;   (add-hook 'org-mode-hook (lambda () (org-bullets-mode))))

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
    (dired-rainbow-define image "#f66d9b" ("tiff" "tif" "cdr" "gif" "ico" "jpeg" "jpg" "png" "psd" "eps" "svg" "JPG" "webp" "emf" "wmf"))
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
      ;; Execute the command with the unique buffer name
      (async-shell-command command buffer-name)
      ;; Optionally, you might want to automatically display this buffer
      (display-buffer buffer-name))))


;; Bind the function to the key T
(define-key dired-mode-map "T" 'my-dired-copy-to-other-window)

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

(add-to-list 'default-frame-alist '(font . "Consolas 11"))

(setq dired-guess-shell-alist-user
      '(("\\.pdf\\'" "inkscape ? &" "gsview ? &" "acrobat ? &" "gimp ? &" "inkview ? &")
        ("\\.eps\\'" "inkscape ? &" "gsview ? &" "inkview ? &")
        ("\\.emf\\'" "inkscape ? &" "inkview ? &")
        ("\\.svg\\'" "inkscape ? &" "inkview ? &" "code ? &")
        ("\\.ps\\'" "inkscape ? &" "inkview ? &")
        ("\\.mp3\\'" "vlc ? &" "ffplay ? &")
        ("\\.mp4\\'" "vlc ? &" "ffplay ? &")
        ("\\.mkv\\'" "vlc ? &" "ffplay ? &")
        ("\\.avi\\'" "vlc ? &" "ffplay ? &")
        ("\\.png\\'" "gimp ? &")
        ("\\.jpg\\'" "gimp ? &")
        ("\\.JPG\\'" "gimp ? &")
        ("\\.plt\\'" "gnuplot ? &")
        ("\\.dem\\'" "gnuplot ? &")
        ("\\.gp\\'" "gnuplot ? &")
        ("\\.csv\\'" "notepad++ ? &" "gvim ? &")
        ("\\.dat\\'" "notepad++ ? &" "gvim ? &")
        ("\\.txt\\'" "notepad++ ? &" "gvim ? &")
        ("\\.zip\\'" "7zfm.exe ? &")
        ("\\.rar\\'" "7zfm.exe ? &")
        ("\\.tar\\'" "7zfm.exe ? &")
        ("\\.tar.xz\\'" "7zfm.exe ? &")
        ("\\.gz\\'" "7zfm.exe ? &")
        ("\\.tar.gz\\'" "7zfm.exe ? &")))

(defun dired-open-with-choice ()
  "Prompt for a program to open the current file in `dired` with a default choice and display all options."
  (interactive)
  (let* ((file (dired-get-file-for-visit))
         (file-url (concat "file:///" (replace-regexp-in-string "\\\\" "/" file))) ;; Convert path to VLC format
         (choices (cond
                   ((string-match "\\.pdf\\'" file) 
                    '("inkscape"
                      "inkview"
                      "gimp"                      
                      "gsview"
                      "Acrobat"))
                   ((string-match "\\.\\(dat\\|txt\\|nfo\\|f\\|f90\\)\\'" file) 
                    '("notepad++"
                      "gvim"
                      "code"))
                   ((string-match "\\.\\(mp4\\|mkv\\|mpg\\|avi\\|m4v\\)\\'" file) 
                    '("vlc"
                      "ffplay"))
                   ((string-match "\\.\\(ps\\|eps\\)\\'" file) 
                    '("inkscape"
                      "inkview"
                      "gsview"))
                   ((string-match "\\.\\(png\\|jpg\\|jpeg\\|bmp\\|gif\\|tif\\)\\'" file) 
                    '("ffplay"
                      "gimp-3"))
                   ((string-match "\\.\\(svg\\|emf\\|wmf\\)\\'" file) 
                    '("inkscape"))
                   ((string-match "\\.\\(plt\\|gp\\|dem\\)\\'" file) 
                    '("gnuplot"))
                   ((string-match "\\.\\(zip\\|7z\\|rar\\|tar.xz\\|tar.gz\\|.gz\\|.tar\\)\\'" file) 
                    '("7zFM"))
                   (t nil)))
         (default-program (car choices))
         (program (if choices
                      (helm-comp-read (format "Open with (default: %s): " default-program)
                                      choices
                                      :default default-program
                                      :must-match t)
                    (error "No associated programs for this file type"))))
    
    (when program
      (cond
       ((string-match "ffplay" program)
        (call-process-shell-command (concat "\"" program "\" " (shell-quote-argument file)) nil 0))

       ((string-match "vlc.exe" program)
        (call-process-shell-command (concat "\"" program "\"fff " file-url) nil 0))

       ;; Default behavior for other programs
       (t (start-process "dired-open" nil program file))))))
    
(define-key dired-mode-map (kbd "C-c o") 'dired-open-with-choice)

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

;; works with single file, no space
;; (defun copy-local-to-remote ()
;;   (interactive)

;; works with several files and spaces
(defun copy-local-to-remote ()
  "Copy all selected local files to a remote Tramp session."
  (interactive)
  (let* ((local-files (dired-get-marked-files))
         (remote-buffer (window-buffer (next-window)))
         (remote-dir (with-current-buffer remote-buffer default-directory))
         (tramp-info (tramp-dissect-file-name remote-dir))
         (remote-host (tramp-file-name-host tramp-info))
         (remote-user (tramp-file-name-user tramp-info)))
    (dolist (local-file local-files)
      (let* ((remote-file (concat (tramp-file-name-localname tramp-info)
                                  (file-name-nondirectory local-file))))
        (async-shell-command
         (format "pscp.exe \"%s\" %s@%s:\"%s\""
                 (expand-file-name local-file) remote-user remote-host remote-file))))))

;; works with several files and spaces
(defun copy-remote-to-local ()
  "Copy all selected remote files to the local system using `pscp.exe`."
  (interactive)
  (let* ((remote-files (dired-get-marked-files))
         (local-buffer (window-buffer (next-window)))
         (local-dir (file-name-as-directory (with-current-buffer local-buffer default-directory))))
    
    (select-window (next-window)) ; Switch to the Tramp window
    
    ;; Process each file separately to avoid pscp limitations
    (dolist (remote-file remote-files)
      (let* ((tramp-info (tramp-dissect-file-name remote-file))
             (remote-host (tramp-file-name-host tramp-info))
             (remote-user (tramp-file-name-user tramp-info))
             (remote-path (tramp-file-name-localname tramp-info))
             (local-file (concat local-dir (file-name-nondirectory remote-path))))
        (async-shell-command
         (format "pscp.exe -r \"%s@%s:%s\" \"%s\""
                 remote-user remote-host remote-path local-file))))
    
    (select-window (get-buffer-window local-buffer)))) ; Switch back to the local window

(global-set-key (kbd "C-x c") 'copy-file-remote-or-local)

(defun copy-files-remote-or-local ()
  "Copy selected files between remote and local and update Dired buffer."
  (interactive)
  (let* ((current-files (dired-get-marked-files)))
    (if (file-remote-p (car current-files))  ;; Check the first file for remote status
        (progn
          (copy-remote-to-local)
          (dired-revert))
      (progn
        (copy-local-to-remote)
        (dired-revert)))))

(global-set-key (kbd "C-x c") 'copy-files-remote-or-local)

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

(setq 
 org-default-notes-file "c:/users/bdulauroy/hpc.org"
 initial-buffer-choice  org-default-notes-file)

;; Open Dired with a specific directory at startup
(defun my/open-dired-at-startup ()
  (dired "c:/Users/bdulauroy/Downloads")) ; Replace "~/path/to/directory" with your desired directory

(add-hook 'emacs-startup-hook #'my/open-dired-at-startup)

(defun create-script-with-shebang-and-gnuplot (file-name)
  "Create a new script file with a specified shebang line and open in Emacs.
   Include a sample Gnuplot script in the file."
  (interactive "sEnter script name: ")
  (let* ((file-path (expand-file-name file-name))
         (language (completing-read "Choose shebang (Bash [b] | Python [p] | PBS [s] | Gnuplot [g] | R [r] | Texte [t]): "
                                    '(("b" "Bash") ("p" "Python") ("s" "PBS") ("g" "Gnuplot") ("r" "R") ("t" "Texte")  )))
         (shebang-line (cond ((string= language "b") "#!/bin/bash")
                             ((string= language "p") "#!/usr/bin/env python")
                             ((string= language "s") "#!/bin/bash\n#PBS -l nodes=1:ppn=2\n#PBS -q normal\n#PBS -N processing")
                             ((string= language "g") "#!/usr/bin/env gnuplot")
                             ((string= language "R") "#!/usr/bin/env Rscript")
                             ((string= language "t") "") ; no shebang for .txt
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

    (revert-buffer)  ; Refresh Dired buffe
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

(defun close-all-buffers ()
  (interactive)
  (mapc 'kill-buffer (buffer-list)))

(global-set-key (kbd "C-x C-k") 'close-all-buffers)

(setq visible-bell t)

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

;; (defun dired-send-to-remote ()
;;   "Send the selected file or folder to a remote Linux directory using `pscp.exe`.

(defun dired-send-to-remote ()
  "Send selected files or folders to a remote Linux directory using `pscp.exe`.
First, choose a root destination:
1. `/urrnfs01/bdulauro/` (default: Home)
2. `/TEMP/bdulauro/`
Then, optionally enter a subdirectory."
  (interactive)
  (let* ((pscp-path "C:/Program Files (x86)/PuTTY/pscp.exe") ;; Adjust if needed
         (files (dired-get-marked-files))  ;; Get all selected files
         (roots '(("Home" . "bdulauro@ausrichhpci03:/urrnfs01/bdulauro/")
                  ("TEMP" . "bdulauro@ausrichhpci03:/TEMP/bdulauro/")))
         (root-choice (assoc (completing-read "Select destination root: " (mapcar #'car roots) 
                                              nil t "Home") roots)) ;; Pre-filled "Home" as default
         (default-dest (cdr root-choice))
         (subdir (read-string (format "Subdirectory (default: %s): " default-dest) "")) ;; Optional subdir
         (destination-dir (concat default-dest (if (string-empty-p subdir) "" (concat subdir "/")))))

    ;; Loop over all selected files and send them one by one
    (dolist (file files)
      (let* ((file-name (file-name-nondirectory file)) ;; Extract filename
             (destination (concat destination-dir file-name))
             (pscp-command (format "\"%s\" \"%s\" \"%s\"" pscp-path file destination)))
        (message "Running: %s" pscp-command)  ;; Debug message
        (async-shell-command pscp-command)))))  ;; Run command asynchronously

;; Assign shortcut key in Dired mode (e.g., "C-c s")
;; (define-key dired-mode-map (kbd "C-c s") 'dired-send-to-remote)

(defun dired-receive-from-remote ()
  "Download selected files from a remote Linux directory using `pscp.exe`.
Files are saved to `c:/Users/bdulauroy/` by default, but you can specify a subdirectory."
  (interactive)
  (let* ((pscp-path "C:/Program Files (x86)/PuTTY/pscp.exe") ;; Adjust if needed
         (files (dired-get-marked-files))  ;; Get all selected remote files
         (default-local "c:/Users/bdulauroy/") ;; Default local directory
         (subdir (read-string (format "Subdirectory (default: %s): " default-local) "")) ;; Optional subdir
         (local-destination (concat default-local (if (string-empty-p subdir) "" (concat subdir "/")))))

    ;; Ensure we execute the command locally (not on the remote)
    (with-temp-buffer
      (dolist (file files)
        (let* ((remote-file (replace-regexp-in-string "^/psftp:[^:]+:" "" file)) ;; Extract remote path
               (remote-full-path (concat "bdulauro@ausrichhpci03:" remote-file)) ;; Add SCP remote user/host
               (local-file (concat local-destination (file-name-nondirectory remote-file)))
               (pscp-command (format "\"%s\" -r \"%s\" \"%s\"" pscp-path remote-full-path local-file)))
          (message "Running: %s" pscp-command)  ;; Debug message
          (call-process-shell-command pscp-command nil 0))))))

;; Assign shortcut key in Dired mode (e.g., "C-c r")
;; (define-key dired-mode-map (kbd "C-c r") 'dired-receive-from-remote)

(defun dired-transfer-auto ()
  "Automatically transfer selected files between local and remote using `pscp.exe`.
If the current Dired buffer is local, it uploads files to the remote.
If the current Dired buffer is remote (`psftp:`), it downloads files to the local machine."
  (interactive)
  (if (string-match "^/psftp:" default-directory)
      (progn
        (message "Remote directory detected. Downloading files...")
        (dired-receive-from-remote))  ;; Download from remote
    (progn
      (message "Local directory detected. Uploading files...")
      (dired-send-to-remote))))  ;; Upload to remote

;; Assign the unified command to a single keybinding
(define-key dired-mode-map (kbd "C-c s") 'dired-transfer-auto)

;;(setq image-dired-dir "c:/Users/bdulauroy/emacs-thumbnails/")
(setq image-dired-debug t)


(setq image-dired-thumb-size 256
      image-dired-thumb-width 256
      image-dired-thumb-height 256)


(setq doc-view-ghostscript-program "C:/Program Files/gs/gs9.23/bin/gswin64c.exe")  ;; Use Ghostscript
(setq doc-view-resolution 80)            ;; Lower resolution for faster loading
(setq doc-view-cache-directory "~/.emacs.d/docview-cache")
(setq doc-view-continuous t)
(setq auto-mode-alist
      (append '(("\\.ps\\'" . doc-view-mode)
                ("\\.eps\\'" . doc-view-mode))
              auto-mode-alist))

(define-key dired-mode-map (kbd "i") 'image-dired-dired-display-image)

(setq eshell-prompt-function
      (lambda ()
        (concat (propertize "| " 'face '(:foreground "magenta" :bold t)))))

(defun my/display-disk-space ()
  "Display free disk space using PowerShell."
  (interactive)
  (let ((output (shell-command-to-string "powershell -command \"Get-PSDrive -PSProvider FileSystem\"")))
    (with-output-to-temp-buffer "*Disk Space*"
      (princ output))))

(global-set-key (kbd "C-c C-d") 'my/display-disk-space)

(message "%s" custom-enabled-themes)

(defun compare-files-with-tool ()
  (interactive)
  (let* ((left-file (buffer-file-name (window-buffer (selected-window))))
         (right-file (buffer-file-name (window-buffer (next-window))))
         (tool (completing-read-default "Choose comparison tool: "
                                        '("tkdiff" "examdiff")
                                        nil t "tkdiff")))  ;; Force default UI
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

(global-set-key (kbd "C-x C-v") 'compare-files-with-gvim)

(defun dired-convert-svg-to-emf ()
  "Convert selected .svg files in Dired to .emf using Inkscape with a process limit."
  (interactive)
  (let ((files (dired-get-marked-files))
        (max-parallel 4))  ;; Adjust this number as needed
    (while files
      (let ((batch (seq-take files max-parallel)))  ;; Take a batch of `max-parallel` files
        (setq files (seq-drop files max-parallel))  ;; Remove processed files from list
        (dolist (file batch)
          (let ((output-file (concat (file-name-sans-extension file) ".emf")))
            (start-process "inkscape-convert" "*inkscape-output*" "inkscape" file "--export-type=emf" "--export-filename" output-file)))
        (sleep-for 1)))  ;; Small delay to avoid overloading system
    (message "All conversions completed!")))

(define-key dired-mode-map (kbd "C-c C-e") 'dired-convert-svg-to-emf)

(defun dired-open-inkview ()
  "Open all selected SVG files in Inkview from Dired."
  (interactive)
  (let* ((files (dired-get-marked-files))
         (svg-files (seq-filter (lambda (f) (string-match "\\.svg\\'" f)) files)))
    (if svg-files
        (apply #'start-process "inkview" nil "inkview" (mapcar #'convert-standard-filename svg-files))
      (message "No SVG files selected."))))

(define-key dired-mode-map (kbd "C-c d") 'dired-open-inkview)

(define-key dired-mode-map (kbd "* .") 'dired-mark-extension)

(defun my-dired-open-emf-as-png (file)
  "Convert FILE (EMF) to PNG using Inkscape and open the PNG."
  (let* ((png-file (concat (file-name-sans-extension file) ".png"))
         (convert-cmd
          (format "inkscape %S --export-type=png --export-filename=%S --export-area-drawing --export-dpi=150"
                  file png-file)))
    (unless (file-exists-p png-file)
      (shell-command convert-cmd))
    (if (file-exists-p png-file)
        (find-file png-file)
      (message "Conversion failed: %s" file))))

(with-eval-after-load 'dired
  (defun my/dired-ret ()
    "RET in Dired: EMF -> convert/open, otherwise open normally."
    (interactive)
    (let* ((file (dired-get-file-for-visit))
           (ext  (downcase (or (file-name-extension file) ""))))
      (if (string= ext "emf")
          (my-dired-open-emf-as-png file)
        (dired-find-file))))

  (evil-define-key 'normal dired-mode-map
    (kbd "RET") #'my/dired-ret
    (kbd "<return>") #'my/dired-ret))



;;; autotab.el --- Smart delimited-text preview via pandas -*- lexical-binding: t; -*-

;; Configure which Python to use.
;; - Linux/macOS: defaults to "python3"
;; - Windows: point to your conda env python.exe
(defcustom autotab-python-program
  (cond
   ((eq system-type 'windows-nt)
    "C:/Users/bdulauroy/AppData/Roaming/Mf3/envs/py312/python.exe")
   (t "python3"))
  "Python executable used by `autotab` and `autotab-plain`."
  :type 'string)

(defun autotab-plain ()
  "Smart preview of delimited text files using pandas (Python 3.12+).
Detects separator, skips non-numeric headers, shows 100 rows.
Displays both output and the generated Python script using pandas' default view."
  (interactive)
  (let* ((file (dired-get-file-for-visit))
         (output-buffer "*Text Preview*")
         (script-buffer "*Text Preview Script*")
         (script
"import sys
import pandas as pd
import re

filename = sys.argv[1]
with open(filename, 'r', encoding='utf-8', errors='ignore') as f:
    lines = f.readlines()

sample = ''.join(lines[:20])
candidate_separators = [',', ';', '\\t', '|']
sep_scores = {}

# Heuristic: consistent field count and low stddev suggests good separator
for sep in candidate_separators:
    pattern = re.escape(sep)
    counts = [len(re.split(pattern, line.strip())) for line in lines[:20] if line.strip()]
    if counts:
        std = pd.Series(counts).std()
        if std < 1.0 and max(counts) > 1:
            sep_scores[sep] = sum(counts)

if sep_scores:
    sep = max(sep_scores, key=sep_scores.get)
    engine = 'c'
else:
    sep = r'\\s+'
    engine = 'python'

def is_data_line(line):
    tokens = re.split(sep, line.strip())
    try:
        floats = [float(t) for t in tokens if t.strip()]
        return len(floats) >= 2
    except:
        return False

start_row = 0
for i, line in enumerate(lines[:20]):
    if is_data_line(line):
        start_row = i
        break

try:
    df = pd.read_csv(filename, sep=sep, header=None, skiprows=start_row, nrows=100, engine=engine)

    # Print using the default pandas view (without tabulate)
    print(df.head(100).to_string(index=False))
except Exception as e:
    print(f'Error reading file: {e}')
"))
    ;; Show script for inspection
    (with-current-buffer (get-buffer-create script-buffer)
      (read-only-mode -1)
      (erase-buffer)
      (insert script)
      (python-mode)
      (read-only-mode 1)
      (display-buffer script-buffer))
    ;; Run script and display output (capture stdout + stderr)
    (with-current-buffer (get-buffer-create output-buffer)
      (read-only-mode -1)
      (erase-buffer)
      (let ((exit-code
             (call-process autotab-python-program nil (list t t) nil "-c" script file)))
        (goto-char (point-min))
        (read-only-mode 1)
        (display-buffer output-buffer)
        (unless (= exit-code 0)
          (message "autotab-plain: Failed (exit %d). See %s for details."
                   exit-code output-buffer))))))

(defun autotab ()
  "Smart preview of delimited text files using pandas/tabulate (Python 3.12+).
Detects separator, skips non-numeric headers, shows 100 rows.
Displays both output and the generated Python script."
  (interactive)
  (let* ((file (dired-get-file-for-visit))
         (output-buffer "*Text Preview*")
         (script-buffer "*Text Preview Script*")
         (script
"import sys
import pandas as pd
import re

filename = sys.argv[1]
with open(filename, 'r', encoding='utf-8', errors='ignore') as f:
    lines = f.readlines()

sample = ''.join(lines[:20])
candidate_separators = [',', ';', '\\t', '|']
sep_scores = {}

# Heuristic: consistent field count and low stddev suggests good separator
for sep in candidate_separators:
    pattern = re.escape(sep)
    counts = [len(re.split(pattern, line.strip())) for line in lines[:20] if line.strip()]
    if counts:
        std = pd.Series(counts).std()
        if std < 1.0 and max(counts) > 1:
            sep_scores[sep] = sum(counts)

if sep_scores:
    sep = max(sep_scores, key=sep_scores.get)
    engine = 'c'
else:
    sep = r'\\s+'
    engine = 'python'

def is_data_line(line):
    tokens = re.split(sep, line.strip())
    try:
        floats = [float(t) for t in tokens if t.strip()]
        return len(floats) >= 2
    except:
        return False

start_row = 0
for i, line in enumerate(lines[:20]):
    if is_data_line(line):
        start_row = i
        break

try:
    df = pd.read_csv(filename, sep=sep, header=None, skiprows=start_row, nrows=100, engine=engine)
    try:
        from tabulate import tabulate
        print(tabulate(df.values.tolist(), headers=df.columns, tablefmt='grid'))
    except ImportError:
        print(df.to_string(index=False))
except Exception as e:
    print(f'Error reading file: {e}')
"))
    ;; Show script for inspection
    (with-current-buffer (get-buffer-create script-buffer)
      (read-only-mode -1)
      (erase-buffer)
      (insert script)
      (python-mode)
      (read-only-mode 1)
      (display-buffer script-buffer))
    ;; Run script and display output (capture stdout + stderr)
    (with-current-buffer (get-buffer-create output-buffer)
      (read-only-mode -1)
      (erase-buffer)
      (let ((exit-code
             (call-process autotab-python-program nil (list t t) nil "-c" script file)))
        (goto-char (point-min))
        (read-only-mode 1)
        (display-buffer output-buffer)
        (unless (= exit-code 0)
          (message "autotab: Failed (exit %d). See %s for details."
                   exit-code output-buffer))))))

(provide 'autotab)
;;; autotab.el ends here

(defvar autoplot-process nil
  "The last Gnuplot process started by autoplot.")

(defun autoplot-full (xcol ycols xlabel ylabel title terminal xrange-min xrange-max yrange-min yrange-max every-n)
  "Full-featured Gnuplot plotting function."
  (interactive
   (list
    (read-string "X column (default 1): " "1")
    (read-string "Y column(s) (e.g. 2 or 6 7 8): " "2")
    (read-string "X label: ")
    (read-string "Y label: ")
    (read-string "Title: ")
    (completing-read "Terminal (qt, x11, png, svg, dumb): " '("qt" "x11" "png" "svg" "dumb") nil t "qt")
    (read-string "X range min (Enter for auto): ")
    (read-string "X range max (Enter for auto): ")
    (read-string "Y range min (Enter for auto): ")
    (read-string "Y range max (Enter for auto): ")
    (read-string "Plot every N-th line (e.g. 10, default 1): " "1")))

  (let* ((files (dired-get-marked-files))
         (first-file (car files))
         (basename (if (= (length files) 1)
                       (file-name-base first-file)
                     "multi_autoplot"))
         (lines (with-temp-buffer
                  (insert-file-contents first-file nil 0 1000)
                  (split-string (buffer-string) "\n")))
         (separator
          (cond
           ((string-match-p "," (car lines)) "','")
           ((string-match-p ";" (car lines)) "';'")
           ((string-match-p "\t" (car lines)) "'\t'")
           (t "whitespace")))
         (skip-lines
          (number-to-string
           (or (cl-position-if
                (lambda (line)
                  (let* ((clean (replace-regexp-in-string "[ \t]+" " " line))
                         (parts (split-string clean))
                         (nums (cl-remove-if-not #'string-to-number parts)))
                    (>= (length nums) 2)))
                lines :end 20)
               0)))
         (script-file (expand-file-name (concat basename ".plt")
                                        (file-name-directory first-file)))
         (plot-lines '())
         (style-base 13)
         (style-max 21)
         (style-index 0))

    ;; Build plot lines
    (dolist (file files)
      (let ((label-base (replace-regexp-in-string "_" "\\\\_" (file-name-base file))))
        (dolist (ycol (split-string ycols))
          (setq style-index (1+ style-index))
          (let ((style-num (+ style-base (mod (1- style-index) (- style-max style-base +1)))))
            (push (format "'%s' using %s:%s every %s::%s w l ls %d lw 2 title \"%s col %s\""
                          (file-name-nondirectory file)
                          xcol ycol every-n skip-lines
                          style-num label-base ycol)
                  plot-lines)))))

    ;; Terminal setup
    (let* ((term-line
            (cond
             ((string= terminal "qt") "set term qt\nset termoption noenhanced")
             ((string= terminal "x11") "set term x11\nset termoption noenhanced")
             ((string= terminal "png") "set term png size 1000,600\nset termoption noenhanced")
             ((string= terminal "svg") "set term svg size 1000,600 font 'Arial,12'\nset termoption noenhanced")
             ((string= terminal "dumb") "set term dumb 120 40")
             (t "set term qt\nset termoption noenhanced")))
           (output-line
            (if (member terminal '("png" "svg"))
                (format "set output '%s.%s'" basename terminal)
              ""))
           (xrange-line (unless (and (string= xrange-min "") (string= xrange-max ""))
                          (format "set xrange [%s:%s]"
                                  (if (string= xrange-min "") "*" xrange-min)
                                  (if (string= xrange-max "") "*" xrange-max))))
           (yrange-line (unless (and (string= yrange-min "") (string= yrange-max ""))
                          (format "set yrange [%s:%s]"
                                  (if (string= yrange-min "") "*" yrange-min)
                                  (if (string= yrange-max "") "*" yrange-max))))
           (pause-line (if (member terminal '("qt" "x11")) "pause -1" ""))
           (script (format "
set datafile separator %s
%s
%s

# Base styles
set style line 1 lc rgb '#377eb8' pt 7 ps 1 lt 1 lw 3
set style line 2 lc rgb '#e41a1c' pt 7 ps 1 lt 1 lw 3
set style line 11 lc rgb '#808080' lt 1
set border 3 back ls 11
set tics nomirror
set style line 12 lc rgb '#808080' lt 0 lw 1
set key top right font \",13\" tc rgb '#606060'
set xtics font \",13\"
set ytics font \",13\"

# Color palette (13–21)
set style line 13 lw 2 lt 1 pt 7 lc rgb '#0072bd'
set style line 14 lw 2 lt 1 pt 7 lc rgb '#d95319'
set style line 15 lw 2 lt 1 pt 7 lc rgb '#edb120'
set style line 16 lw 2 lt 1 pt 7 lc rgb '#7e2f8e'
set style line 17 lw 2 lt 1 pt 7 lc rgb '#77ac30'
set style line 18 lw 2 lt 1 pt 7 lc rgb '#4dbeee'
set style line 19 lw 2 lt 1 pt 7 lc rgb '#c06c84'
set style line 20 lw 2 lt 1 pt 7 lc rgb '#7f4e34'
set style line 21 lw 2 lt 1 pt 7 lc rgb '#606060'

set xlabel '%s' font \",13\" tc rgb '#606060'
set ylabel '%s' font \",13\" tc rgb '#606060'
set title  '%s' font ',13' tc rgb '#606060'

%s
%s

plot \\
%s

%s"
                   separator term-line output-line
                   xlabel ylabel title
                   (or xrange-line "")
                   (or yrange-line "")
                   (mapconcat #'identity (reverse plot-lines) ",\\\n")
                   pause-line)))

      ;; Write and run
      (with-temp-file script-file
        (insert script))

      (if (string= terminal "dumb")
          (with-current-buffer (get-buffer-create "*autoplot*")
            (erase-buffer)
            (call-process "gnuplot" nil (current-buffer) nil script-file)
            (display-buffer (current-buffer)))
        (progn
          (when (process-live-p autoplot-process)
            (kill-process autoplot-process))
          (setq autoplot-process
                (start-process "autoplot-gnuplot" "*autoplot*" "gnuplot" script-file))
          (message "Plotted %d curve(s) using terminal '%s'." style-index terminal))))))

(defun autoplot-default ()
  "Quick autoplot with default values (qt terminal)."
  (interactive)
  (autoplot-full "1" "2" "" "" "" "qt" "" "" "" "" "1"))

(defun autoplot-dumb ()
  "Quick autoplot with default values using dumb terminal (ASCII plot in buffer)."
  (interactive)
  (autoplot-full "1" "2" "" "" "" "dumb" "" "" "" "" "1"))

(defun autoplot (&optional arg)
  "Main autoplot command.
Runs quick version by default, or full prompt if ARG is given (e.g. with C-u)."
  (interactive "P")
  (if arg
      (call-interactively #'autoplot-full)
    (autoplot-default)))

(global-set-key (kbd "C-c C-p") #'autoplot)
(global-set-key (kbd "C-c C-d") #'autoplot-dumb)

(defun excel-file-p (file)
  "Return non-nil if FILE looks like an Excel file."
  (let ((ext (downcase (file-name-extension file))))
    (member ext '("xlsx" "xls" "xlsm"))))

(defconst excel-viewer-python
  (if (eq system-type 'windows-nt)
      "C:/Users/bdulauroy/AppData/Roaming/Mf3/envs/py312/python.exe"
    "python3"))
(defun excel-viewer-tabular ()
  "Display the content of a selected Excel .xlsx file in Dired using Python, pandas, and tabulate."
  (interactive)
  (let* ((file (dired-get-file-for-visit))
         (output-buffer "*Excel View*")
         (script "
import sys
import pandas as pd
from tabulate import tabulate

def heuristic_header(df):
    # A simple heuristic to find the header row by looking for non-numeric values
    for i, row in df.iterrows():
        if row.apply(lambda x: isinstance(x, str) and len(x) > 0).any():
            return i
    return 0  # Default to 0 if no header row found

try:
    # Load the Excel file without a header
    df = pd.read_excel(sys.argv[1], header=None)

    # Use heuristic to find the first valid header row
    header_row = heuristic_header(df)

    # Set the header row and drop it from the data
    df.columns = df.iloc[header_row]
    df = df.drop(header_row)

    # Round the floating-point values to 2 decimal places
    df = df.round(2)

    # Display the first 50 rows with tabulate
    print(tabulate(df.head(50), headers='keys', tablefmt='grid'))
except Exception as e:
    print(f'Error: {e}')
"))
    (with-current-buffer (get-buffer-create output-buffer)
      (read-only-mode -1)
      (erase-buffer)
      (let ((exit-code (call-process excel-viewer-python nil t nil "-c" script file)))
        (if (= exit-code 0)
            (progn
              (goto-char (point-min))
              (read-only-mode 1)
              (display-buffer output-buffer))
          (message "Failed to read Excel file."))))))

(defun smart-tabular-view ()
  "Smart preview: Excel → excel-viewer-tabular, else → autotab."
  (interactive)
  (let ((file (dired-get-file-for-visit)))
    (if (excel-file-p file)
        (excel-viewer-tabular)
      (autotab))))

(defun smart-plain-view ()
  "Smart preview: Excel → excel-viewer, else → autotab-plain."
  (interactive)
  (let ((file (dired-get-file-for-visit)))
    (if (excel-file-p file)
        (excel-viewer)
      (autotab-plain))))

(global-set-key (kbd "C-c C-v") #'smart-tabular-view) ;; grid/tabulate view
(global-set-key (kbd "C-c C-x") #'smart-plain-view)   ;; plain pandas view

(require 'tramp-term)

(require 'go-mode)

(defun lyncburg ()
  "Open an Emacs shell and SSH with TTY to auslynchpci11."
  (interactive)
  (let ((buf (get-buffer-create "*ssh-auslynch*")))
    (shell buf)
    (with-current-buffer buf
      (goto-char (point-max))
      (insert "ssh -tt bdulauroy@auslynchpci11")
      (comint-send-input))))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)
   (shell . t)
   (emacs-lisp . t)
   (gnuplot . t)
   (R . t)
   (latex . t)
   ))

(defun run-update-bat ()
  "Run the update.bat script from the desktop."
  (interactive)
  (async-shell-command
   "\"C:/Users/bdulauroy/OneDrive - Framatome/Desktop/update.bat\""))

(defun timecard ()
  "Insert a summary Org table of clocked time by date and charge code at point, grouped by day,
and below it, insert a pivot table with charge code/description as rows and days as columns."
  (interactive)
  (require 'org)
  (require 'org-element)
  (require 'subr-x) ;; string utilities (Emacs 27+)

  (let ((date-width 10)
        (charge-width 28)
        (desc-width 23)
        (hours-width 7)
        (entries '()))

    ;; -----------------------------
    ;; Collect all clock entries
    ;; -----------------------------
    (org-element-map (org-element-parse-buffer) 'headline
      (lambda (headline)
        (let* ((heading (org-element-property :raw-value headline))
               (begin   (org-element-property :begin headline))
               (chargecode nil)
               (logbook (org-element-map headline 'drawer
                          (lambda (drawer)
                            (when (string= (org-element-property :drawer-name drawer) "LOGBOOK")
                              drawer))
                          nil t)))
          (save-excursion
            (goto-char begin)
            (setq chargecode (org-entry-get nil "CHARGECODE" t)))
          (when logbook
            (let ((logbook-start (org-element-property :begin logbook))
                  (logbook-end (org-element-property :end logbook)))
              (save-excursion
                (goto-char logbook-start)
                (while (re-search-forward
                        "^[ \t]*CLOCK: *\\[\\([0-9]+-[0-9]+-[0-9]+\\)[^]]*\\]--\\[\\([0-9]+-[0-9]+-[0-9]+\\)[^]]*\\].*=>[ \t]*\\([0-9]+\\):\\([0-9]+\\)"
                        logbook-end t)
                  (let* ((start-date (match-string 1))
                         (hours (string-to-number (match-string 3)))
                         (minutes (string-to-number (match-string 4)))
                         (duration (+ hours (/ minutes 60.0))))
                    (push (list :date start-date
                                :chargecode (or chargecode "Unspecified")
                                :project heading
                                :hours duration)
                          entries)))))))))

    ;; -----------------------------
    ;; Summary Table (by day) — unchanged logic except daily & grand totals
    ;; -----------------------------
    (let ((summary (make-hash-table :test 'equal)))
      ;; Accumulate hours by (date, chargecode, project)
      (dolist (e entries)
        (let* ((date (plist-get e :date))
               (charge (plist-get e :chargecode))
               (project (plist-get e :project))
               (key     (list date charge project)))
          (puthash key (+ (gethash key summary 0) (plist-get e :hours)) summary)))

      ;; Flatten to list and sort by date, then charge, then project
      (let (lines)
        (maphash
         (lambda (key hours)
           (pcase-let ((`(,date ,charge ,project) key))
             (push (list date charge project hours) lines)))
         summary)
        (setq lines (sort lines
                          (lambda (a b)
                            (let ((da (car a)) (db (car b)))
                              (if (string= da db)
                                  (let ((ca (cadr a)) (cb (cadr b)))
                                    (if (string= ca cb)
                                        (string< (nth 2 a) (nth 2 b))
                                      (string< ca cb)))
                                (string< da db))))))
        (let ((table-lines '())
              (last-date nil)
              (day-total 0.0)
              (grand-total 0.0))

          ;; Header
          (push (format "| %-10s | %-28s | %-23s | %-7s |"
                        "Date" "Charge Code" "Description" "Hours") table-lines)
          (push (format "|-%s-+-%s-+-%s-+-%s-|"
                        (make-string date-width ?-)
                        (make-string charge-width ?-)
                        (make-string desc-width ?-)
                        (make-string hours-width ?-))
                table-lines)

          ;; Rows
          (dolist (line lines)
            (let ((date (nth 0 line))
                  (charge (nth 1 line))
                  (project (nth 2 line))
                  (hours (nth 3 line)))
              (when (and last-date (not (string= date last-date)))
                ;; Insert subtotal for the previous day, then a blank row
                (push (format "| %-10s | %-28s | %-23s | %7.2f |"
                              "" "TOTAL" "" day-total)
                      table-lines)
                (push "|" table-lines) ;; blank row after day total
                (setq day-total 0.0))
              (setq last-date date)
              (setq day-total   (+ day-total hours))
              (setq grand-total (+ grand-total hours))
              (push (format "| %-10s | %-28s | %-23s | %7.2f |"
                            date charge project hours)
                    table-lines)))

          ;; Final day's total
          (push (format "| %-10s | %-28s | %-23s | %7.2f |"
                        "" "TOTAL" "" day-total)
                table-lines)

          ;; Footer separator + Grand TOTAL
          (push (format "|-%s-+-%s-+-%s-+-%s-|"
                        (make-string date-width ?-)
                        (make-string charge-width ?-)
                        (make-string desc-width ?-)
                        (make-string hours-width ?-))
                table-lines)
          (push (format "| %-10s | %-28s | %-23s | %7.2f |"
                        "TOTAL" "" "" grand-total)
                table-lines)

          ;; Insert and align
          (let ((start (point)))
            (insert (mapconcat #'identity (nreverse table-lines) "\n"))
            (insert "\n\n")
            (save-excursion
              (goto-char start)
              (org-table-align))))))

    ;; -----------------------------
    ;; Pivot Table (day-by-day with column TOTALS row)
    ;; -----------------------------
    (let* ((dates (sort (delete-dups (mapcar (lambda (e) (plist-get e :date)) entries)) #'string<))
           (row-keys (delete-dups
                      (mapcar (lambda (e) (list (plist-get e :chargecode)
                                                (plist-get e :project)))
                              entries)))
           (pivot (make-hash-table :test 'equal)))
      ;; Build pivot hash
      (dolist (e entries)
        (let* ((charge (plist-get e :chargecode))
               (project (plist-get e :project))
               (date    (plist-get e :date))
               (key     (list charge project date)))
          (puthash key (+ (gethash key pivot 0) (plist-get e :hours)) pivot)))

      ;; Render pivot with bottom totals row
      (let ((table-lines '()))
        ;; Header
        (push (concat
               (format "| %-28s | %-23s |" "Charge Code" "Description")
               (mapconcat (lambda (d) (format " %-10s |" d)) dates "")
               "   Total |")
              table-lines)
        ;; Separator
        (push (concat
               (format "|-%s-+-%s-+" (make-string charge-width ?-) (make-string desc-width ?-))
               (mapconcat (lambda (_d) (format "-%s-+" (make-string date-width ?-))) dates "")
               (format "-%s-|" (make-string hours-width ?-)))
              table-lines)
        ;; Data rows
        (dolist (row row-keys)
          (let* ((charge (nth 0 row))
                 (project (nth 1 row))
                 (row-hours '())
                 (total 0.0))
            (dolist (d dates)
              (let ((h (gethash (list charge project d) pivot 0)))
                (push h row-hours)
                (setq total (+ total h))))
            (push (format "| %-28s | %-23s |%s %7.2f |"
                          charge
                          project
                          (mapconcat (lambda (h) (format " %10.2f |" h)) (nreverse row-hours) "")
                          total)
                  table-lines)))

        ;; Column totals
        (let* ((col-sums
                (mapcar (lambda (d)
                          (let ((s 0.0))
                            (dolist (row row-keys s)
                              (let ((charge  (nth 0 row))
                                    (project (nth 1 row)))
                                (setq s (+ s (gethash (list charge project d) pivot 0)))))))
                        dates))
               (grand-total (apply #'+ col-sums)))
          ;; Optional separator before totals row
          (push (concat
                 (format "|-%s-+-%s-+" (make-string charge-width ?-) (make-string desc-width ?-))
                 (mapconcat (lambda (_d) (format "-%s-+" (make-string date-width ?-))) dates "")
                 (format "-%s-|" (make-string hours-width ?-)))
                table-lines)
          ;; TOTAL row at bottom
          (push (format "| %-28s | %-23s |%s %7.2f |"
                        "TOTAL" ""
                        (mapconcat (lambda (h) (format " %10.2f |" h)) col-sums "")
                        grand-total)
                table-lines))

        ;; Insert and align
        (let ((start (point)))
          (insert (mapconcat #'identity (nreverse table-lines) "\n"))
          (insert "\n")
          (save-excursion
            (goto-char start)
            (org-table-align)))))))

;;(require 'apdl-mode)
;; Auto-load apdl-mode for additional file types
;(add-to-list 'auto-mode-alist '("\\.dat\\'" . apdl-mode))
;(add-to-list 'auto-mode-alist '("\\.inp\\'" . apdl-mode))
;(add-to-list 'auto-mode-alist '("\\.mac\\'" . apdl-mode))

;;; --- Content-based APDL detection for .dat/.inp/.mac ---
;;(require 'apdl-mode) ;; ensure the mode is available

(defun my-apdl--apdl-extension-p (file)
  "Return non-nil if FILE has extension dat/inp/mac (case-insensitive)."
  (when file
    (let ((ext (downcase (file-name-extension file ""))))
      (member ext '("dat" "inp" "mac")))))

(defun my-apdl--looks-like-apdl-p ()
  "Return non-nil if the current buffer looks like an APDL input deck.
Scans only the first 30 lines for /TITLE, /PREP7, /SOLU, /POST1, or /POST26."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (let ((case-fold-search t))          ; be forgiving about case
        (let ((end (progn (forward-line 30) (point))))
          (goto-char (point-min))
          (re-search-forward
           ;; ^\s*/KEYWORD  — allow optional leading spaces before the slash
           "^\\s*/\\(TITLE\\|PREP7\\|SOLU\\|POST1\\|POST26\\)\\b"
           end t))))))

(defun my-apdl-maybe-enable-apdl-mode ()
  "Enable `apdl-mode' for .dat/.inp/.mac files that look like APDL."
  (when (and (my-apdl--apdl-extension-p buffer-file-name)
             (my-apdl--looks-like-apdl-p))
    (apdl-mode)))

;; Run the detector whenever a file is visited
(add-hook 'find-file-hook #'my-apdl-maybe-enable-apdl-mode)
;;; --------------------------------------------------------

;; disable inline compression for TRAMP
(with-eval-after-load 'tramp
  (setq tramp-inline-compress-start-size nil))

