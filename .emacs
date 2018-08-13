(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")
                         ("ELPA" . "http://tromey.com/elpa/") 
                         ("SC"  . "http://joseito.republika.pl/sunrise-commander/")
                         ("melpa" . "https://melpa.org/packages/")))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(compilation-message-face (quote default))
 '(custom-safe-themes
   (quote
    ("c3d4af771cbe0501d5a865656802788a9a0ff9cf10a7df704ec8b8ef69017c68" "eea01f540a0f3bc7c755410ea146943688c4e29bea74a29568635670ab22f9bc" "6ee6f99dc6219b65f67e04149c79ea316ca4bcd769a9e904030d38908fd7ccf9" "a4c9e536d86666d4494ef7f43c84807162d9bd29b0dfd39bdf2c3d845dcc7b2e" "e9460a84d876da407d9e6accf9ceba453e2f86f8b86076f37c08ad155de8223c" "3629b62a41f2e5f84006ff14a2247e679745896b5eaa1d5bcfbc904a3441b0cd" default)))
 '(fci-rule-color "#3C3D37")
 '(highlight-changes-colors (quote ("#FD5FF0" "#AE81FF")))
 '(highlight-tail-colors
   (quote
    (("#3C3D37" . 0)
     ("#679A01" . 20)
     ("#4BBEAE" . 30)
     ("#1DB4D0" . 50)
     ("#9A8F21" . 60)
     ("#A75B00" . 70)
     ("#F309DF" . 85)
     ("#3C3D37" . 100))))
 '(magit-diff-use-overlays nil)
 '(pos-tip-background-color "#FFFACE")
 '(pos-tip-foreground-color "#272822")
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#F92672")
     (40 . "#CF4F1F")
     (60 . "#C26C0F")
     (80 . "#E6DB74")
     (100 . "#AB8C00")
     (120 . "#A18F00")
     (140 . "#989200")
     (160 . "#8E9500")
     (180 . "#A6E22E")
     (200 . "#729A1E")
     (220 . "#609C3C")
     (240 . "#4E9D5B")
     (260 . "#3C9F79")
     (280 . "#A1EFE4")
     (300 . "#299BA6")
     (320 . "#2896B5")
     (340 . "#2790C3")
     (360 . "#66D9EF"))))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list
   (unspecified "#272822" "#3C3D37" "#F70057" "#F92672" "#86C30D" "#A6E22E" "#BEB244" "#E6DB74" "#40CAE4" "#66D9EF" "#FB35EA" "#FD5FF0" "#74DBCD" "#A1EFE4" "#F8F8F2" "#F8F8F0")))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

 
;; start of my config

;; fix a bug in emacs 25 and python https://github.com/syl20bnr/spacemacs/issues/8797
(setq python-shell-completion-native-enable nil)

;;:(add-to-list 'load-path "C:\\Users\\bdulauroy\\Appdata\Roaming\\.emacs.d\\neotree-20170522.758")
(set-default-font "Consolas 10")
(setq-default line-spacing 1)
(package-initialize)
(add-to-list 'custom-theme-load-path "C:\\Users\\bdulauroy\\Documents\\Personnal\\Apps\\emacs25\\share\\emacs\\25.3\\site-lisp\\monokai-alt-theme-20170630.1348")
(add-to-list 'custom-theme-load-path "C:\\Users\\bdulauroy\\Documents\\Personnal\\Apps\\emacs25\\share\\emacs\\25.3\\site-lisp\\color-theme-molokai-0.1")
(add-to-list 'custom-theme-load-path "C:\\Users\\bdulauroy\\Documents\\Personnal\\Apps\\emacs25\\share\\emacs\\25.3\\site-lisp\\monokai-theme-20180402.221")

;;(load-theme 'monokai-alt)
(load-theme 'monokai)

(tool-bar-mode -1)
(set-default 'truncate-lines t)
(setq inhibit-startup-screen t)

(require 'evil)
(evil-mode 1)

(require 'powerline)
(powerline-default-theme)

(require 'neotree)
(global-set-key [f8] 'neotree-toggle)

;;(require 'minimap)
;;(global-set-key [f9] 'minimap-mode)

;; (global-set-key (kbd "C-x f") 'fiplr-find-file)

(require 'indent-guide)
(indent-guide-global-mode)

(require 'auto-complete)
;;(ac-config-default)
(eval-after-load 'auto-complete '(global-auto-complete-mode 1))

(global-prettify-symbols-mode +1)

;;(define-globalized-minor-mode my-global-rainbow-mode rainbow-mode
;;  (lambda () (rainbow-mode 1)))

;;(my-global-rainbow-mode 1)

;;(add-hook 'foo-mode-hook #'rainbow-delimiters-mode)
;;(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

(dolist (pattern '("\\.gnuplot\\'" "\\.plt\\'")) (add-to-list 'auto-mode-alist (cons pattern 'gnuplot-mode)))

;; assuming you extracted the files on drive "c:"
;; for example: "c:\\emacs-24.5\\share\\emacs\\24.5\\site-lisp\\ansys-mode"

;; .mac is the macro suffix of ANSYS i. e. these macros can be called
;; in the ANSYS command prompt like a regular ANSYS function (without
;; the suffix .mac)
(add-to-list 'auto-mode-alist '("\\.mac\\'" . ansys-mode))
;; .dat and .inp are WorkBench's solver input file suffixes
(add-to-list 'auto-mode-alist '("\\.dat\\'" . ansys-mode))
(add-to-list 'auto-mode-alist '("\\.inp\\'" . ansys-mode))
;; .anf is the suffix for "ANSYS Neutral" files which include mostly
;;  gometric data but also some APDL snippets.
(add-to-list 'auto-mode-alist '("\\.anf$" . ansys-mode))

(autoload 'ansys-mode "ansys-mode" nil t)

(add-hook 'neotree-mode-hook
            (lambda ()
              (define-key evil-normal-state-local-map (kbd "TAB") 'neotree-enter)
              (define-key evil-normal-state-local-map (kbd "SPC") 'neotree-quick-look)
              (define-key evil-normal-state-local-map (kbd "q") 'neotree-hide)
              (define-key evil-normal-state-local-map (kbd "RET") 'neotree-enter)))
(setq-default neo-show-hidden-files t)

(recentf-mode 1)
(setq recentf-max-menu-items 25)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)
;; enable minimap at startup
;; (minimap-mode 1)
;;(latex-preview-pane-enable)
(require 'rpn-calc)
;;(add-to-list 'default-frame-alist '(height . 60))
;;(add-to-list 'default-frame-alist '(width . 100))

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
;;'(font . "DejaVu Sans Mono-11"))
;;'(font . "Roboto Mono for Powerline-11"))

(require 'multiple-cursors)
(global-set-key (kbd "C-x j") 'mc/edit-lines)

;; Set Frame width/height (can be set in ~/.Xdefaults too)
'(setq default-frame-alist
'((top . 25) (left . 275) (width . 100) (height . 60)))

