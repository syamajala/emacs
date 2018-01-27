(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("c9fa45acd59564778b031178375261dbdc9259c9781c86e64c937ded3d8132e7" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" default)))
 '(elpy-shell-echo-input nil)
 '(ffap-machine-p-known (quote reject))
 '(global-linum-mode t)
 '(helm-dash-browser-func (quote w3m))
 '(helm-make-build-dir "/build")
 '(helm-swoop-split-with-multiple-windows t)
 '(linum-format "%d ")
 '(linum-highlight-in-all-buffersp nil)
 '(org-html-htmlize-output-type (quote css))
 '(org-org-htmlized-css-url "static/worg.css")
 '(package-selected-packages
   (quote
    (cl-lib ess csv-mode ace-window dumb-jump helm-descbinds vue-mode nodejs-repl js2-mode company-tern w3m helm-dash web-mode ws-butler window-number window-layout which-key test-simple swiper stickyfunc-enhance sr-speedbar smartparens smart-mode-line rainbow-delimiters pythonic malinka magit lua-mode loc-changes load-relative list-utils latex-preview-pane irony-eldoc htmlize hlinum helm-themes helm-swoop helm-projectile helm-make helm-ls-git helm-flycheck helm-ag google-c-style flycheck-irony elpy dtrt-indent cython-mode company-irony-c-headers company-irony cmake-ide cmake-font-lock clean-aindent-mode clang-format browse-kill-ring auctex)))
 '(rm-blacklist
   (quote
    (" hl-p" " SP" " Abbrev" " FA" " hs" " Helm" " wb" " WK" " yas" " company" " Irony" " ElDoc" " FlyC")))
 '(safe-local-variable-values
   (quote
    ((irony-additional-clang-options "-std=c++14")
     (cmake-ide-build-dir . "/home/seshu/dev/bioswarm++/build")
     (cmake-ide-build-dir . "/home/seshu/dev/cppweb/build")
     (cmake-ide-build-dir . "/home/seshu/dev/NumericalMethods/build"))))
 '(sml/vc-mode-show-backend t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ediff-even-diff-Ancestor ((t (:background "Grey" :foreground "black"))))
 '(ediff-even-diff-B ((t (:background "Grey" :foreground "black"))))
 '(ediff-fine-diff-A ((t (:background "red" :foreground "black"))))
 '(ediff-fine-diff-B ((t (:background "green" :foreground "black"))))
 '(ediff-fine-diff-C ((t (:background "yellow" :foreground "black"))))
 '(ediff-odd-diff-A ((t (:background "Grey" :foreground "black"))))
 '(ediff-odd-diff-C ((t (:background "Grey" :foreground "black"))))
 '(helm-grep-file ((t (:foreground "magenta" :underline t))))
 '(helm-selection ((t (:background "brightblack" :foreground "brightyellow"))))
 '(helm-source-header ((t (:background "color-235" :foreground "white" :weight bold :height 1.3 :family "Sans Serif"))))
 '(helm-swoop-target-line-face ((t (:background "yellow" :foreground "#222222"))))
 '(helm-swoop-target-word-face ((t (:background "magenta" :foreground "#ffffff"))))
 '(linum-highlight-face ((t (:inherit default :background "color-235" :foreground "brightred"))))
 '(mode-line ((t (:background "color-235" :inverse-video nil))))
 '(mode-line-inactive ((t (:background "color-235" :inverse-video nil))))
 '(rainbow-delimiters-depth-2-face ((t (:foreground "blue"))))
 '(rainbow-delimiters-depth-4-face ((t (:foreground "green"))))
 '(rainbow-delimiters-depth-5-face ((t (:foreground "magenta"))))
 '(rainbow-delimiters-depth-7-face ((t (:foreground "cyan"))))
 '(region ((t (:background "brightblack")))))

;; no menu or welcome
(menu-bar-mode -1)
(tool-bar-mode -1)
(setq inhibit-startup-message t)

(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list
   'package-archives
   '("melpa" . "http://melpa.org/packages/")
   t)
    (package-initialize))

;; custom theme
(load-theme 'seshu)

;; window switching
(global-set-key (kbd "C-c l") 'windmove-left)
(global-set-key (kbd "C-c r") 'windmove-right)
(global-set-key (kbd "C-c u") 'windmove-up)
(global-set-key (kbd "C-c d") 'windmove-down)

;; window number
(require 'window-number)
(window-number-mode 1)
(window-number-meta-mode 1)

;; hlinum
(require 'hlinum)
(hlinum-activate)
(setq linum-format "%d ")
(global-linum-mode 1)

;; lua
(autoload 'lua-mode "lua-mode" "Lua editing mode." t)
(add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
(add-to-list 'interpreter-mode-alist '("lua" . lua-mode))

;; gdb
(setq gdb-many-windows t
      gdb-show-main t)

;; sml
(setq sml/theme 'respectful)
(sml/setup)

;; x10
(add-to-list 'auto-mode-alist '("\\.x10\\'" . java-mode))

;; cuda
(add-to-list 'auto-mode-alist '("\\.cu\\'" . c++-mode))

;; c-style
(setq c-default-style "bsd")
(setq-default c-basic-offset 2)

;; rtags
(require 'rtags)
(require 'company-rtags)
(require 'flycheck-rtags)
(require 'helm-rtags)

(setq rtags-autostart-diagnostics t)
(rtags-diagnostics)
(setq rtags-completions-enabled t)
(setq rtags-display-result-backend 'helm)
(rtags-enable-standard-keybindings)

;; cmake-ide
(cmake-ide-setup)

;; malinka
;; (require 'malinka)
;; (add-hook 'c-mode-common-hook 'malinka-mode)

;; cmake
(require 'cmake-mode)

(setq auto-mode-alist
      (append
       '(("CMakeLists\\.txt\\'" . cmake-mode))
       '(("\\.cmake\\'" . cmake-mode))
       auto-mode-alist))

;; cmake-font-lock
(add-hook 'cmake-mode-hook 'cmake-font-lock-activate)

;; irony
(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'c-mode-hook 'irony-mode)
(add-hook 'objc-mode-hook 'irony-mode)

(defun my-irony-mode-hook ()
  (define-key irony-mode-map [remap completion-at-point]
    'irony-completion-at-point-async)
  (define-key irony-mode-map [remap complete-symbol]
    'irony-completion-at-point-async))

(add-hook 'irony-mode-hook 'my-irony-mode-hook)
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
(add-hook 'irony-mode-hook 'company-irony-setup-begin-commands)

;; company
(require 'company-irony-c-headers)
(add-hook 'after-init-hook 'global-company-mode)

(setq company-idle-delay 0)

(define-key c-mode-map [(tab)] 'company-complete)
(define-key c++-mode-map [(tab)] 'company-complete)
(setq company-backends (delete 'company-semantic company-backends))
(eval-after-load 'company
  '(add-to-list
    'company-backends '(company-irony-c-headers company-irony company-rtags company-yasnippet company-tern)))

;; flycheck-mode
(add-hook 'c++-mode-hook 'flycheck-mode)
(add-hook 'c-mode-hook 'flycheck-mode)
(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup))

;; eldoc
(add-hook 'irony-mode-hook 'irony-eldoc)

;; whitespace
;; (global-set-key (kbd "C-c w") 'whitespace-mode)
(add-hook 'prog-mode-hook (lambda () (interactive) (setq show-trailing-whitespace 1)))
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;; clean-aindent
(require 'clean-aindent-mode)
(add-hook 'prog-mode-hook 'clean-aindent-mode)
(define-key global-map (kbd "RET") 'newline-and-indent)

;; smartparens
(require 'smartparens-config)
(show-smartparens-global-mode +1)
(smartparens-global-mode 1)

;; when you press RET, the curly braces automatically
;; add another newline
(sp-with-modes '(c-mode c++-mode)
  (sp-local-pair "{" nil :post-handlers '(("||\n[i]" "RET")))
  (sp-local-pair "/*" "*/" :post-handlers '((" | " "SPC")
                                            ("* ||\n[i]" "RET"))))

;; magit
(global-set-key (kbd "C-x g") 'magit-status)

;; helm
(require 'helm)
(require 'helm-config)
(require 'helm-grep)

;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebihnd tab to do persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

(define-key helm-grep-mode-map (kbd "<return>")  'helm-grep-mode-jump-other-window)
(define-key helm-grep-mode-map (kbd "n")  'helm-grep-mode-jump-other-window-forward)
(define-key helm-grep-mode-map (kbd "p")  'helm-grep-mode-jump-other-window-backward)

(when (executable-find "curl")
  (setq helm-google-suggest-use-curl-p t))

(setq
 helm-scroll-amount 4 ; scroll 4 lines other window using M-<next>/M-<prior>
 helm-ff-search-library-in-sexp t ; search for library in `require' and `declare-function' sexp.
 helm-split-window-in-side-p t ;; open helm buffer inside current window, not occupy whole other window
 helm-candidate-number-limit 500 ; limit the number of displayed canidates
 helm-ff-file-name-history-use-recentf t
 helm-move-to-line-cycle-in-source t ; move to end or beginning of source when reaching top or bottom of source.
 helm-buffers-fuzzy-matching t ; fuzzy matching buffer names when non-nil
                                        ; useful in helm-mini that lists buffers
 helm-M-x-fuzzy-match t
 ;; helm-follow-mode-persistent t
 ;; helm-ag-use-temp-buffer t
 ;; helm-mode-fuzzy-match t
 ;; helm-completion-in-region-fuzzy-match t
 )

(add-to-list 'helm-sources-using-default-as-input 'helm-source-man-pages)

(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-h SPC") 'helm-all-mark-rings)
(global-set-key (kbd "C-c h o") 'helm-occur)

(global-set-key (kbd "C-c h C-c w") 'helm-wikipedia-suggest)

(global-set-key (kbd "C-c h x") 'helm-register)
;; (global-set-key (kbd "C-x r j") 'jump-to-register)

(define-key 'help-command (kbd "C-f") 'helm-apropos)
(define-key 'help-command (kbd "r") 'helm-info-emacs)
(define-key 'help-command (kbd "C-l") 'helm-locate-library)

;; use helm to list eshell history
(add-hook 'eshell-mode-hook
          #'(lambda ()
              (define-key eshell-mode-map (kbd "M-l")  'helm-eshell-history)))

;;; Save current position to mark ring
(add-hook 'helm-goto-line-before-hook 'helm-save-current-pos-to-mark-ring)

;; show minibuffer history with Helm
(define-key minibuffer-local-map (kbd "M-p") 'helm-minibuffer-history)
(define-key minibuffer-local-map (kbd "M-n") 'helm-minibuffer-history)

(define-key global-map [remap find-tag] 'helm-etags-select)

(define-key global-map [remap list-buffers] 'helm-buffers-list)

(helm-autoresize-mode 1)
(helm-mode 1)

;; helm-swoop
;; Locate the helm-swoop folder to your path
(require 'helm-swoop)

;; Change the keybinds to whatever you like :)
(global-set-key (kbd "C-c h o") 'helm-swoop)
(global-set-key (kbd "C-c t") 'helm-multi-swoop-all)

;; When doing isearch, hand the word over to helm-swoop
(define-key isearch-mode-map (kbd "M-i") 'helm-swoop-from-isearch)

;; From helm-swoop to helm-multi-swoop-all
(define-key helm-swoop-map (kbd "M-i") 'helm-multi-swoop-all-from-helm-swoop)

;; Save buffer when helm-multi-swoop-edit complete
(setq helm-multi-swoop-edit-save t)

;; If this value is t, split window inside the current window
(setq helm-swoop-split-with-multiple-windows t)

;; Split direcion. 'split-window-vertically or 'split-window-horizontally
(setq helm-swoop-split-direction 'split-window-vertically)

;; If nil, you can slightly boost invoke speed in exchange for text color
;;(setq helm-swoop-speed-or-color t)

;; helm-flycheck
(require 'helm-flycheck)

;; helm-dash
(setq helm-dash-common-docsets '("Python 2" "C++" "HTML" "CSS" "JavaScript" "VueJS"))

;; projectile
(projectile-global-mode)
(setq projectile-completion-system 'helm)
(helm-projectile-on)

;; hs-minor-mode
(add-hook 'c-mode-common-hook 'hs-minor-mode)
(add-hook 'python-mode-hook 'hs-minor-mode)

;; stickyfunc-enhance
(add-to-list 'semantic-default-submodes 'global-semantic-stickyfunc-mode)
(setq semantic-default-submodes (delete 'semanticdb semantic-default-submodes))
(semantic-mode 1)
(require 'stickyfunc-enhance)
(global-semantic-stickyfunc-mode)

;; dtrt-indent
(require 'dtrt-indent)
(dtrt-indent-mode 1)
(setq dtrt-indent-verbosity 0)

;; ws-butler
(require 'ws-butler)
;; (add-hook 'c-mode-common-hook 'ws-butler-mode)
(ws-butler-global-mode)

;; which-key
(require 'which-key)
(which-key-mode)

;; yasnippet
(require 'yasnippet)
(add-to-list 'yas-snippet-dirs "~/.emacs.d/yasnippet-snippets")
(yas-reload-all)
(add-hook 'c-mode-common-hook 'yas-minor-mode)
(add-hook 'python-mode 'yas-minor-mode)
;; (yas-global-mode 1)

;; python
(package-initialize)
(elpy-enable)
;; python2
;; (setq python-shell-interpreter "ipython2")
;; (setq elpy-rpc-python-command "python2")
;;python3
;;(elpy-use-ipython)
(setq python-shell-interpreter "ipython" python-shell-interpreter-args "--simple-prompt --pprint")

;; rainbow delimiters
(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;; speedbar
(require 'sr-speedbar)
(global-set-key (kbd "C-c s") 'sr-speedbar-toggle)

;; web-mode
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(define-key web-mode-map (kbd "C-c @ C-c") 'web-mode-fold-or-unfold)

;; cuda
(add-to-list 'auto-mode-alist '("\\.vue\\'" . vue-mode))

;; dumb-jump
(dumb-jump-mode)

;; ace-window
(global-set-key (kbd "M-p") 'ace-window)

;; ess-site
(require 'ess-site)

;; gc-cons
(defun my-minibuffer-setup-hook ()
  (setq gc-cons-threshold most-positive-fixnum))

(defun my-minibuffer-exit-hook ()
  (setq gc-cons-threshold 800000))

(add-hook 'minibuffer-setup-hook #'my-minibuffer-setup-hook)
(add-hook 'minibuffer-exit-hook #'my-minibuffer-exit-hook)

;; backups
(setq
 vc-make-backup-files t
 backup-by-copying t
 backup-directory-alist
 '(("." . "~/.emacs.d/backups/per-save"))
 delete-old-versions t
 kept-new-versions 2
 kept-old-versions 2
 version-control t)

(defun force-backup-of-buffer ()
  ;; Make a special "per session" backup at the first save of each
  ;; emacs session.
  (when (not buffer-backed-up)
    ;; Override the default parameters for per-session backups.
    (let ((backup-directory-alist '(("" . "~/.emacs.d/backups/per-session")))
          (kept-new-versions 2))
      (backup-buffer)))
  ;; Make a "per save" backup on each save.  The first save results in
  ;; both a per-session and a per-save backup, to keep the numbering
  ;; of per-save backups consistent.
  (let ((buffer-backed-up nil))
          (backup-buffer)))

(add-hook 'before-save-hook  'force-backup-of-buffer)

(message "Deleting old backup files...")
(let ((week (* 60 60 24 7))
      (current (float-time (current-time))))
  (dolist (file (directory-files temporary-file-directory t))
    (when (and (backup-file-name-p file)
               (> (- current (float-time (fifth (file-attributes file))))
                  week))
      (message "%s" file)
      (delete-file file))))
