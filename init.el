(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(smart-mode-line-respectful))
 '(custom-safe-themes
   '("d2b80dd4995f33bb2b720e886d2f2def9ab1a5e82691a2f5b99d6811cc194028" "288482f5c627c1fe5a1d26fcc17ec6ca8837f36bf940db809895bf3f8e2e4edd" "c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "c12562436a8beba7b28fd8acba63d13851e30088c4a954db050e1c767cfb36f6" "c9fa45acd59564778b031178375261dbdc9259c9781c86e64c937ded3d8132e7" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" default))
 '(package-selected-packages
   '(smart-mode-line-atom-one-dark-theme web-mode projectile-speedbar sr-speedbar rainbow-delimiters ws-butler dtrt-indent helm-projectile projectile rust-mode company dap-mode flycheck helm-flycheck helm-lsp lsp-mode lsp-ui which-key helm-swoop helm-ag smart-mode-line helm magit smartparens clean-aindent-mode cmake-mode cuda-mode lua-mode hlinum window-number use-package))
 '(rm-blacklist
   '(" hl-p" " SP" " Abbrev" " FA" " hs" " Helm" " wb" " WK" " yas" " company" " Irony" " ElDoc" " FlyC" " Lens" " dtrt-indent" " ARev")))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; no menu or welcome
(menu-bar-mode -1)
(setq inhibit-startup-message t)

;; custom theme
(load-theme 'seshu)

(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list
   'package-archives
   '("melpa" . "http://melpa.org/packages/")
   t)
  ;;(package-initialize)
  )

;; This is only needed once, near the top of the file
(eval-when-compile
  ;; Following line is not needed if use-package.el is in ~/.emacs.d
  ;;(add-to-list 'load-path "<path where use-package is installed>")
  (require 'use-package))

;; window switching
(global-set-key (kbd "C-c l") 'windmove-left)
(global-set-key (kbd "C-c r") 'windmove-right)
(global-set-key (kbd "C-c u") 'windmove-up)
(global-set-key (kbd "C-c d") 'windmove-down)

;; winner-mode
(winner-mode)

(defun toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
	     (next-win-buffer (window-buffer (next-window)))
	     (this-win-edges (window-edges (selected-window)))
	     (next-win-edges (window-edges (next-window)))
	     (this-win-2nd (not (and (<= (car this-win-edges)
					 (car next-win-edges))
				     (<= (cadr this-win-edges)
					 (cadr next-win-edges)))))
	     (splitter
	      (if (= (car this-win-edges)
		     (car (window-edges (next-window))))
		  'split-window-horizontally
		'split-window-vertically)))
	(delete-other-windows)
	(let ((first-win (selected-window)))
	  (funcall splitter)
	  (if this-win-2nd (other-window 1))
	  (set-window-buffer (selected-window) this-win-buffer)
	  (set-window-buffer (next-window) next-win-buffer)
	  (select-window first-win)
	  (if this-win-2nd (other-window 1))))))

(global-set-key (kbd "C-c \\") 'toggle-window-split)

;; window number
(use-package window-number
  :config
  (window-number-mode 1)
  (window-number-meta-mode 1))

;; hlinum
(use-package hlinum
  :config
  (hlinum-activate)
  (setq linum-format "%d ")
  (global-linum-mode 1))

;; lua
(use-package lua-mode
  :mode "\\.lua\\'"
  :interpreter "lua")

;; regent
(use-package regent-mode
  :load-path "elpa/regent-mode/"
  :mode "\\.rg\\'")

;; terra
(use-package terra-mode
  :load-path "elpa/terra-mode/"
  :mode "\\.t\\'")

;; rust
(use-package rust-mode
  :mode "\\.rs\\'")

;; gdb
(setq gdb-many-windows t
      gdb-show-main t)

;; cuda
(use-package cuda-mode
  :mode "\\.cu")

;; c-style
(setq c-default-style "bsd")
(setq-default c-basic-offset 2)

;; cmake
(use-package cmake-mode
  :mode "CMakeLists\\.txt\\'"
  :hook (cmake-mode . cmake-font-lock-activate))


;; whitespace
;; (global-set-key (kbd "C-c w") 'whitespace-mode)
(add-hook 'prog-mode-hook (lambda () (interactive) (setq show-trailing-whitespace 1)))
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;; clean-aindent
(use-package clean-aindent-mode
  :bind ([?\r] . 'newline-and-indent)
  :hook (prog-mode . clean-aindent-mode))

;; smartparens
(use-package smartparens-config
  :config
  (show-smartparens-global-mode +1)
  (smartparens-global-mode 1)
  ;; when you press RET, the curly braces automatically
  ;; add another newline
  (sp-with-modes '(c-mode c++-mode)
    (sp-local-pair "{" nil :post-handlers '(("||\n[i]" "RET")))
    (sp-local-pair "/*" "*/" :post-handlers '((" | " "SPC")
                                              ("* ||\n[i]" "RET")))))

;; magit
(use-package magit
  :bind ("C-x g" . 'magit-status))

;; sml
(use-package smart-mode-line-atom-one-dark-theme)

(use-package smart-mode-line
  :config
  (setq sml/theme 'atom-one-dark)
  (sml/setup))

;; helm
(use-package helm
  :bind
  (("M-x" . helm-M-x)
   ("M-y" . helm-show-kill-ring)
   ("C-x b" . helm-mini)
   ("C-x C-f" . helm-find-files)
   ("C-h <SPC>" . helm-all-mark-rings)
   ("C-c h o" . helm-occur)
   ("C-c h x" . helm-register)
   ([remap list-buffers helm-buffers-list])
   (:map helm-map
	 ("<tab>" . helm-execute-persistent-action)
	 ("C-i" . helm-execute-persistent-action)
	 ("C-z" . helm-select-action))
   (:map minibuffer-local-map
	 ("M-p" . helm-minibuffer-history)
	 ("M-n" . helm-minibuffer-history)))
  :config
  (setq
   helm-scroll-amount 4 ; scroll 4 lines other window using M-<next>/M-<prior>
   helm-ff-search-library-in-sexp t ; search for library in `require' and `declare-function' sexp.
   helm-split-window-in-side-p t ;; open helm buffer inside current window, not occupy whole other window
   helm-candidate-number-limit 500 ; limit the number of displayed canidates
   helm-ff-file-name-history-use-recentf t
   helm-move-to-line-cycle-in-source t ; move to end or beginning of source when reaching top or bottom of source.
   helm-buffers-fuzzy-matching t ; fuzzy matching buffer names when non-nil
   helm-M-x-fuzzy-match t ; useful in helm-mini that lists buffers
   ;; helm-follow-mode-persistent t
   ;; helm-ag-use-temp-buffer t
   ;; helm-mode-fuzzy-match t
   ;; helm-completion-in-region-fuzzy-match t
   )
  (helm-autoresize-mode 1)
  (helm-mode 1))

;; helm-grep
(use-package helm-grep
  :after helm
  :bind ((:map helm-grep-mode-map
	 ("<return>" . helm-grep-mode-jump-other-window)
	 ("n" . helm-grep-mode-jump-other-window-forward)
	 ("p" . helm-grep-mode-jump-other-window-backward))))


;; helm-swoop
(use-package helm-swoop
  :after helm
  :bind (("C-c h o" . helm-swoop)
	 ("C-c t" . helm-multi-swoop-all)
	 (:map isearch-mode-map
	       ("M-i" . helm-swoop-from-isearch)))
  :bind-keymap (("M-i" . helm-multi-swoop-all-from-helm-swoop))
  :config
  (setq helm-multi-swoop-edit-save t ;; Save buffer when helm-multi-swoop-edit complete
	helm-swoop-split-with-multiple-windows t ;; split window inside the current window
	helm-swoop-split-direction 'split-window-vertically ;;'split-window-vertically or 'split-window-horizontally
	))

;; company
(use-package company-mode
  :hook (prog-mode . company-mode))

;; lsp
(use-package lsp-mode
  :hook ((lsp-mode . (lambda ()
                      (let ((lsp-keymap-prefix "C-c l"))
                        (lsp-enable-which-key-integration))))
         (prog-mode . lsp-mode))
  :bind ((:map lsp-mode-map ("C-c l" . lsp-command-map)))
  :config
  (defun reset-lsp-blacklist ()
    (setf (lsp-session-folders-blocklist (lsp-session)) nil)
    (lsp--persist-session (lsp-session))))

;; use python-lsp-server

;; lsp-ui
(use-package lsp-ui
  :after lsp-mode)

;; helm-lsp
(use-package helm-lsp
  :after (helm lsp-mode))

;; dap
(use-package dap-mode)

;; dap-python
(use-package dap-python
  :after dap-mode
  :config (setq dap-python-debugger 'debugpy))

;; hs-minor-mode
(add-hook 'prog-mode-hook 'hs-minor-mode)

;; dtrt-indent
(use-package dtrt-indent
  :config
  (dtrt-indent-mode 1)
  (setq dtrt-indent-verbosity 0))

;; ws-butler
(use-package ws-butler
  :config
  (ws-butler-global-mode))

;; which-key
(use-package which-key
  :config
  (which-key-mode))

;; rainbow delimiters
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; speedbar
(use-package sr-speedbar
  :bind (("C-c s" . sr-speedbar-toggle)))

(use-package web-mode
  :mode "\\.html\\'"
  :bind ((:map web-mode-map ("C-c @ C-c" . web-mode-fold-or-unfold))))

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

