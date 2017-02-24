(setq-default buffer-file-coding-system 'utf-8-unix)
(setq-default default-buffer-file-coding-system 'utf-8-unix)
(set-default-coding-systems 'utf-8-unix)
(prefer-coding-system 'utf-8-unix)

;; (add-to-list 'load-path "~/.emacs.d/lisp")
(setenv "PATH" (concat "/usr/local/bin:" (getenv "PATH")))
(when (> emacs-major-version 23)
  (require 'package)
  (add-to-list 'package-archives
			   '("melpa" . "http://melpa.milkbox.net/packages/") t)
  (package-initialize)

  (defun ensure-package-installed (&rest packages)
	"Assure every package is installed, ask for installation if it’s not.
    Return a list of installed packages or nil for every skipped package."
	(mapcar
	 (lambda (package)
	   ;; (package-installed-p 'evil)
	   (if (package-installed-p package)
		   nil
		 (if (y-or-n-p (format "Package %s is missing.  Install it? " package))
			 (package-install package)
		   package)))
	 packages))

  ;; make sure to have downloaded archive description.
  ;; Or use package-archive-contents as suggested by Nicolas Dudebout
  (or (file-exists-p package-user-dir)
	  (package-refresh-contents))

  (ensure-package-installed
   'base16-theme
   'company
   'company-c-headers
   'company-irony
   'company-tern
   'delight
   'emmet-mode
   'expand-region
   'flycheck
   'golden-ratio
   'helm
   'iedit
   'irony
   'js2-mode
   'persp-mode
   'rainbow-delimiters
   'smartparens
   'spaceline
   'tern
   'web-mode
   'which-key
   'window-numbering
   'yasnippet
   ))

;; Disable startup message
(setq inhibit-startup-message t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-c-headers-path-system
   (quote
	("/usr/include/" "/usr/local/include/" "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/../include/c++/v1/")))
 '(initial-scratch-message nil)
 '(package-selected-packages
   (quote
	(phi-search multiple-cursors delight general-close which-key company-irony irony helm persp-mode spaceline window-numbering company-tern company-c-headers company js2-mode base16-theme yasnippet web-mode typing iedit expand-region emmet-mode 2048-game))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Compile witout prompt. Add C-u before M-x compile to compile normally
(setq compilation-read-command nil)
;; Press C-u C-x C-m to compile normally
(global-set-key "\C-x\C-m" 'compile)

(global-set-key (kbd "ESC <up>") 'beginning-of-buffer)
(global-set-key (kbd "ESC <down>") 'end-of-buffer)

;; Clear Eshell buffer
(defun eshell-clear-buffer ()
  "Clears the shell buffer ala Unix's clear or DOS' cls."
  (interactive)
  ;; the shell prompts are read-only, so clear that for the duration
  (let ((inhibit-read-only t))
	;; simply delete the region
	(delete-region (point-min) (point-max))
	(eshell-send-input)))

(add-hook 'eshell-mode-hook
		  '(lambda()
			 (local-set-key (kbd "C-l") 'eshell-clear-buffer)))

(defun fake-kill-line ()
  "Save the rest of the current line to the 'kill-ring'."
  (interactive)
  (save-excursion
	(set-mark-command nil)
	(move-end-of-line 1)
	(kill-ring-save (region-beginning) (region-end))))

(global-set-key (kbd "C-c k") 'fake-kill-line)


;;
;;  smartparens-mode
;;

(require 'smartparens-config)
(smartparens-strict-mode)
(smartparens-global-mode 1)


;;
;;  C-Mode
;;

;; Set C-Mode to use Linux indentation style.
(setq c-default-style "linux")
;; More indentation settings; Set tab-width to 4.
(setq-default c-basic-offset 4
			  tab-width 4
			  indent-tabs-mode t)
;; Map "newline-and-indent" to RET
(require 'cc-mode)
(define-key c-mode-base-map (kbd "RET") 'newline-and-indent)

;; ;; Function to run command in an eshell buffer if compilation finishes without errors.
;; (defun run-compilation-output-in-eshell (buf msg)
;;   "If compilation finished successfully, switch to eshell and execute a command."
;;   (interactive)
;;   (when (equal major-mode 'compilation-mode)
;; 	(when (string= msg "finished\n")
;; 	  (other-window 1)
;; 	  (eshell)
;; 	  (goto-char (point-max))
;; 	  (eshell-kill-input)
;; 	  (insert "./a.out")
;; 	  (eshell-send-input)
;; 	  (other-window 1))))

;; Change default compile command to cc <buffer name>.
(add-hook 'c-mode-hook
		  (lambda ()
			(when (not (equal buffer-file-name nil))
			  (set (make-local-variable 'compile-command)
				   (concat "gcc " (shell-quote-argument buffer-file-name))))))

;; ;; Run a.out in an eshell buffer after compilation.
;; (add-hook 'compilation-finish-functions 'run-compilation-output-in-eshell)


;;
;;  c++-mode
;;

(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(add-hook 'c++-mode-hook
		  (lambda ()
			(when (not (equal buffer-file-name nil))
			  (set (make-local-variable 'compile-command) "g++ *.cc"))))


;; ;;
;; ;;  semantic
;; ;;

;; (require 'semantic)
;; (global-semanticdb-minor-mode 1)
;; (global-semantic-idle-scheduler-mode 1)
;; (semantic-mode 1)
;; (semantic-add-system-include "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/../include/c++/v1" 'c++-mode)
;; (semantic-add-system-include "~/linux/kernel")
;; (semantic-add-system-include "~/linux/include")


;;
;;  Iedit
;;

;; Set iedit toggle key to C-x ; because C-; doesn't work in OS X
(define-key global-map (kbd "C-x ;") 'iedit-mode)
(define-key isearch-mode-map (kbd "C-x ;") 'iedit-mode-from-isearch)
(define-key esc-map (kbd "C-x ;") 'iedit-execute-last-modification)
(define-key help-map (kbd "C-x ;") 'iedit-mode-toggle-on-function)


;;
;;  expand-region
;;

(require 'expand-region)
(global-set-key (kbd "C-c =") 'er/expand-region)


;;
;;  web-mode
;;

;; The customisations below should not be put in the hook. Declare them before loading web-mode.el

(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))

(add-hook 'web-mode-hook
		  '(lambda()
			 (setq web-mode-markup-indent-offset 2)
			 (setq web-mode-style-padding 2)
			 (setq web-mode-script-padding 2)
			 (setq web-mode-code-indent-offset 2)
			 (setq web-mode-enable-css-colorization t)))

(define-key web-mode-map (kbd "C-c C-i") nil)
(define-key web-mode-map (kbd "C-c C-i") 'web-mode-jsx-fix)


;;
;;  emmet-mode
;;

(require 'emmet-mode)
(add-hook 'web-mode-hook 'emmet-mode)
(add-hook 'sgml-mode-hook 'emmet-mode)
(add-hook 'css-mode-hook 'emmet-mode)


;;
;;  org-mode
;;

(require 'org)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)
(setq org-agenda-files '("~/.org"))


;;
;;  yasnippet
;;

(yas-global-mode 1)


;;
;;  company
;;

(require 'company)
(add-hook 'after-init-hook 'global-company-mode)
(add-to-list 'company-backends 'company-irony)
(add-to-list 'company-backends 'company-c-headers)
(add-to-list 'company-backends 'company-tern)

;; ;; begin company and yasnippet conflict fix
;; (defun check-expansion ()
;;   (save-excursion
;; 	(if (looking-at "\\_>") t
;; 	  (backward-char 1)
;; 	  (if (looking-at "\\.") t
;; 		(backward-char 1)
;; 		(if (looking-at "->") t nil)))))

;; (defun do-yas-expand ()
;;   (let ((yas/fallback-behavior 'return-nil))
;; 	(yas/expand)))

;; (defun tab-indent-or-complete ()
;;   (interactive)
;;   (if (minibufferp)
;; 	  (minibuffer-complete)
;; 	(if (or (not yas/minor-mode)
;; 			(null (do-yas-expand)))
;; 		(if (check-expansion)
;; 			(company-complete-common)
;; 		  (indent-for-tab-command)))))

;; (global-set-key [tab] 'tab-indent-or-complete)
;; ;; end fix


;;
;;  irony
;;

(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'c-mode-hook 'irony-mode)
(add-hook 'objc-mode-hook 'irony-mode)

;; replace the `completion-at-point' and `complete-symbol' bindings in
;; irony-mode's buffers by irony-mode's function
(defun my-irony-mode-hook ()
  (define-key irony-mode-map [remap completion-at-point]
    'irony-completion-at-point-async)
  (define-key irony-mode-map [remap complete-symbol]
    'irony-completion-at-point-async))
(add-hook 'irony-mode-hook 'my-irony-mode-hook)
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

(setq irony-additional-clang-options
	  (quote
	   ("-I/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/../include/c++/v1"
		"-I/usr/local/include"
		"-I/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/../lib/clang/8.0.0/include"
		"-I/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/include"
		"-I/usr/include")))


;;
;;  js2-mode
;;

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))


;;
;;  tern-mode
;;

(add-to-list 'load-path "/usr/local/lib/node_modules/tern/emacs/")
(autoload 'tern-mode "tern.el" nil t)
(add-hook 'js2-mode-hook (lambda () (tern-mode t)))


;;
;;  persp-mode
;;

(with-eval-after-load "persp-mode-autoloads"
  (setq wg-morph-on nil) ;; switch off animation
  (setq persp-autokill-buffer-on-remove 'kill-weak)
  (add-hook 'after-init-hook #'(lambda () (persp-mode 1))))


;;
;;  window-numbering-mode
;;

(defun window-numbering-install-mode-line (&optional position)
  "Do nothing.")
(window-numbering-mode)


;;
;;  helm
;;

(require 'helm-config)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-c h o") 'helm-occur)
(setq helm-buffers-fuzzy-matching t
      helm-recentf-fuzzy-match    t)
(helm-mode 1)
(when (executable-find "curl")
  (setq helm-google-suggest-use-curl-p t))

(setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
      helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
      helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
      helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
      helm-ff-file-name-history-use-recentf t
      helm-echo-input-in-header-line t)

(defun spacemacs//helm-hide-minibuffer-maybe ()
  "Hide minibuffer in Helm session if we use the header line as input field."
  (when (with-helm-buffer helm-echo-input-in-header-line)
    (let ((ov (make-overlay (point-min) (point-max) nil nil t)))
      (overlay-put ov 'window (selected-window))
      (overlay-put ov 'face
                   (let ((bg-color (face-background 'default nil)))
                     `(:background ,bg-color :foreground ,bg-color)))
      (setq-local cursor-type nil))))


(add-hook 'helm-minibuffer-set-up-hook
          'spacemacs//helm-hide-minibuffer-maybe)

(setq helm-autoresize-max-height 0)
(setq helm-autoresize-min-height 20)
(helm-autoresize-mode 1)

;;
;;  which-key
;;

(require 'which-key)
(which-key-mode)
(which-key-setup-side-window-bottom)


;;
;;  golden-ratio
;;

(require 'golden-ratio)
(setq golden-ratio-extra-commands
      (append golden-ratio-extra-commands
              '(select-window-0
				select-window-1
				select-window-2
				select-window-3
				select-window-4
				select-window-5
				select-window-6
				select-window-7
				select-window-8
				select-window-9)))
; (golden-ratio-mode 1)

(add-hook 'after-init-hook #'global-flycheck-mode)


(if (display-graphic-p)
    (progn
	  (load-theme 'base16-ocean t)
	  ;; (defun invisible-fringes ()
	  ;; 	(set-face-attribute 'fringe nil
	  ;; 						:foreground (face-foreground 'default)
	  ;; 						:background (face-background 'default)))
	  ;; (invisible-fringes)
	  (set-frame-font "Dejavu Sans Mono 10")
	  ;; This is bound to f11 in Emacs 24.4
	  ;; (toggle-frame-fullscreen)
	  ;; Who use the bar to scroll?
	  (scroll-bar-mode 0)
	  ;; No toolbar
	  (tool-bar-mode 0)
	  ;; ;; No menubar
	  (menu-bar-mode 0)
	  ;; No fringes
	  (fringe-mode 0)
	  ;;  spaceline
	  (setq powerline-default-separator 'wave) ; fix off-colors, must be in this order!
	  (require 'spaceline-config)
	  (spaceline-spacemacs-theme)
	  ;;  delight
	  (require 'delight)
	  (delight '((company-mode "A" company)
				 (smartparens-mode "p" smartparens)
				 (yas-minor-mode "y" yasnippet)
				 (golden-ratio-mode "g" golden-ratio)
				 (visual-line-mode "W" simple)
				 (abbrev-mode "a" abbrev)
				 (helm-mode "H" helm)
				 (irony-mode "I" irony)
				 (which-key-mode nil which-key)
				 (flycheck-mode nil flycheck)
				 ))
	  )
  ;; else
  ;; No menubar
  (menu-bar-mode 0))
