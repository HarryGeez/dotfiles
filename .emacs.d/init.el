;; (add-to-list 'load-path "~/.emacs.d/lisp")
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
		 (if (y-or-n-p (format "Package %s is missing. Install it? " package))
			 (package-install package)
		   package)))
	 packages))

  ;; make sure to have downloaded archive description.
  ;; Or use package-archive-contents as suggested by Nicolas Dudebout
  (or (file-exists-p package-user-dir)
	  (package-refresh-contents))

  (ensure-package-installed
   'iedit
   'web-mode
   'emmet-mode
   'expand-region
   'yasnippet
   'workgroups2
   'auto-complete
   ))

;; Disable startup message
(setq inhibit-startup-message t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(initial-scratch-message nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; ;; This is bound to f11 in Emacs 24.4
;; (toggle-frame-fullscreen)
;; ;; Who use the bar to scroll?
;; (scroll-bar-mode 0)
;; ;; No toolbar
;; (tool-bar-mode 0)
;; No menubar
(menu-bar-mode 0)

;; Compile witout prompt. Add C-u before M-x compile to compile normally
(setq compilation-read-command nil)
;; Press C-u C-x C-m to compile normally
(global-set-key "\C-x\C-m" 'compile)

(global-set-key (kbd "ESC <up>") 'beginning-of-buffer)
(global-set-key (kbd "ESC <down>") 'end-of-buffer)

;; Clear Eshell buffer
(defun eshell-clear-buffer ()
  "Clears the shell buffer ala Unix's clear or DOS' cls"
  (interactive)
  ;; the shell prompts are read-only, so clear that for the duration
  (let ((inhibit-read-only t))
	;; simply delete the region
	(delete-region (point-min) (point-max))
	(eshell-send-input)))

(add-hook 'eshell-mode-hook
		  '(lambda()
			 (local-set-key (kbd "C-l") 'eshell-clear-buffer)))


;;
;;  Electric-Pair Mode
;;

(electric-pair-mode 1)

;; ;; Only enable electric-pair for specified modes
;; (defvar my-electic-pair-modes '(c-mode))
;; (defun my-inhibit-electric-pair-mode (char)
;;   (not (member major-mode my-electic-pair-modes)))
;; (setq electric-pair-inhibit-predicate #'my-inhibit-electric-pair-mode)

;;
;;  C-Mode
;;

;; Set C-Mode to use Stroustrup indentation style.
(setq c-default-style "linux")
;; More indentation settings; Set tab-width to 4.
(setq-default c-basic-offset 4
			  tab-width 4
			  indent-tabs-mode t)
;; Map "newline-and-indent" to RET
(require 'cc-mode)
(define-key c-mode-base-map (kbd "RET") 'newline-and-indent)

;; Function to run command in an eshell buffer if compilation finishes without errors.
(defun run-compilation-output-in-eshell (buf msg)
  "If compilation finished successfully, switch to eshell and execute a command."
  (interactive)
  (when (equal major-mode 'compilation-mode)
	(when (string= msg "finished\n")
	  (other-window 1)
	  (eshell)
	  (goto-char (point-max))
	  (eshell-kill-input)
	  (insert "./a.out")
	  (eshell-send-input)
	  (other-window 1))))

;; Change default compile command to cc <buffer name>.
(add-hook 'c-mode-hook
		  (lambda ()
			(when (not (equal buffer-file-name nil))
			  (set (make-local-variable 'compile-command)
				   (concat "gcc " (shell-quote-argument buffer-file-name))))))

;; Run a.out in an eshell buffer after compilation.
(add-hook 'compilation-finish-functions 'run-compilation-output-in-eshell)


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
(add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsx$" . web-mode))

(add-hook 'web-mode-hook
		  '(lambda()
			 (setq web-mode-markup-indent-offset 2)
			 (setq web-mode-style-padding 2)
			 (setq web-mode-script-padding 2)
			 (setq web-mode-code-indent-offset 2)
			 (setq web-mode-enable-css-colorization t)))

(setq web-mode-content-types-alist
	  '(("jsx" . "\\.js[x]?\\'")))

(defun web-mode-jsx-fix ()
  "Fix web-mode's broken JSX syntax highlighting and reindent buffer."
  (interactive)
  (when (equal major-mode 'web-mode)
	(web-mode)
	(indent-region (point-min) (point-max))
	(delete-trailing-whitespace)))

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


;;
;;  workgroups2
;;

(require 'workgroups2)
(setq wg-prefix-key (kbd "C-c z"))
;; (workgroups-mode 1)
;; (wg-open-session "~/.emacs_workgroup")


;;
;;  yasnippet
;;

(yas-global-mode 1)


;;
;;  auto-complete
;;

;; start auto-complete with emacs
(require 'auto-complete)
;; do default config for auto-complete
(require 'auto-complete-config)
(ac-config-default)
