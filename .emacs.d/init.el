; (add-to-list 'load-path "~/.emacs.d/lisp")
(when (> emacs-major-version 23)
  (require 'package)
  (add-to-list 'package-archives
    '("melpa" . "http://melpa.milkbox.net/packages/") t)
  (package-initialize))

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

;; Change default compile command to cc <buffer name>
(add-hook 'c-mode-hook
		  (lambda ()
			(set (make-local-variable 'compile-command)
				 (concat "gcc " (shell-quote-argument buffer-file-name)))))
;; Compile witout prompt. Add C-u before M-x compile to compile normally
(setq compilation-read-command nil)
;; Press C-u C-x C-m to compile normally
(global-set-key "\C-x\C-m" 'compile)

(global-set-key (kbd "ESC <up>") 'beginning-of-buffer)
(global-set-key (kbd "ESC <down>") 'end-of-buffer)

;;;;;;;;;;;;;;;;
;;            ;;
;;  Autopair  ;;
;;            ;;
;;;;;;;;;;;;;;;;

(require 'autopair)
(autopair-global-mode)

;;;;;;;;;;;;;;;;;;;;;
;;                 ;;
;;  C Programming  ;;
;;                 ;;
;;;;;;;;;;;;;;;;;;;;;

;; Set C-Mode to use Stroustrup indentation style.
(setq c-default-style "stroustrup")
;; More indentation settings; Set tab-width to 4.
(setq-default c-basic-offset 4
			  tab-width 4
			  indent-tabs-mode t)
;; Map "newline-and-indent" to RET
(require 'cc-mode)
(define-key c-mode-base-map (kbd "RET") 'newline-and-indent)

;;;;;;;;;;;;;;;;;;;;;;;
;;                   ;;
;;  Smart-Tabs-Mode  ;;
;;                   ;;
;;;;;;;;;;;;;;;;;;;;;;;

;; (autoload 'smart-tabs-mode "smart-tabs-mode"
;;   "Intelligently indent with tabs, align with spaces!")
;; (autoload 'smart-tabs-mode-enable "smart-tabs-mode")
;; (autoload 'smart-tabs-advice "smart-tabs-mode")
;; (autoload 'smart-tabs-insinuate "smart-tabs-mode")
;; ; Smart-Tabs for C
;; (add-hook 'c-mode-hook 'smart-tabs-mode-enable)
;; (smart-tabs-advice c-indent-line c-basic-offset)
;; (smart-tabs-advice c-indent-region c-basic-offset)

;;;;;;;;;;;;;
;;         ;;
;;  Iedit  ;;
;;         ;;
;;;;;;;;;;;;;

;; Set iedit toggle key to C-x ; because C-; doesn't work in OS X
(define-key global-map (kbd "C-x ;") 'iedit-mode)
(define-key isearch-mode-map (kbd "C-x ;") 'iedit-mode-from-isearch)
(define-key esc-map (kbd "C-x ;") 'iedit-execute-last-modification)
(define-key help-map (kbd "C-x ;") 'iedit-mode-toggle-on-function)

;;;;;;;;;;;;;;;;;;;;;
;;                 ;;
;;  expand-region  ;;
;;                 ;;
;;;;;;;;;;;;;;;;;;;;;

(require 'expand-region)
(global-set-key (kbd "C-c =") 'er/expand-region)
