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

;; Autopair
(require 'autopair)
(autopair-global-mode)

;; C Programming
; Set C-Mode to use Stroustrup indentation style.
(setq c-default-style "stroustrup")
; More indentation settings; Set tab-width to 4.
(setq-default c-basic-offset 4
			  tab-width 4
			  indent-tabs-mode t)
; Map "newline-and-indent" to RET
(require 'cc-mode)
(define-key c-mode-base-map (kbd "RET") 'newline-and-indent)

;; Smart-Tabs-Mode
(autoload 'smart-tabs-mode "smart-tabs-mode"
  "Intelligently indent with tabs, align with spaces!")
(autoload 'smart-tabs-mode-enable "smart-tabs-mode")
(autoload 'smart-tabs-advice "smart-tabs-mode")
(autoload 'smart-tabs-insinuate "smart-tabs-mode")
; Smart-Tabs for C
(add-hook 'c-mode-hook 'smart-tabs-mode-enable)
(smart-tabs-advice c-indent-line c-basic-offset)
(smart-tabs-advice c-indent-region c-basic-offset)
