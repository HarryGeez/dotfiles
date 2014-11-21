(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)
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
(require 'autopair)
(autopair-global-mode)
;; (add-hook 'c-mode-common-hook
;; 	  #'(lambda () (autopair-mode)))
(setq-default c-basic-offset 4
	      tab-width 4
	      indent-tabs-mode t)
(require 'cc-mode)
(define-key c-mode-base-map (kbd "RET") 'newline-and-indent)
(setq c-default-style "stroustrup")
