;;; init.el --- My init, my style                   -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Wei Jian Gan

;; Author: Wei Jian Gan <weijiangan@outlook.com>
;; Keywords: init

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; You do not need to do anything.  Just place it in your .emacs.d :)

;;; Code:

(setq mac-option-modifier 'meta)
(setq mac-command-modifier 'super)

(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (url (concat (if no-ssl "http" "https") "://melpa.org/packages/")))
  (add-to-list 'package-archives (cons "melpa" url) t))
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(require 'bind-key)

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns x))
  :ensure t
  :config
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-env "GOPATH"))

(use-package add-node-modules-path
  :ensure t
  :hook (js2-mode))

(use-package ace-jump-mode
  :ensure t
  :defer t
  :bind ("C-c SPC" . ace-jump-mode))

(use-package company
  :diminish "Ⓐ"
  :ensure t
  :hook ((js2-mode go-mode) . company-mode)
  :init
  (require 'company)
  ;; (add-to-list 'company-backends 'company-irony)
  ;; (add-to-list 'company-backends 'company-c-headers)
  (setq company-dabbrev-downcase nil)
  (setq company-tooltip-limit 20)                      ; bigger popup window
  (setq company-idle-delay .3)                         ; decrease delay before autocompletion popup shows
  (setq company-echo-delay 0)                          ; remove annoying blinking
  (setq company-begin-commands '(self-insert-command)) ; start autocompletion only after typing
  )

(use-package company-c-headers
  :ensure t
  :defer t)

(use-package company-go
  :ensure t
  :defer t)

(use-package company-irony
  :ensure t
  :defer t)

(use-package diminish
  :ensure t
  :config
  (diminish 'eldoc-mode "ⓔ"))

(use-package disable-mouse
  :disabled
  :ensure t
  :config
  (global-disable-mouse-mode))

(use-package emmet-mode
  :ensure t
  :hook (web-mode sgml-mode css-mode))

(use-package expand-region
  :ensure t
  :bind ("C-c =" . er/expand-region))

(use-package flycheck
  :diminish
  :ensure t
  :init
  (add-hook 'after-init-hook #'global-flycheck-mode))

(use-package go-mode
  :ensure t
  :defer t
  :init
  (add-hook 'go-mode-hook
            (lambda ()
              (add-hook 'after-save-hook 'gofmt-before-save nil 'make-it-local)
              (setq tab-width 4)
              (set (make-local-variable 'company-backends) '(company-go)))))

(use-package golden-ratio
  :diminish "ⓖ"
  :ensure t
  :defer t)

(use-package helm
  ;; :diminish "Ⓗ"
  :diminish
  :ensure t
  :demand
  :commands (helm-mode helm-autoresize-mode)
  :bind (("M-x" . helm-M-x)
         ("M-y" . helm-show-kill-ring)
         ("C-x b" . helm-mini)
         ("C-c h o" . helm-occur)
         ("C-x C-f" . helm-find-files))
  :preface
  (defun spacemacs//helm-hide-minibuffer-maybe ()
    "Hide minibuffer in Helm session if we use the header line as input field."
    (when (with-helm-buffer helm-echo-input-in-header-line)
      (let ((ov (make-overlay (point-min) (point-max) nil nil t)))
        (overlay-put ov 'window (selected-window))
        (overlay-put ov 'face
                     (let ((bg-color (face-background 'default nil)))
                       `(:background ,bg-color :foreground ,bg-color)))
        (setq-local cursor-type nil))))
  :config
  (require 'helm-config)
  (setq-default helm-buffers-fuzzy-matching            t)
  (setq-default helm-ff-file-name-history-use-recentf  t)
  (setq-default helm-ff-search-library-in-sexp         t)
  (setq-default helm-recentf-fuzzy-match               t)
  (setq helm-echo-input-in-header-line                 t)
  (setq helm-move-to-line-cycle-in-source              t)
  ;; scroll 8 lines other window using M-<next>/M-<prior>
  (setq helm-scroll-amount                             8)
  ;; open helm buffer inside current window, not occupy whole other window
  (setq helm-split-window-inside-p                     t)
  (setq helm-autoresize-max-height                     0)
  (setq helm-autoresize-min-height                    30)
  (add-hook 'helm-minibuffer-set-up-hook
            'spacemacs//helm-hide-minibuffer-maybe)
  (helm-mode 1)
  (helm-autoresize-mode 1))

(use-package iedit
  :ensure t)

(use-package irony
  :diminish "Ⓘ"
  :ensure t
  :bind (:map irony-mode-map
              ("<remap> <completion-at-point>" . irony-completion-at-point-async)
              ("<remap> <complete-symbol>" . irony-completion-at-point-async))
  :hook
  (((c++-mode c-mode objc-mode) . irony-mode)
   (irony-mode . irony-cdb-autosetup-compile-options)))

(use-package js
  :init
  (add-hook 'js-mode-hook
            (lambda ()
              (setq js-indent-level 2))))

(use-package js2-mode
  :ensure t
  :bind (:map js2-mode-map ("M-." . nil))
  :mode (("\\.js\\'" . js2-mode)
         ;; ("components?\\/.*\\.jsx?\\'" . js2-jsx-mode)
         )
  :interpreter ("node" . js2-jsx-mode)
  :init
  (add-hook 'js2-mode-hook
            (lambda ()
              (setq js2-basic-offset 2)
              ;; (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t)
              )))

(use-package js2-refactor
  :ensure t
  :defer t)

(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)
     ("C-x C-j" . magit-dired-jump)))

(use-package markdown-mode
  :ensure t
  :defer t
  :init
  (setq markdown-command "marked")
  (setq markdown-asymmetric-header t)
  (setq markdown-header-scaling t)
  (setq markdown-css-paths `(,"~/Git/github-markdown-css/github-markdown.css")))

(use-package multiple-cursors
  :ensure t
  :defer t)

(use-package persp-mode
  :disabled
  :diminish
  :ensure t
  :commands (persp-mode)
  :defer t
  :init
  (setq wg-morph-on nil) ;; switch off animation
  (setq persp-autokill-buffer-on-remove 'kill-weak)
  :config
  (persp-mode 1))

(use-package phi-search
  :ensure t
  :defer t)

(use-package prettier-js
  :diminish "Ⓟ"
  :ensure t
  :defer t
  :hook
  (js2-mode . prettier-js-mode))

(use-package protobuf-mode
  :ensure t
  :defer t
  :init
  (add-hook 'protobuf-mode-hook
            (lambda ()
              (setq indent-tabs-mode nil)
              (setq tab-width 2))))


(use-package pug-mode
  :ensure t
  :defer t)

(use-package rainbow-delimiters
  :ensure t
  :defer t)

(use-package rainbow-mode
  :ensure t
  :defer t)

(use-package rjsx-mode
  :ensure t
  :mode "components\\/.*\\.js\\'")

(use-package smartparens
  :diminish "ⓟ"
  :ensure t
  :commands (smartparens-global-mode)
  :init
  (require 'smartparens-config)
  :defer 7
  :config
  (smartparens-global-mode t))

(use-package snazzy-theme
  :if window-system
  :ensure t
  :config
  (load-theme 'snazzy t)
  (set-face-attribute 'fringe nil
                      :foreground (face-foreground 'default)
                      :background (face-background 'default)))

(use-package spaceline
  :if window-system
  :ensure t
  :commands (spaceline-toggle-buffer-position-off spaceline-toggle-hud-off
                                                  spaceline-spacemacs-theme spaceline-helm-mode)
  :defer t
  :init
  (defun spacemacs/compute-powerline-height ()
    "Return an adjusted powerline height."
    (let ((scale (if (and (boundp 'powerline-scale) powerline-scale)
                     powerline-scale 1)))
      (truncate (* scale (frame-char-height)))))
  (require 'spaceline-config)
  (setq powerline-image-apple-rgb nil)
  (setq powerline-default-separator "wave") ; fix off-colors, must be in this order!
  (setq powerline-scale 1.5)
  (setq powerline-height (spacemacs/compute-powerline-height))
  (setq spaceline-window-numbers-unicode t)
  (setq spaceline-minor-modes-separator " ")
  :config
  (spaceline-toggle-buffer-position-off)
  (spaceline-toggle-hud-off)
  (spaceline-spacemacs-theme)
  (spaceline-helm-mode))

(use-package typescript-mode
  :ensure t
  :defer t)

(use-package web-mode
  :ensure t
  :mode ("\\.phtml\\'" "\\.tpl\\.php\\'" "\\.[agj]sp\\'" "\\.as[cp]x\\'" "\\.erb\\'"
         "\\.mustache\\'" "\\.djhtml\\'" "\\.html?\\'")
  :preface
  (defun sp-web-mode-is-code-context (id action context)
    (and (eq action 'insert)
         (not (or (get-text-property (point) 'part-side)
                  (get-text-property (point) 'block-side)))))
  :init
  (add-hook 'web-mode-hook
            '(lambda()
               (setq web-mode-markup-indent-offset 2)
               (setq web-mode-css-indent-offset 2)
               (setq web-mode-code-indent-offset 2)
               (setq web-mode-style-padding 2)
               (setq web-mode-script-padding 2)
               (setq web-mode-enable-auto-pairing nil)
               (setq web-mode-enable-css-colorization t)))
  (add-hook 'local-write-file-hooks
            (lambda ()
              (delete-trailing-whitespace)
              nil))
  :config
  (sp-local-pair 'web-mode "<" nil :when '(sp-web-mode-is-code-context)))

(use-package which-key
  :diminish
  :defer 5
  :ensure t
  :commands (which-key-mode which-key-setup-side-window-bottom)
  :config
  (which-key-mode)
  (which-key-setup-side-window-bottom))

(use-package winum
  :ensure t
  :defer 6
  :bind (("C-`" . winum-select-window-by-number)
         ("C-²" . winum-select-window-by-number)
         ("s-0" . winum-select-window-0-or-10)
         ("s-1" . winum-select-window-1)
         ("s-2" . winum-select-window-2)
         ("s-3" . winum-select-window-3)
         ("s-4" . winum-select-window-4)
         ("s-5" . winum-select-window-5)
         ("s-6" . winum-select-window-6)
         ("s-7" . winum-select-window-7)
         ("s-8" . winum-select-window-8))
  :config
  (setq winum-auto-setup-mode-line nil)
  (winum-mode))

(use-package xref-js2
  :ensure t
  :defer t)

(use-package yaml-mode
  :ensure t
  :defer t)

(use-package yasnippet
  :ensure t
  :commands (yas-global-mode)
  :defer t
  ;; :config
  ;; (yas-global-mode 1)
  )

(use-package cc-mode
  :no-require t
  :defer t
  :bind (:map c-mode-base-map ([ret] . newline-and-indent))
  :init
  (setq-default c-default-style "linux")
  (setq-default c-basic-offset 4
                tab-width 4
                indent-tabs-mode nil)
  ;; (define-key c-mode-base-map (kbd "RET") 'newline-and-indent)
  )

(use-package c-mode
  :no-require t
  :defer t
  :init
  (add-hook 'c-mode-hook
            (lambda ()
              (when (not (equal buffer-file-name nil))
                (set (make-local-variable 'compile-command)
                     (concat "gcc " (shell-quote-argument buffer-file-name)))))))

(use-package css-mode
  :no-require t
  :init
  (setq css-indent-offset 2))

(use-package dired
  :no-require t
  :defer t
  :hook (dired-mode . auto-revert-mode))

(use-package octave
  :no-require t
  :defer t
  :init
  (add-hook 'inferior-octave-mode-hook
            (lambda ()
              (setq comint-prompt-read-only t))))

(use-package visual-line-mode
  :no-require t
  :diminish "Ⓦ"
  :defer t)

(use-package abbrev
  :no-require t
  :diminish "ⓐ"
  :defer t)

(use-package whitespace
  :no-require t
  :diminish "ⓦ"
  :defer t)

(use-package hl-line-mode
  :no-require t
  :hook (emacs-startup . global-hl-line-mode))

(use-package recentf
  :no-require t
  :custom
  (recentf-max-saved-items 300))

(defun fake-kill-line ()
  "Save the rest of the current line to the 'kill-ring'."
  (interactive)
  (save-excursion
    (set-mark-command nil)
    (move-end-of-line 1)
    (kill-ring-save (region-beginning) (region-end))))

(global-set-key (kbd "C-c k") 'fake-kill-line)

(if (window-system)
    (progn
      (add-to-list 'default-frame-alist '(font . "SF Mono 13"))
      ;; This is bound to f11 in Emacs 24.4
      ;; (toggle-frame-fullscreen)
      ;; Who use the bar to scroll?
      (scroll-bar-mode 0)
      ;; No toolbar
      (tool-bar-mode 0)
      ;; No menubar
      ;; (menu-bar-mode 0)
      ;; No fringes
      ;; (fringe-mode 0)
      (fringe-mode '(20 . 20))
      (setq ns-use-srgb-colorspace nil))
  ;; else
  ;; No menubar
  (menu-bar-mode 0)
  (setq frame-background-mode 'dark))

(provide 'init)
;;; init.el ends here
