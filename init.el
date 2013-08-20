; Start Server
; ============

(server-start)

; SEARCH PATH
; ===========

(add-to-list 'load-path user-emacs-directory)

; MELPA
; =====

; packages to install:
;  - melpa
;  - icicle
;  - auctex

(defun install-package-unless-installed (package)
  (when (not (package-installed-p package))
    (package-install package)))

(when (> emacs-major-version 23)
  (require 'package)
  (package-initialize)
  (add-to-list 'package-archives
               '("melpa" . "http://melpa.milkbox.net/packages/")
               'APPEND)
  (install-package-unless-installed 'haskell-mode))


; GENERIC MODE
; ============

(require 'generic-x)
(require 'pts-mode)

; MODIFIER KEYS
; =============

(setq w32-pass-lwindow-to-system nil 
      w32-pass-rwindow-to-system nil 
      w32-pass-apps-to-system nil 
      w32-lwindow-modifier 'super ; Left Windows key 
      w32-rwindow-modifier 'super ; Right Windows key 
      w32-apps-modifier 'hyper) ; Menu key

; CONFIGURE INPUT METHODS
; =======================

(defun disable-input-method ()
  (when current-input-method
    (toggle-input-method)))

(defun enable-input-method ()
  (when (not current-input-method)
    (toggle-input-method)))

(add-hook 'text-mode-hook 'enable-input-method)
(add-hook 'TeX-mode-hook 'disable-input-method)

; CUSTOM
; ======

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-PDF-mode t)
 '(TeX-source-correlate-method (quote synctex))
 '(TeX-source-correlate-mode t)
 '(TeX-source-correlate-start-server t)
 '(agda2-ghci-options (quote ("-package Agda-2.3.0")))
 '(agda2-include-dirs (quote ("." "c:\\Users\\rendel\\Documents\\agda-lib\\src")))
 '(compilation-auto-jump-to-first-error t)
 '(compilation-scroll-output (quote first-error))
 '(cua-auto-tabify-rectangles nil)
 '(cua-enable-cua-keys t)
 '(cua-mode t nil (cua-base))
 '(default-input-method "Agda")
 '(fill-column 65)
 '(indent-tabs-mode nil)
 '(tex-fontify-script nil))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "SystemWindow" :foreground "SystemWindowText" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 120 :width normal :family "FreeMono"))))
 '(fixed-pitch ((t (:family "FreeMono"))))
 '(italic ((t (:underline nil :slant italic))))
 '(variable-pitch ((t (:family "FreeSerif")))))

; AGDA MODE
; =========

; Prepare for loading Agda mode and load Agda input method
(let ((agda-mode-path
       (let ((coding-system-for-read 'utf-8))
         (shell-command-to-string "agda-mode locate"))))
  (when (file-exists-p agda-mode-path)
    (load agda-mode-path)
    (require 'agda-input)
    (require 'two-mode-mode)
    (add-to-list 'auto-mode-alist
      '("\\.lagda\\'" . two-mode-mode))))

; SCALA MODE
; ==========

(add-to-list 'load-path
  (concat
    (getenv "SCALA_HOME")
    "\\misc\\scala-tool-support\\emacs"))
(require 'scala-mode-auto)

; ICICLE
; ======

(icy-mode)
