; configure super and hyper keys
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
(add-hook 'tex-mode-hook 'disable-input-method)

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(agda2-ghci-options (quote ("-package Agda-2.3.0")))
 '(agda2-include-dirs (quote ("." "C:\\Program Files (x86)\\Agda\\2.3.0\\lib-0.6\\src")))
 '(cua-auto-tabify-rectangles nil)
 '(cua-enable-cua-keys t)
 '(cua-mode t nil (cua-base))
 '(default-input-method "Agda")
 '(fill-column 65)
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

(add-to-list 'load-path user-emacs-directory)

; Prepare for loading Agda mode and load Agda input method
(load-file (let ((coding-system-for-read 'utf-8))
                (shell-command-to-string "agda-mode locate")))

(require 'agda-input)

; configure two-mode for literate agda
(require 'two-mode-mode)

(add-to-list 'auto-mode-alist
  '("\\.lagda\\'" . two-mode-mode))
