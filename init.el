; OPEN INIT FILE
; ==============

(defun find-dot-emacs () (interactive)
  "Try to find and open the dot emacs file"
  (find-file "~/.emacs.d/init.el"))

; Maximize Window
; ===============

(add-hook 'window-setup-hook
          (lambda () (w32-send-sys-command #xf030)))

(add-hook 'term-setup-hook
          (lambda () (w32-send-sys-command #xf030)))

; Start Server
; ============

(server-start)

; SEARCH PATH
; ===========

(add-to-list 'load-path user-emacs-directory)

; MELPA
; =====

(defun install-package-unless-installed (package)
  (unless (package-installed-p package)
    (package-install package)))

(when (> emacs-major-version 23)
  (require 'package)
  (add-to-list 'package-archives
               '("melpa" . "http://melpa.milkbox.net/packages/")
               'APPEND)
  (package-initialize)
  (install-package-unless-installed 'icicles)
  (install-package-unless-installed 'auctex)
  (install-package-unless-installed 'scala-mode2)
  (install-package-unless-installed 'git-rebase-mode)
  (install-package-unless-installed 'git-commit-mode)
  (install-package-unless-installed 'haskell-mode))

; GREP
; ====

; Use Cygwin's find instead of Microsoft's find.
(defadvice grep-compute-defaults (around grep-compute-defaults-advice-null-device)
  "Use cygwin's find."
  (let ((null-device "/dev/null")
        (find-program "c:/cygwin64/bin/find.exe"))
     ad-do-it))
(ad-activate 'grep-compute-defaults)

(eval-after-load "grep"
  '(progn
     ;; Allow searching for agda files.
     (add-to-list 'grep-files-aliases
		  '("agda" . "*.agda *.lagda")
		  t)

     ;; Ignore cabal-dev sandbox directories.
     (add-to-list 'grep-find-ignored-directories
                  "cabal-dev")

     ;; Ignore agda interface files.
     (add-to-list 'grep-find-ignored-files
                  "*.agdai")))

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

; BUFFER NAMES
; ============

(require 'uniquify)

; TEX
; ===

(eval-after-load "tex"
  '(add-to-list 'TeX-command-list
                '("Make" "make --batch" TeX-run-TeX nil t :help "Call make.bat")
                '("Texify" "texify --batch --pdf --run-viewer %t" TeX-run-TeX nil t :help "Texify file and view result")))

; CUSTOM
; ======

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-PDF-mode t)
 '(TeX-save-query nil)
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
 '(haskell-compile-cabal-build-alt-command "cd %s & cabal clean -s && cabal build --ghc-option=-ferror-spans")
 '(haskell-compile-cabal-build-command "cd %s && cabal build --ghc-option=-ferror-spans")
 '(haskell-mode-hook (quote (turn-on-haskell-indentation turn-on-haskell-doc-mode)))
 '(haskell-program-name "ghci")
 '(indent-tabs-mode nil)
 '(inferior-haskell-wait-and-jump t)
 '(inhibit-startup-screen t)
 '(prolog-electric-colon-flag t)
 '(prolog-electric-dot-flag t)
 '(prolog-electric-dot-full-predicate-template t)
 '(prolog-electric-newline-flag t)
 '(prolog-electric-tab-flag t)
 '(prolog-electric-underscore-flag t)
 '(prolog-hungry-delete-key-flag t)
 '(prolog-program-name (quote (((getenv "EPROLOG") (eval (getenv "EPROLOG"))) (eclipse "eclipse") (mercury nil) (sicstus "sicstus") (swi "swipl") (gnu "gprolog") (t "prolog"))))
 '(prolog-system (quote swi))
 '(sbt:ansi-support (quote filter))
 '(sbt:program-name "sbt-inferior")
 '(tex-fontify-script nil)
 '(uniquify-buffer-name-style (quote forward) nil (uniquify)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "SystemWindow" :foreground "SystemWindowText" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 120 :width normal :family "FreeMono"))))
 '(fixed-pitch ((t (:family "FreeMono"))))
 '(italic ((t (:underline nil :slant italic))))
 '(variable-pitch ((t (:family "FreeSerif")))))

; ETAGS
; =====

; from http://www.emacswiki.org/emacs/EmacsTags
(defun view-tag-other-window (arg)
  "Same as `find-tag-other-window' but doesn't move the point"
  (interactive "P")
  (let ((window (get-buffer-window)))
    (if arg
        (find-tag-other-window nil t)
      (call-interactively 'find-tag-other-window))
    (recenter 0)
    (select-window window)))

(global-set-key (kbd "M-.") 'view-tag-other-window)

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

; SBT MODE
; ========

; Activate the sbt mode in a submode of the git repo.
(let ((sbt-mode-path (concat user-emacs-directory "sbt-mode")))
  (when (file-exists-p sbt-mode-path)
    (add-to-list 'load-path sbt-mode-path)
    (require 'sbt-mode)))

(add-hook 'sbt-mode-hook (lambda ()
  (local-set-key (kbd "C-a") 'comint-bol)
  (local-set-key (kbd "S-<return>") 'comint-accumulate)))

(add-hook 'scala-mode-hook (lambda ()
  (local-set-key (kbd "C-c C-l") 'sbt-run-previous-command))
  (local-set-key (kbd "M-.") 'sbt-find-definitions))

; ICICLE
; ======

(icy-mode)


; CODING SYSTEM
; =============

(prefer-coding-system 'utf-8)

; DISABLE SOME KEYBINDINGS
; ========================

(global-unset-key "\C-x\C-c")

; PAREN MATCHING
; ==============

(show-paren-mode 1)
(setq show-paren-style 'mixed)

; HASKELL MODE
; ============

(eval-after-load "haskell-mode"
  '(define-key haskell-mode-map (kbd "C-c C-c") 'haskell-compile))

(eval-after-load "haskell-cabal"
  '(define-key haskell-cabal-mode-map (kbd "C-c C-c") 'haskell-compile))
