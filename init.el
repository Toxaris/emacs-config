; -*- lexical-binding: t -*-

; OPEN INIT FILE
; ==============

(defun find-dot-emacs () (interactive)
  "Try to find and open the dot emacs file"
  (find-file "~/.emacs.d/init.el"))

; STARTUP
; =======

(tool-bar-mode -1)

(add-hook 'window-setup-hook
          (lambda () (w32-send-sys-command #xf030)))

(set-variable 'inhibit-startup-screen t)

; Start Server
; ============

(server-start)

; SEARCH PATH
; ===========

(defmacro add-all-to-list (lst &rest elms)
  `(progn ,@(mapcar (lambda (elm) `(add-to-list ,lst ,elm)) elms)))

(add-all-to-list 'load-path
  (concat user-emacs-directory "lisp")
  (concat user-emacs-directory "use-package"))

; USE-PACKAGE
; ===========

(require 'use-package)

; MELPA
; =====

(require 'package)
(add-to-list 'package-archives
  '("melpa" . "http://melpa.milkbox.net/packages/")
  'APPEND)

(package-initialize)

(use-package icicles
  :ensure t)

(use-package magit
  :ensure t)

(use-package dtrt-indent
  :ensure t
  :config
  (dtrt-indent-mode t))

; GREP
; ====

(defun check-candidate-find (program)
  "Check whether a program appears to be GNU find."
  (and (string-match "GNU find"
         (shell-command-to-string
           (combine-and-quote-strings
            (list program "--version"))))
       program))

(defvar gnu-find-program
  (or (check-candidate-find "find")
      (check-candidate-find "c:/cygwin64/bin/find.exe"))
  "The path to a version of GNU find.")


; Use Cygwin's find instead of Microsoft's find.
(defadvice grep-compute-defaults (around grep-compute-defaults-advice-null-device)
  "Use cygwin's find."
  (let ((null-device "/dev/null")
        (find-program gnu-find-program))
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

; BUFFER NAMES
; ============

(use-package uniquify
  :config
  (set-variable 'uniquify-buffer-name-style 'forward))

; CUA
; ===

(use-package cua-base
  :config
  (set-variable 'cua-auto-tabify-rectangles nil)
  (set-variable 'cua-enable-cua-keys t)
  (cua-mode))

; SPELL CHECKING
; ==============

(defun make-hunspell-program-wrapper (main-dict)
  (lambda (strings)
    (let* ((user-dict (concat main-dict ".user"))
           (user-dict-path (expand-file-name user-dict user-emacs-directory))
           (dicts (mapconcat #'identity (list main-dict user-dict-path) ",")))
    (with-temp-buffer
      (dolist (string strings)
        (insert string)
        (newline))
      (call-process-region (point-min) (point-max)
        "hunspell" t t nil
        "-w"
        "-d" dicts)
      (wcheck-parser-lines)))))

(use-package wcheck-mode
  :ensure t
  :defer t
  :config
  (add-to-list 'wcheck-language-data-defaults
    '(action-parser . wcheck-parser-ispell-suggestions))
  (add-all-to-list 'wcheck-language-data
    `("American English"
      (program . ,(make-hunspell-program-wrapper "en_US"))
      (action-program . "C:/ProgramData/chocolatey/bin/hunspell.exe")
      (action-args "-a" "-d" "en_US")))
  (set-variable 'wcheck--timer-idle 0.1)
  (wcheck-change-language "American English" t))

; TEX
; ===

(use-package tex-site
  :ensure auctex)

(defun guess-and-set-TeX-master ()
  (let ((guess (guess-TeX-master buffer-file-name)))
    (when guess
      (set-variable 'TeX-master guess))))

(defmacro with-all-buffers (body)
  (let ((buffer (gensym "buffer")))
    `(dolist (,buffer (buffer-list))
       (with-current-buffer ,buffer
         ,body))))

(defun guess-TeX-master (filename)
  "Guess the master file for FILENAME from currently open .tex files."
  (let* ((candidate nil)
         (filename (file-name-nondirectory filename))
         (basename (file-name-sans-extension filename))
         (pattern (mapconcat #'identity
                    (list (concat "\\\\input{" filename "}")
                          (concat "\\\\include{" basename "}")
                          (concat "^%include " filename))
                    "\\|")))
    (with-all-buffers
      (when (and buffer-file-name (string-match "\\.tex$" buffer-file-name))
        (save-excursion
          (goto-char (point-min))
          (when (re-search-forward pattern nil t)
            (setq candidate (or (and (stringp TeX-master)
                                     TeX-master)
                                buffer-file-name))))))
    candidate))

(defun lhs2tex-setup ()
  (TeX-auto-add-regexp
   '("^%include \\(\\.*[^#}%\\\\\\.\n\r]+\\)\\(\\.[^#}%\\\\\\.\n\r]+\\)?"
     1 TeX-auto-file)))

(use-package tex
  :defer
  :config
  (add-all-to-list 'TeX-command-list
    '("Make" "make.bat --batch" TeX-run-TeX nil t :help "Call make.bat")
    '("Texify" "texify --batch --pdf --run-viewer %t" TeX-run-TeX nil t :help "Texify file and view result"))
  (TeX-PDF-mode t)
  (set-variable 'tex-fontify-script nil)
  (set-variable 'TeX-save-query nil)
  (add-hook 'TeX-mode-hook 'disable-input-method)
  (add-hook 'TeX-mode-hook 'lhs2tex-setup)
  (add-hook 'TeX-mode-hook 'guess-and-set-TeX-master)
  (set-variable 'TeX-parse-self t)
  (set-variable 'TeX-auto-save t))

(use-package latex
  :defer
  :config
  (font-lock-add-keywords 'latex-mode
    `(("^\\(%include\\) \\(\\.*[^#}%\\\\\\.\n\r]+\\)\\(\\.[^#}%\\\\\\.\n\r]+\\)?"
       (1 font-lock-keyword-face prepend)
       (2 font-lock-constant-face prepend)
       (3 font-lock-constant-face prepend))
      (,(concat "^" (regexp-opt (list "%{" "%}" "%endif") t))
       (1 font-lock-keyword-face prepend))
      ("^\\(%if\\) \\([^\n\r]*\\)"
       (1 font-lock-keyword-face prepend)
       (2 font-lock-constant-face prepend)))))

(use-package font-latex
  :defer
  :config
  (add-all-to-list 'font-latex-syntactic-keywords-extra
    '("\\(@\\)\\([^@]*\\)\\(@\\)"
      (1 "\"" t)
      (2 "." t)
      (3 "\"" t))
    '("\\(|\\)\\([^|]*\\)\\(|\\)"
      (1 "\"" t)
      (2 "." t)
      (3 "\"" t))))

(use-package bibtex
  :defer
  :config
  (set-variable 'bibtex-align-at-equal-sign t)
  (set-variable 'bibtex-entry-format
    '(opts-or-alts required-fields numerical-fields whitespace realign last-comma delimiters strings sort-fields))
  (set-variable 'bibtex-text-indentation 18)
  (add-hook 'bibtex-mode-hook 'disable-input-method))

(use-package reftex
  :defer
  :config
  (add-all-to-list 'reftex-default-bibliography
    "bib/tsr.bib"
    "../bib/tsr.bib"
    "../../bib/tsr.bib"))

; CUSTOM
; ======

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(agda2-ghci-options (quote ("-package Agda-2.3.0")))
 '(agda2-include-dirs (quote ("." "c:\\Users\\Tillmann\\Documents\\GitHub\\agda-stdlib\\src")))
 '(compilation-auto-jump-to-first-error t)
 '(compilation-scroll-output (quote first-error))
 ; '(default-input-method "Agda")
 '(fill-column 65)
 '(icicle-Completions-text-scale-decrease 0.0)
 '(indent-tabs-mode nil)
 '(prolog-electric-colon-flag t)
 '(prolog-electric-dot-flag t)
 '(prolog-electric-dot-full-predicate-template t)
 '(prolog-electric-newline-flag t)
 '(prolog-electric-tab-flag t)
 '(prolog-electric-underscore-flag t)
 '(prolog-hungry-delete-key-flag t)
 '(prolog-program-name (quote (((getenv "EPROLOG") (eval (getenv "EPROLOG"))) (eclipse "eclipse") (mercury nil) (sicstus "sicstus") (swi "swipl") (gnu "gprolog") (t "prolog"))))
 '(prolog-system (quote swi))
 '(sbt:ansi-support (quote filter)))

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

; PTS MODE
; ========

(let ((pts-mode-path (shell-command-to-string "pts --locate-emacs-mode")))
  (when (file-exists-p pts-mode-path)
    (add-to-list 'load-path pts-mode-path)
    (require 'pts-mode)))

; SBT MODE
; ========

; Activate sbt-mode in a submodule of the git repo.
(let ((sbt-mode-path (concat user-emacs-directory "sbt-mode")))
  (when (file-exists-p sbt-mode-path)
    (add-to-list 'load-path sbt-mode-path)
    (require 'sbt-mode)
    (set-variable 'sbt:program-name (concat user-emacs-directory "sbt-inferior.bat"))))

(add-hook 'sbt-mode-hook (lambda ()
  (local-set-key (kbd "C-a") 'comint-bol)
  (local-set-key (kbd "S-<return>") 'comint-accumulate)))

(add-hook 'scala-mode-hook (lambda ()
  (local-set-key (kbd "C-c C-l") 'sbt-run-previous-command)
  (local-set-key (kbd "M-.") 'sbt-find-definitions)))

; UROBORO MODE
; ============

; Activate uroboro-mode in a submodule of the git rep.
(let ((uroboro-mode-path (concat user-emacs-directory "uroboro-mode")))
  (when (file-exists-p uroboro-mode-path)
    (add-to-list 'load-path uroboro-mode-path)
    (require 'uroboro-mode)))

; SCALA MODE
; ==========

; Activate scala-mode2 in a submodule of the git repo.
(let ((scala-mode2-path (concat user-emacs-directory "scala-mode2")))
  (when (file-exists-p scala-mode2-path)
    (add-to-list 'load-path scala-mode2-path)
    (require 'scala-mode2)))

(add-hook 'scala-mode-hook (lambda()
  (local-set-key (kbd "RET") (lambda ()
    (interactive)
    (newline-and-indent)
    (scala-indent:insert-asterisk-on-multiline-comment)))))

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

; HASKELL
; =======

(use-package haskell-mode
  :ensure t
  :defer
  :config
  (define-key haskell-mode-map (kbd "C-c C-c") 'haskell-compile)
  (add-hook 'haskell-mode-hook 'turn-on-haskell-decl-scan)
  (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
  (add-hook 'haskell-mode-hook 'haskell-auto-insert-module-template))

(use-package haskell-cabal
  :defer
  :config
  (define-key haskell-cabal-mode-map (kbd "C-c C-c") 'haskell-compile)
  (define-key haskell-cabal-mode-map (kbd "M-.")
    'haskell-cabal-find-or-create-source-file))

(use-package haskell-compile
  :defer
  :config
  (set-variable 'haskell-compile-cabal-build-command
    "cd %s && stack build --ghc-options \"-ferror-spans\"")
  (set-variable 'haskell-compile-cabal-build-alt-command
    "cd %s & cabal clean -s && cabal build --ghc-option=-ferror-spans"))

(require 'speedbar)
(speedbar-add-supported-extension ".hs")
(speedbar-add-supported-extension ".lhs")
(speedbar-add-supported-extension ".hsc")

; JEKYLL
; ======

; TODO: how to activate only for jekyll projects
; TODO: is this what I want?!
(use-package jekyll-modes
  :ensure t
  :mode ("\\.md$" . jekyll-markdown-mode)
  :mode ("\\.html" . jekyll-html-mode))

; TRANSPOSE FRAME
; ===============

; Activate transpose-frame in a submodule of the git repo.
(let ((transpose-frame-path (concat user-emacs-directory "transpose-frame")))
  (when (file-exists-p transpose-frame-path)
    (add-to-list 'load-path transpose-frame-path)
    (require 'transpose-frame)))

; DIFF MODE
; =========

; Highlight trailing whitespace
(add-hook 'diff-mode-hook (lambda ()
  (set-variable 'show-trailing-whitespace t)))

; SPEED BAR
; =========

; Bind s-s to show the speedbar as a window in the same frame
(global-set-key (kbd "C-c C-s") 'sr-speedbar-toggle)

; LISP INDENTATION
; ================

(put 'add-to-list 'lisp-indent-function 1)
(put 'add-all-to-list 'lisp-indent-function 1)
(put 'add-hook 'lisp-indent-function 1)
(put 'set-variable 'lisp-indent-function 1)
(put 'with-all-buffers 'lisp-indent-function 0)

; NO-LONGER DISABLED COMMANDS
; ===========================

(put 'downcase-region 'disabled nil)
