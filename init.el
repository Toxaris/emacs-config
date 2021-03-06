; -*- lexical-binding: t -*-

;;;; important configuration

;; done first so that it is available even if there is an error
;; further down in the initialization file.

;;; command to open this initialization file

(defun find-dot-emacs () (interactive)
  "Try to find and open the dot emacs file"
  (find-file "~/.emacs.d/init.el"))

;;; startup

(tool-bar-mode -1)

(add-hook 'window-setup-hook
          (lambda () (w32-send-sys-command #xf030)))

(set-variable 'inhibit-startup-screen t)

(server-start)

;;; lock files

(set-variable 'create-lockfiles nil)

;;; modifier keys

(setq w32-pass-lwindow-to-system nil 
      w32-pass-rwindow-to-system nil 
      w32-pass-apps-to-system nil
      w32-lwindow-modifier 'super ; Left Windows key 
      w32-rwindow-modifier 'super ; Right Windows key 
      w32-apps-modifier 'hyper) ; Menu key

;;; coding system

(prefer-coding-system 'utf-8)

;;; fonts

(set-face-font 'default "Source Code Pro-16")
(set-face-font 'variable-pitch "Source Sans Pro")

;; ‐
(set-fontset-font t (cons #x2010 #x2010) "Source Code Pro")

;; ‒ – — ― ‖ ‗ ‘ ’ ‚ ‛ “ ” „
(set-fontset-font t (cons #x2012 #x201E) "Source Code Pro")

;; † ‡ •
(set-fontset-font t (cons #x2020 #x2022) "Source Code Pro")

;; …
(set-fontset-font t (cons #x2026 #x2026) "Source Code Pro")

;; ‰
(set-fontset-font t (cons #x2030 #x2030) "Source Code Pro")

;; ′ ″
(set-fontset-font t (cons #x2032 #x2033) "Source Code Pro")

;; ‵
(set-fontset-font t (cons #x2035 #x2035) "Source Code Pro")

;; ‹ › ※ ‼ ‽ ‾ ‿
(set-fontset-font t (cons #x2039 #x203F) "Source Code Pro")

;; ⁄
(set-fontset-font t (cons #x2044 #x2044) "Source Code Pro")

;; ⁰ ⁱ
(set-fontset-font t (cons #x2070 #x2071) "Source Code Pro")

;; ⁴ ⁵ ⁶ ⁷ ⁸ ⁹
(set-fontset-font t (cons #x2074 #x207E) "Source Code Pro")

;; ⁽ ⁾
(set-fontset-font t (cons #x207D #x207E) "Source Code Pro")

;; ₀ ₁ ₂ ₃ ₄ ₅ ₆ ₇ ₈ ₉
(set-fontset-font t (cons #x2080 #x2089) "Source Code Pro")

;; ₍ ₎
(set-fontset-font t (cons #x208D #x208E) "Source Code Pro")

;; ⅐ ⅑ ⅒ ⅓ ⅔ ⅕ ⅖ ⅗ ⅘ ⅙ ⅚ ⅛ ⅜ ⅝ ⅞
(set-fontset-font t (cons #x2150 #x215E) "Source Code Pro")

;; ← ↑ → ↓ ↔ ↕ ↖ ↗ ↘ ↙
(set-fontset-font t (cons #x2190 #x2199) "Source Code Pro")

;; ⇐ ⇑ ⇒ ⇓
(set-fontset-font t (cons #x21D0 #x21D3) "Source Code Pro")

;; ∀
(set-fontset-font t (cons #x2200 #x2200) "Source Code Pro")

;; ∂ ∃
(set-fontset-font t (cons #x2202 #x2203) "Source Code Pro")

;; ∆
(set-fontset-font t (cons #x2206 #x2206) "Source Code Pro")

;; ∏
(set-fontset-font t (cons #x220F #x220F) "Source Code Pro")

;; ∑
(set-fontset-font t (cons #x2211 #x2211) "Source Code Pro")

;; −
(set-fontset-font t (cons #x2212 #x2212) "Source Code Pro")

;; ∕
(set-fontset-font t (cons #x2215 #x2215) "Source Code Pro")

;; ∙ √
(set-fontset-font t (cons #x2219 #x221A) "Source Code Pro")

;; ∞ ∟
(set-fontset-font t (cons #x221E #x221F) "Source Code Pro")

;; ∥
(set-fontset-font t (cons #x2225 #x2225) "Source Code Pro")

;; ≈
(set-fontset-font t (cons #x2248 #x2248) "Source Code Pro")

;; ≠ ≡
(set-fontset-font t (cons #x2260 #x2261) "Source Code Pro")

;; ≤ ≥
(set-fontset-font t (cons #x2264 #x2265) "Source Code Pro")

;; ⌐
(set-fontset-font t (cons #x2310 #x2310) "Source Code Pro")

;; ⌒
(set-fontset-font t (cons #x2312 #x2312) "Source Code Pro")

;; ⌜ ⌝ ⌞ ⌟

(set-fontset-font t (cons #x231C #x231F) "Source Code Pro")

;; ⌠ ⌡
(set-fontset-font t (cons #x2320 #x2321) "Source Code Pro")

;; ┡┅┅┅╾╼╃
;; ╰╖╭╌╌╼┙
;;  ╚╡
(set-fontset-font t (cons #x2500 #x257F) "Source Code Pro")

;; ⟤ ⟥ ⟦ ⟧ ⟨ ⟩ ⟪ ⟫
(set-fontset-font t (cons #x27E4 #x27EB) "FreeMono")

;;; ANSI colors in compilation buffers

;; https://stackoverflow.com/a/3072831
(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region (point-min) (point-max))))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

;;; helper functions and macros

(defmacro add-all-to-list (lst &rest elms)
  `(progn ,@(mapcar (lambda (elm) `(add-to-list ,lst ,elm)) elms)))

;; based on http://www.lunaryorn.com/2016/04/28/fullscreen-magit-status.html
;; and magit--display-buffer-fullframe in magit-mode.el
(defun display-buffer-full-frame (buffer alist)
  "Display BUFFER in fullscreen.

ALIST is a `display-buffer' ALIST.

Return the new window for BUFFER."
  (let ((window (or (display-buffer-reuse-window buffer alist)
                    (display-buffer-same-window buffer alist)
                    (display-buffer-pop-up-window buffer alist)
                    (display-buffer-use-some-window buffer alist))))
    (when window
      (delete-other-windows window))
    window))

;;; search path

(add-all-to-list 'load-path
  (concat user-emacs-directory "lisp")
  (concat user-emacs-directory "use-package"))

;;; package setup

(require 'use-package)

(use-package package
  :config
  (add-to-list 'package-archives
    '("melpa" . "http://melpa.milkbox.net/packages/")
    'APPEND)
  (package-initialize))

;;; CUA

(use-package cua-base
  :config
  (set-variable 'cua-auto-tabify-rectangles nil)
  (set-variable 'cua-enable-cua-keys t)
  (cua-mode))

;;; icicles

(use-package icicles
  :ensure t
  :config
  (icy-mode)
  (set-variable 'icicle-Completions-text-scale-decrease 0.0))

;;;; general configuration

;;; disable some keybindings

(global-unset-key "\C-x\C-c")

;;; un-disable commands

(put 'downcase-region 'disabled nil)

;;; zooming

(global-set-key (kbd "<C-wheel-up>") 'text-scale-increase)
(global-set-key (kbd "<C-wheel-down>") 'text-scale-decrease)

;;; paren matching

(show-paren-mode 1)
(setq show-paren-style 'mixed)

;;; scrolling

(use-package sublimity-scroll
  :load-path "sublimity"
  :ensure sublimity
  :config
  (set-variable 'sublimity-scroll-weight 10)
  (set-variable 'sublimity-scroll-drift-length 6)
  (sublimity-mode t))

(set-variable 'mouse-wheel-scroll-amount '(15 ((shift) . nil)))
(set-variable 'mouse-wheel-progressive-speed nil)

;;; dtrt-indent

(use-package dtrt-indent
  :ensure t
  :config
  (dtrt-indent-mode t))

;;; transpose frame

;; Activate transpose-frame in a submodule of the git repo.
(let ((transpose-frame-path (concat user-emacs-directory "transpose-frame")))
  (when (file-exists-p transpose-frame-path)
    (add-to-list 'load-path transpose-frame-path)
    (require 'transpose-frame)))

;;; speed bar

;; Bind s-s to show the speedbar as a window in the same frame
(global-set-key (kbd "C-c C-s") 'sr-speedbar-toggle)

;;; magit

(use-package magit
  :ensure t
  :config
  (global-set-key (kbd "C-x g") 'magit-status)
  (add-all-to-list 'display-buffer-alist
    `(,(rx "*magit: ")
      (display-buffer-full-frame))))

;;; vc-git

(use-package vc-git
  :defer t
  :bind ("M-s g" . vc-git-grep))

;;; grep

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

;; Use Cygwin's find instead of Microsoft's find.
(defadvice grep-compute-defaults (around grep-compute-defaults-advice-null-device)
  "Use cygwin's find."
  (let ((null-device "/dev/null")
        (find-program gnu-find-program))
     ad-do-it))
(ad-activate 'grep-compute-defaults)

(use-package grep
  :defer t
  :bind ("M-s M-g" . lgrep)

  :config
  ;; Allow searching for agda files.
  (add-to-list 'grep-files-aliases
    '("agda" . "*.agda *.lagda")
    t)

  ;; Ignore cabal-dev sandbox directories.
  (add-to-list 'grep-find-ignored-directories
    "cabal-dev")

  ;; Ignore agda interface files.
  (add-to-list 'grep-find-ignored-files
    "*.agdai"))

;;; multi-occur-in-this-mode

;; based on
;; https://www.masteringemacs.org/article/searching-buffers-occur-mode

(defun get-buffers-matching-mode (mode)
  "Returns a list of buffers where their major-mode is equal to MODE"
  (let ((buffer-mode-matches '()))
   (dolist (buf (buffer-list))
     (with-current-buffer buf
       (if (eq mode major-mode)
           (add-to-list 'buffer-mode-matches buf))))
   buffer-mode-matches))

(defun multi-occur-in-this-mode ()
  "Show all lines matching REGEXP in buffers with this major mode."
  (interactive)
  (let ((args (occur-read-primary-args)))
    (multi-occur
      (get-buffers-matching-mode major-mode)
      (car args)
      (cadr args))))

(global-set-key (kbd "M-s M-o") 'multi-occur-in-this-mode)

;;; generic mode

(require 'generic-x)

;;; input method

(defun disable-input-method ()
  (when current-input-method
    (toggle-input-method)))

(defun enable-input-method ()
  (when (not current-input-method)
    (toggle-input-method)))

(add-hook 'text-mode-hook 'enable-input-method)

;;; buffer names

(use-package uniquify
  :config
  (set-variable 'uniquify-buffer-name-style 'forward))

;;; spell checking

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

;;; etags

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

;;;; custom

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(agda2-ghci-options (quote ("-package Agda-2.3.0")))
 '(agda2-include-dirs
   (quote
    ("." "c:\\Users\\Tillmann\\Documents\\GitHub\\agda-stdlib\\src")))
 '(compilation-auto-jump-to-first-error t)
 '(compilation-scroll-output (quote first-error))
 '(fill-column 65)
 '(indent-tabs-mode nil)
 '(prolog-electric-colon-flag t)
 '(prolog-electric-dot-flag t)
 '(prolog-electric-dot-full-predicate-template t)
 '(prolog-electric-newline-flag t)
 '(prolog-electric-tab-flag t)
 '(prolog-electric-underscore-flag t)
 '(prolog-hungry-delete-key-flag t)
 '(prolog-program-name
   (quote
    (((getenv "EPROLOG")
      (eval
       (getenv "EPROLOG")))
     (eclipse "eclipse")
     (mercury nil)
     (sicstus "sicstus")
     (swi "swipl")
     (gnu "gprolog")
     (t "prolog"))))
 '(prolog-system (quote swi))
 '(sbt:ansi-support (quote filter)))

;;; yasnippets

(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode 1))

(defun company-backend-add-yasnippet (backend)
  (if (listp backend)
      (if (member 'company-yasnippet backend)
          backend
        `(,@backend :with company-yasnippet))
    `(,backend :with company-yasnippet)))

;;; company

(use-package company
  :ensure t
  :config
  (set-variable 'company-frontends
    '(company-pseudo-tooltip-frontend
      company-echo-metadata-frontend))
  (add-hook 'after-init-hook
    (lambda ()
      (global-company-mode)
      (set-variable 'company-backends
        (mapcar #'company-backend-add-yasnippet company-backends)))))

;;;; modes for specific file formats and computer languages

;;; doc-view

;; based on https://github.com/lunaryorn/.emacs.d
;; by Sebastian Wiesner, GPLv3

(use-package doc-view
  :defer t
  :config
  (setq doc-view-resolution 300)
  (defconst doc-view-mutool-program "mutool")

  (defun doc-view-pdf->png-converter-mutool (pdf png page callback)
    "Convert a PDF file to PNG at PAGE.

After conversion invoke CALLBACK.  See `doc-view-start-process'
for more information about CALLBACK."
    (doc-view-start-process
     "pdf->png" doc-view-mutool-program
     `("draw"
       ,(concat "-o" png)
       ,(format "-r%d" (round doc-view-resolution))
       ,pdf
       ,@(if page `(,(format "%d" page))))
     callback))

  ;; If `mutool' exists use our own converter function to call "mutool draw".
  ;; Otherwise check whether docview found mudraw and warn if it didn't
  (if (executable-find doc-view-mutool-program)
      (setq doc-view-pdf->png-converter-function
            #'doc-view-pdf->png-converter-mutool)
    ;; Warn if Doc View falls back to Ghostscript for rendering
    (unless (eq doc-view-pdf->png-converter-function
                'doc-view-pdf->png-converter-mupdf)
      (warn "Doc View is not using mupdf!"))))

;;; TeX

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
    '("Texify" "texify --batch --pdf %S %t" TeX-run-TeX nil t :help "Texify file and view result"))
  (TeX-PDF-mode t)
  (set-variable 'tex-fontify-script nil)
  (set-variable 'TeX-save-query nil)
  (add-hook 'TeX-mode-hook 'disable-input-method)
  (add-hook 'TeX-mode-hook 'lhs2tex-setup)
  (add-hook 'TeX-mode-hook 'guess-and-set-TeX-master)
  (add-hook 'TeX-mode-hook 'turn-on-reftex)
  (let ((sumatra-pdf-path "C:/Program Files/SumatraPDF/SumatraPDF.exe"))
    (when (file-exists-p sumatra-pdf-path)
      (set-variable 'TeX-source-correlate-mode t)
      (set-variable 'TeX-source-correlate-method 'synctex)
      (add-to-list 'TeX-view-program-list
        `("SumatraPDF"
          ("\""
           ,sumatra-pdf-path
           "\" -reuse-instance"
           (mode-io-correlate " -forward-search \"%b\" %n")
           " %o")))
      (assq-delete-all 'output-pdf TeX-view-program-selection)
      (add-to-list 'TeX-view-program-selection '(output-pdf "SumatraPDF"))))
  (set-variable 'TeX-parse-self t)
  (set-variable 'TeX-auto-save t))

(use-package latex
  :defer
  :config
  (font-lock-add-keywords 'latex-mode
    `(("^\\(%include\\)[ \t]+\\(\\.*[^#}%\\\\\\.\n\r]+\\)\\(\\.[^#}%\\\\\\.\n\r]+\\)?"
       (1 font-lock-preprocessor-face prepend)
       (2 font-lock-constant-face prepend)
       (3 font-lock-constant-face prepend))
      (,(concat "^" (regexp-opt (list "%{" "%}" "%endif" "%else") t))
       (1 font-lock-preprocessor-face prepend))
      ("^\\(%format\\)[ \t]+\\(\\([^ \t\n\r]+[0-9']\\)\\|\\([^ \t\n\r]*[^ \t\n\r0-9']\\)\\)\\(\\([ \t]+\\([^= \t\n\r]\\|[^ \t\n\r][^ \t\n\r]+\\)\\)*\\)[ \t]*$"
       (1 font-lock-preprocessor-face prepend)
       (3 font-lock-function-name-face prepend t)
       (4 font-lock-warning-face prepend t)
       (5 font-lock-warning-face prepend t))
      ("^\\(%format\\)[ \t]+\\(\\([^ \t\n\r]+[ \t]+\\)+\\)\\(=\\)\\([ \t]\\|$\\)"
       (1 font-lock-preprocessor-face prepend)
       (2 font-lock-function-name-face prepend)
       (4 font-lock-preprocessor-face prepend)
       ("\\(\"[^\"\r\n]*\"\\)\\|\\([^\" \t\r\n]+\\)\\|\\(\"[^\"\n\r]*$\\)" nil nil
         (1 font-lock-string-face prepend t)
         (2 font-lock-function-name-face prepend t)
         (3 font-lock-warning-face prepend t)))
      ("^\\(%if\\)[ \t]+\\([^\n\r]*\\)"
       (1 font-lock-preprocessor-face prepend)
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
    "../../bib/tsr.bib")
  (setq reftex-plug-into-AUCTeX t))

;;; agda

;; Prepare for loading Agda mode and load Agda input method
(let ((agda-mode-path
       (let ((coding-system-for-read 'utf-8))
         (shell-command-to-string "agda-mode locate"))))
  (when (file-exists-p agda-mode-path)
    (load agda-mode-path)
    (require 'agda-input)
    (set-variable 'default-input-method "Agda")
    (require 'two-mode-mode)
    (add-to-list 'auto-mode-alist
                 '("\\.lagda\\'" . two-mode-mode))

    (use-package agda2-highlight
      :defer t
      :config
      (set-face-attribute 'agda2-highlight-keyword-face nil
                          :family "Source Code Pro Medium"))))


;;; pts

(let ((pts-mode-path (shell-command-to-string "pts --locate-emacs-mode")))
  (when (file-exists-p pts-mode-path)
    (add-to-list 'load-path pts-mode-path)
    (require 'pts-mode)))

;;; sbt

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

;;; uroboro

; Activate uroboro-mode in a submodule of the git rep.
(let ((uroboro-mode-path (concat user-emacs-directory "uroboro-mode")))
  (when (file-exists-p uroboro-mode-path)
    (add-to-list 'load-path uroboro-mode-path)
    (require 'uroboro-mode)))

;;; scala

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

;;; groovy

(use-package groovy-mode
  :ensure t
  :defer)

;;; elm

(use-package elm-mode
  :ensure t
  :defer)

;;; haskell

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

;;; jekyll

;; TODO: how to activate only for jekyll projects
;; TODO: is this what I want?!
(use-package jekyll-modes
  :ensure t
  :mode ("\\.md$" . jekyll-markdown-mode)
  :mode ("\\.html" . jekyll-html-mode)
  :config
  (use-package markdown-mode
    :ensure t)
  (use-package yaml-mode
    :ensure t))

;;; diff

;; Highlight trailing whitespace
(add-hook 'diff-mode-hook (lambda ()
  (set-variable 'show-trailing-whitespace t)))

;;; lisp

(put 'add-to-list 'lisp-indent-function 1)
(put 'add-all-to-list 'lisp-indent-function 1)
(put 'add-hook 'lisp-indent-function 1)
(put 'set-variable 'lisp-indent-function 1)
(put 'with-all-buffers 'lisp-indent-function 0)
