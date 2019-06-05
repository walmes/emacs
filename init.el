;;======================================================================
;; Configuration file to Emacs (>=25.2.2) by Walmes Zeviani.
;;
;; This file is hosted at https://github.com/walmes/emacs.
;;
;; Almost all the content available here was obtained/inspired by
;; queries on the internet. Please, send questions, problems and/or
;; suggestions as an issue on GitHub project of this file.
;;======================================================================

;; http://www.emacswiki.org/wiki/EmacsNiftyTricks
;; “I’ve used Emacs for many years now, but have never reached its
;;    maximum potential.” -- Anon.
;;
;; http://www.mygooglest.com/fni/dot-emacs.html
;; “Show me your ~/.emacs and I will tell
;;    you who you are.” -- Bogdan Maryniuk.
;;
;; https://www.emacswiki.org/emacs/EmacsKoans
;; “-- Master, does Emacs have buddha-nature?
;;  -- I can't se why not, it has everything else.”

;;----------------------------------------------------------------------
;; Basic definitions.
;;----------------------------------------------------------------------

(setq user-full-name "Walmes Zeviani"
      user-mail-address "walmeszeviani")

;; Add directory with supplementary configuration files.
(add-to-list 'load-path "~/.emacs.d/lisp/")

(global-hl-line-mode 1)             ;; Highlight the cursor line.
(visual-line-mode 1)                ;; Screen lines, not logical lines.
(show-paren-mode 1)                 ;; Highlight matching pairs.
(delete-selection-mode 1)           ;; Allows delete region.
(setq column-number-mode t)         ;; Show cursor position.
(setq auto-save-default nil)        ;; Turn off #autosave#.
(setq make-backup-files nil)        ;; Turn off backup~.
(setq comment-empty-lines t)        ;; Comment even in empty lines.
(setq x-select-enable-clipboard t)  ;; Allow shared transfer area.
(setq-default indent-tabs-mode nil) ;; Spaces to indent.
(setq-default fill-column 72)       ;; Column width.
;; (setq-default auto-fill-function
;;               'do-auto-fill)        ;; Auto break long lines.

;; Highlight whitespace.
(setq whitespace-line-column fill-column)
(setq whitespace-style
      '(face lines-tail trailing tabs empty))
(global-whitespace-mode +1)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Open Emacs without start-up screen.
(setq inhibit-startup-screen t)
(add-hook 'emacs-startup-hook 'delete-other-windows)[/code]

;; To debug on errors.
;; (setq debug-on-error t)

;; Font and size.
(cond
 ((find-font (font-spec :name "Inconsolata"))
  (set-default-font "Inconsolata-14"))
 ((find-font (font-spec :name "Noto Sans Mono"))
  (set-default-font "Noto Sans Mono-14"))
 (t
  (set-default-font "Ubuntu Mono-14")))

;; Turn ido-mode on.
(ido-mode t)

;;----------------------------------------------------------------------
;; Key bindings.
;;----------------------------------------------------------------------

;; C-page down e C-page up to move along buffers.
(global-set-key (kbd "C-<next>") 'next-buffer)
(global-set-key (kbd "C-<prior>") 'previous-buffer)

;; C-z to 'undo, the default is C-/.
(global-unset-key "\C-z")
(global-set-key "\C-z" 'undo)

;; M-. to (un)comment paragraph.
(global-set-key [?\M-.] (kbd "M-h M-; M-}"))

;; M-+ to indent paragraph.
(global-set-key [?\M-+] (kbd "M-h C-M-\\"))

;; "C-~" to keep one white space between objects around point.
(global-set-key (kbd "<C-dead-tilde>") 'fixup-whitespace)

;; "M-~" to joint lines.
(global-set-key (kbd "<M-dead-tilde>") 'delete-indentation)

;; S-F11 and S-F12 to show/hide menu bar and tool bar.
(global-set-key (kbd "<S-f11>") 'toggle-menu-bar-mode-from-frame)
(global-set-key (kbd "<S-f12>") 'toggle-tool-bar-mode-from-frame)

;;----------------------------------------------------------------------
;; My functions.
;;----------------------------------------------------------------------

;; Byte compile file.
;; (byte-compile-file "~/.emacs.d/lisp/funcs.el")

(require 'funcs)

;;----------------------------------------------------------------------
;; Extensions.
;;----------------------------------------------------------------------

(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list 'package-archives
               '("melpa" . "http://melpa.org/packages/") t))

(package-initialize)

;;----------------------------------------------------------------------
;; Packages.
;;----------------------------------------------------------------------

;;----------------------------------------------------------------------
;; use-package: to tidy .emacs definitions.
;; https://github.com/jwiegley/use-package
;; IMPORTANT: https://jwiegley.github.io/use-package/keywords/

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(setq use-package-verbose t)
(setq use-package-always-ensure nil)

(require 'use-package)

;;----------------------------------------------------------------------
;; Color themes. Options > Customize Emacs > Custom Themes
;; https://emacsthemes.com/

;; M-x disable-theme
;; M-x load-theme RET <theme> RET

;; Download NK57 from https://www.1001fonts.com/nk57-monospace-font.html
;;                    https://www.dafont.com/nk57-monospace.font
;; Download PF Din from https://www.gfxfile.com/3tb/PF_Din_Mono.7z
;; Download M+ 1m Font from: https://www.fontspace.com/m-fonts/m-1m
;; Download M+ 2m Font from: https://www.fontspace.com/m-fonts/m-2m
;; Download M+ 1mn Font from: https://www.fontspace.com/m-fonts/m-1mn

;; Select conditional on computer name.
(cond
 ((string-equal system-name "youngest")
  (use-package
    gotham-theme :init (load-theme 'gotham t)
    (set-default-font "NK57 Monospace-16")
    :defer t))
 ((string-equal system-name "aqua")
  (use-package
    flatui-theme :init (load-theme 'flatui t)
    (set-default-font "Noto Sans Mono CJK SC-20")
    :defer t))
 ((string-equal system-name "ulisses")
  (use-package
    molokai-theme :init (load-theme 'molokai t)
    ;; (set-default-font "PFDin-16")
    (set-default-font "M+ 2m-16")
    :defer t))
 (t
  (use-package
    ;; Choose only one.
    ;; Light. -------------------------------
    ;; apropospriate-theme :init (load-theme 'apropospriate-light t)
    ;; flatui-theme :init (load-theme 'flatui t)
    ;; leuven-theme :init (load-theme 'leuven t)
    solarized-theme :init (load-theme 'solarized-light t)
    ;; spacemacs-theme :init (load-theme 'spacemacs-light t)
    ;; Dark. --------------------------------
    ;; flatland-theme :init (load-theme 'flatland t)
    ;; afternoon-theme :init (load-theme 'afternoon t)
    ;; apropospriate-theme :init (load-theme 'apropospriate-dark t)
    ;; monokai-theme :init (load-theme 'monokai t)
    ;; gotham-theme :init (load-theme 'gotham t)
    ;; molokai-theme :init (load-theme 'molokai t)
    ;; solarized-theme :init (load-theme 'solarized-dark t)
    ;; spacemacs-theme :init (load-theme 'spacemacs-dark t)
    :defer t)))

;;----------------------------------------------------------------------
;; helm.
;; http://tuhdo.github.io/helm-intro.html

(use-package helm
  :diminish helm-mode
  :init ;;-----------------------------------
  (progn
    (require 'helm-config)
    (when (executable-find "curl")
      (setq helm-google-suggest-use-curl-p t))
    (when (executable-find "ack-grep")
      (setq helm-grep-default-command
            "ack-grep -Hn --no-group --no-color %e %p %f"
            helm-grep-default-recurse-command
            "ack-grep -H --no-group --no-color %e %p %f"))
    (setq helm-split-window-in-side-p           t
          helm-move-to-line-cycle-in-source     t
          helm-ff-search-library-in-sexp        t
          helm-scroll-amount                    8
          helm-ff-file-name-history-use-recentf t
          helm-M-x-fuzzy-match                  t
          helm-buffers-fuzzy-matching           t
          helm-recentf-fuzzy-match              t
          helm-locate-fuzzy-match               t
          helm-apropos-fuzzy-match              t
          helm-lisp-fuzzy-completion            t
          helm-semantic-fuzzy-match             t
          helm-imenu-fuzzy-match                t)
    (helm-mode)
    (helm-autoresize-mode t)
    )
  :bind
  (("C-c h"     . helm-mini)
   ("C-h a"     . helm-apropos)
   ("C-x C-b"   . helm-buffers-list)
   ("C-x b"     . helm-buffers-list)
   ("M-y"       . helm-show-kill-ring)
   ("M-x"       . helm-M-x)
   ("C-x c o"   . helm-occur)
   ("C-x c s"   . helm-swoop)
   ("C-x c y"   . helm-yas-complete)
   ("C-x c Y"   . helm-yas-create-snippet-on-region)
   ("C-x c b"   . my/helm-do-grep-book-notes)
   ("C-x c SPC" . helm-all-mark-rings)))

;;----------------------------------------------------------------------
;; Company.

;; (when (not (package-installed-p 'company))
;;   (package-install 'company))

(use-package company
  :init
  (setq company-idle-delay             0
        company-minimum-prefix-length  2
        ;; company-require-match          nil
        ;; company-dabbrev-ignore-case    nil
        ;; company-dabbrev-downcase       nil
        ;; company-frontends              '(company-pseudo-tooltip-frontend)
        )
  (global-company-mode t)
  :config
  (add-hook 'prog-mode-hook 'company-mode))

;;----------------------------------------------------------------------
;; To work the accents on Sony Vaio.

(use-package iso-transl
  :ensure nil)

;;----------------------------------------------------------------------
;; Magit.

;; (when (not (package-installed-p 'magit))
;;   (package-install 'magit))

(use-package magit
  :bind
  ("C-c g" . magit-status))

;;----------------------------------------------------------------------
;; essh.el - ESS like shell mode. To eval line/regions in Emacs shell.

;; Byte compile file.
;; (byte-compile-file "~/.emacs.d/lisp/essh.el")

(use-package essh
  :ensure nil
  :config
  (add-hook
   'sh-mode-hook
   '(lambda ()
      (define-key sh-mode-map "\C-c\C-r" 'pipe-region-to-shell)
      (define-key sh-mode-map "\C-c\C-b" 'pipe-buffer-to-shell)
      (define-key sh-mode-map "\C-c\C-j" 'pipe-line-to-shell)
      (define-key sh-mode-map "\C-c\C-n" 'pipe-line-to-shell-and-step)
      (define-key sh-mode-map "\C-c\C-f" 'pipe-function-to-shell)
      (define-key sh-mode-map "\C-c\C-d" 'shell-cd-current-directory))))

;;----------------------------------------------------------------------
;; Bookmark-plus.

;; (when (not (package-installed-p 'bookmark+))
;;   (package-install 'bookmark+))

;; Byte compile file.  Faster load and execution.
;; http://ergoemacs.org/emacs/emacs_byte_compile.html
;; (byte-recompile-directory "~/.emacs.d/elpa/bookmark+" 0 t)

(use-package bookmark+
  :ensure nil
  :load-path "~/.emacs.d/elpa/bookmark+"
  :init
  (setq bookmark-default-file "~/Dropbox/bookmarks"
        bookmark-save-flag 1)
  :config
  ;; ATTENTION: for some unknown reason, the keymap must be defined in
  ;; `:config' because in `:bind' the bookmark list buffer have a
  ;; different appearance.
  (progn
    ;; Create an autonamed bookmark.
    (global-set-key (kbd "<C-f3>")
                    'bmkp-toggle-autonamed-bookmark-set/delete)
    ;; Go to the next bookmark in file.
    (global-set-key (kbd "<f3>")
                    'bmkp-next-bookmark-this-file/buffer-repeat)
    ;; Go to the previous bookmark in file.
    (global-set-key (kbd "<f4>")
                    'bmkp-previous-bookmark-this-file/buffer-repeat)
    ;; Toggle temporary/permanent bookmark.
    (global-set-key (kbd "<S-f3>")
                    'bmkp-toggle-temporary-bookmark)
    ))

;;----------------------------------------------------------------------
;; Visible bookmarks. Easy movement.
;; https://marmalade-repo.org/packages/bm

;; (when (not (package-installed-p 'bm))
;;   (package-install 'bm))

(use-package bm
  :config
  (setq bm-marker 'bm-marker-left
        bm-highlight-style 'bm-highlight-only-fringe)
  :bind
  (("<C-f2>" . bm-toggle)
   ("<f2>"   . bm-next)
   ("<S-f2>" . bm-previous)))

;;----------------------------------------------------------------------
;; Folding code blocks based on indentation.
;; git clone https://github.com/zenozeng/yafolding.el.git

;; (when (not (package-installed-p 'yafolding))
;;   (package-install 'yafolding))

(use-package yafolding
  :bind
  (("C-{" . yafolding-hide-parent-element)
   ("C-}" . yafolding-toggle-element)))

;;----------------------------------------------------------------------
;; Smart Parenthesis.
;; https://github.com/Fuco1/smartparens.

;; (when (not (package-installed-p 'smartparens))
;;   (package-install 'smartparens))

(use-package smartparens
  :diminish smartparens-mode
  :config
  (progn
    (require 'smartparens-config)
    (smartparens-global-mode 1)
    (sp-pair "\"" nil :unless '(sp-point-after-word-p))
    (sp-pair "'" nil :unless '(sp-point-after-word-p))
    )
  )

;;----------------------------------------------------------------------
;; A Emacs tree plugin like NerdTree for Vim.
;; https://github.com/jaypei/emacs-neotree
;; M-x package-install RET neotree

(use-package neotree
  :bind ("<f8>" . neotree-toggle))

;;----------------------------------------------------------------------
;; To edit HTML and related files.

(use-package web-mode
  :mode ("\\.html?\\'" . web-mode)
  :config
  (add-hook 'web-mode-hook
            '(lambda ()
               (setq web-mode-markup-indent-offset 2)) t))

;;----------------------------------------------------------------------
;; MarkDown extensions.
;; (IT MUST BE BEFORE LATEX EXTENSIONS.)

(use-package markdown-mode
  :mode (("\\.md\\'"       . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :config
  (progn
    (add-hook 'markdown-mode-hook 'turn-on-orgstruct)
    (add-hook 'markdown-mode-hook 'turn-on-orgstruct++)
    (require 'imenu-list)
    (add-hook 'markdown-mode-hook 'imenu-add-menubar-index)
    (add-hook 'markdown-mode-hook
              '(lambda ()
                 (global-set-key (kbd "<f10>")
                                 'imenu-list-smart-toggle)))
    )
  )

(use-package yaml-mode
  :defer t
  :mode (("\\.ya?ml\\'" . yaml-mode)
         ("\\.toml\\'"  . yaml-mode)))

;;----------------------------------------------------------------------
;; Org Mode.

(use-package org
  :defer t
  :config
  (setq org-replace-disputed-keys t)
  (setq org-return-follows-link t)
  (setq org-descriptive-links nil)
  ;; Fontify code in code blocks.
  ;; http://orgmode.org/worg/org-contrib/babel/examples/fontify-src-code-blocks.html
  (setq org-src-fontify-natively t)
  ;; Babel.
  (org-babel-do-load-languages 'org-babel-load-languages
                               '((emacs-lisp . t)
                                 (R . t)
                                 (sh . t)))
  (setq org-confirm-babel-evaluate nil)
  )

;;----------------------------------------------------------------------
;; Auto complete mode for Emacs.

;; (when (not (package-installed-p 'auto-complete))
;;   (package-install 'auto-complete))

(use-package auto-complete
  :defer t
  :init
  (progn
    (auto-complete-mode t))
  :config
  (progn
    (use-package auto-complete-config)
    (ac-config-default)
    (setq ac-delay 0.02
          ac-auto-start 0)
    (setq ac-use-quick-help nil
          ac-quick-help-delay 1.)
    (setq ac-use-menu-map t)
    (setq ac-dwim t)
    (setq ac-fuzzy-enable t)
    (setq-default ac-sources '(ac-source-abbrev
                               ac-source-dictionary
                               ac-source-words-in-same-mode-buffers))
    ;; Change 'ac-complete from ENTER to TAB.
    ;; (ac-set-trigger-key "TAB")
    (define-key ac-completing-map "\r" nil)
    (define-key ac-completing-map "\t" 'ac-complete)
    )
  )

;;----------------------------------------------------------------------
;; Python configuration.
;; https://github.com/howardabrams/dot-files/blob/master/emacs-python.org

;; TIP: check if company is enabled in a buffer.
;; (if (bound-and-true-p company-mode)
;;     (message "is on")
;;   (message "is off"))

;; Follow: http://tkf.github.io/emacs-jedi/latest/
;;   Terminal : sudo apt-get install virtualenv
;;   Emacs    : M-x package-install RET jedi RET
;;   Emacs    : M-x jedi:install-server RET

;; Install in Python.
;;   sudo apt-get install python-pip python3-pip
;;   sudo pip install --upgrade pip
;;   pip install jedi
;;   pip install epc

;; sudo find . -name jediepcserver.py
;;   .emacs.d/.python-environments/default/lib/python2.7/site-packages/jediepcserver.py
;;   .emacs.d/elpa/jedi-core-XXX.YYY/jediepcserver.py

;; C-c ? -> jedi:show-doc
;; C-c . -> jedi:goto-definition
;; M-.   -> anaconda-mode-find-definitions

(use-package anaconda-mode
  :init
  (progn
    (add-hook 'python-mode-hook 'anaconda-mode)
    ;; Eldoc from auto-complete is used.
    (add-hook 'python-mode-hook 'anaconda-eldoc-mode)
    ))

(use-package elpy
  :init
  (progn
    (with-eval-after-load 'python (elpy-enable))
    ;; (setq python-shell-interpreter "/usr/bin/python3")
    (setq elpy-rpc-python-command "/home/walmes/anaconda3/bin/python3")
    (setq python-shell-interpreter "/home/walmes/anaconda3/bin/python3")
    ;; To fix a warning message.
    ;; https://emacs.stackexchange.com/questions/30082/your-python-shell-interpreter-doesn-t-seem-to-support-readline
    (setq python-shell-completion-native-enable nil)
    )
  )

;; NOTE: When to use :init or :config.
;; https://emacs.stackexchange.com/questions/10396/difference-between-init-and-config-in-use-package
;;   :init   -> Code to run when `use-package' form evals.
;;   :config -> Runs if and when package loads.

;; https://cestlaz.github.io/posts/using-emacs-45-company/
;; https://steelkiwi.com/blog/emacs-configuration-working-python/
(use-package jedi
  :init ;; Code to run when `use-package' form evals.
  (add-hook 'python-mode-hook
            '(lambda ()
               (jedi:setup)
               (jedi:ac-setup)))
  :config ;; Runs if and when package loads.
  (add-hook
   'python-mode-hook
   '(lambda ()
      (auto-complete-mode t)
      (setq ac-auto-start nil)
      (setq ac-auto-show-menu nil)
      (setq ac-use-quick-help nil)
      (setq ac-auto-start 0
            ac-delay 0
            ac-quick-help-delay 0
            ac-use-fuzzy t
            ac-fuzzy-enable t)
      (company-mode nil)
      (highlight-indentation-mode 0)
      (setq jedi:complete-on-dot t)
      (setq jedi:tooltip-method nil)
      (setq jedi:server-args
            '("--sys-path" "/home/walmes/anaconda3/lib/python3.7/site-packages/"
              "--sys-path" "/home/walmes/anaconda3/lib/python3.6/site-packages/"
              "--sys-path" "/usr/lib/python3.6/"))
      ))
  )

;;----------------------------------------------------------------------
;; ESS - Emacs Speaks Statistics.
;; http://ess.r-project.org/

(use-package ess
  :init
  (progn
    (setq-default ess-dialect "R")
    (setq-default inferior-R-args "--no-restore-history --no-save ")
    ;; (setq inferior-ess-r-program "/home/walmes/anaconda3/bin/R")
    (setq ess-fancy-comments nil
          ess-indent-with-fancy-comments nil
          comint-scroll-to-bottom-on-input t
          comint-scroll-to-bottom-on-output t
          comint-move-point-for-output t
          ess-indent-level 4)
    )
  :bind
  (("C-S-<f5>" . ess-eval-chunk)
   ("C-S-<f6>" . ess-eval-chunk-and-step)
   ("C-S-<f7>" . ess-noweb-next-code-chunk)
   ("C-S-<f8>" . ess-noweb-previous-code-chunk)
   ("C-S-<f9>" . ess-noweb-goto-chunk))
  :config
  ;; Script and console font lock highlight.
  (setq ess-R-font-lock-keywords
        '((ess-R-fl-keyword:modifiers . t)
          (ess-R-fl-keyword:fun-defs . t)
          (ess-R-fl-keyword:keywords . t)
          (ess-R-fl-keyword:assign-ops . t)
          (ess-R-fl-keyword:constants . t)
          (ess-fl-keyword:fun-calls . t)
          (ess-fl-keyword:numbers . t)
          (ess-fl-keyword:operators . t)
          (ess-fl-keyword:delimiters . t)
          (ess-fl-keyword:= . t)
          (ess-R-fl-keyword:F&T . t)))
  (setq inferior-R-font-lock-keywords
        '((ess-S-fl-keyword:prompt . t)
          (ess-R-fl-keyword:messages . t)
          (ess-R-fl-keyword:modifiers . t)
          (ess-R-fl-keyword:fun-defs . t)
          (ess-R-fl-keyword:keywords . t)
          (ess-R-fl-keyword:assign-ops . t)
          (ess-R-fl-keyword:constants . t)
          (ess-fl-keyword:matrix-labels . t)
          (ess-fl-keyword:fun-calls . t)
          (ess-fl-keyword:numbers . t)
          (ess-fl-keyword:operators . t)
          (ess-fl-keyword:delimiters . t)
          (ess-fl-keyword:= . t)
          (ess-R-fl-keyword:F&T . t)))
  (add-hook
   'ess-mode-hook
   '(lambda ()
      ;;-------------------------------------
      (require 'ess-site)
      (setq ess-smart-operators t)
      (setq-local comment-add 0) ;; Single # as default.
      (company-mode 1)       ;; (company-mode -1)
      (auto-complete-mode -1)
      (ess-toggle-underscore nil)
      ;; `Alt + -'  to cycle `<- | <<- | = ...'.
      (define-key ess-mode-map [?\M--] 'ess-cycle-assign)
      (define-key ess-mode-map [f5] 'company-R-args) ;; F5 do show ARGS.
      )
   )
  ;;-----------------------------------------
  (defadvice ess-eval-buffer (before really-eval-buffer compile activate)
    "Prevent call ess-eval-buffer by accident, frequently by
     hitting C-c C-b instead of C-c C-n."
    (if (yes-or-no-p
         (format "Are you sure you want to evaluate the %s buffer?"
                 buffer-file-name))
        (message "ess-eval-buffer started.")
      (error "ess-eval-buffer canceled!")))
  )

(use-package ess-view
  :init (setq ess-view--spreadsheet-program "gnumeric"))

(use-package ess-R-data-view
  ;; :bind ("<f6>" . ess-R-dv-pprint)
  )

;;----------------------------------------------------------------------
;; Navigation in balanced expressions.

(dolist (mode '(ess-mode-hook lisp-mode-hook))
  (add-hook mode
            '(lambda ()
               (global-set-key (kbd "<M-right>")  'forward-sexp)
               (global-set-key (kbd "<M-left>")   'bakward-sexp)
               (global-set-key (kbd "<M-down>")   'forward-list)
               (global-set-key (kbd "<M-up>")     'backward-list)
               (global-set-key (kbd "<M-S-up>")   'backward-up-list)
               (global-set-key (kbd "<M-S-down>") 'down-list))))

;;----------------------------------------------------------------------
;; R+MarkDown extensions (emacs >= 24.3.1).
;; (IT MUST BE BEFORE LATEX EXTENSIONS.)

;; Based on:
;; https://github.com/fernandomayer/spacemacs/blob/master/private/polymode/packages.el#L13

(use-package polymode
  :mode (("\\.Rmd" . Rmd-mode))
  :init
  (progn
    (defun Rmd-mode ()
      "ESS Markdown mode for Rmd files"
      (interactive)
      (require 'poly-R)
      (require 'poly-markdown)
      (R-mode)
      (poly-markdown+r-mode))
    ))

(use-package polymode
  :mode (("\\.Rnw" . Rnw-mode))
  :init
  (progn
    (defun Rnw-mode ()
      "ESS LaTeX mode for Rnw files"
      (interactive)
      (require 'poly-R)
      (require 'poly-noweb)
      (R-mode)
      (poly-noweb+r-mode))
    ))

;;----------------------------------------------------------------------
;; Smart operators with electric spacing.
;; https://github.com/walmes/electric-spacing (fork).

;; Byte compile file.
;; (byte-compile-file "~/.emacs.d/lisp/electric-spacing-r.el")

(use-package electric-spacing-r
  :ensure nil
  :config
  (add-hook 'ess-mode-hook #'electric-spacing-mode)
  (add-hook 'python-mode-hook #'electric-spacing-mode))

;;----------------------------------------------------------------------
;; Latex extensions.

(use-package auctex
  :defer nil
  :mode
  (("\\.pgf\\'" . latex-mode)
   ("\\.pgs\\'" . latex-mode))
  :config
  (setq reftex-plug-into-AUCTeX t)
  (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
  (add-hook 'LaTeX-mode-hook 'turn-on-reftex))

;;----------------------------------------------------------------------
;; Add highlighting for certain keywords.

;; http://lists.gnu.org/archive/html/emacs-orgmode/2010-09/txtb5ChQJCDny.txt
;; http://emacs.1067599.n5.nabble.com/Adding-keywords-for-font-lock-experts-td95645.html
(make-face 'bad-words)
(set-face-attribute 'bad-words nil
                    :foreground "White"
                    :background "Firebrick")
(make-face 'good-words)
(set-face-attribute 'good-words nil
                    :foreground "LightSeaGreen"
                    :background "White")
(make-face 'caution-words)
(set-face-attribute 'caution-words nil
                    :foreground "OrangeRed"
                    :background "LightGray")

(dolist
    (mode '(fundamental-mode emacs-lisp-mode lisp-mode org-mode
            shell-mode sh-mode ess-mode ess-r-mode polymode-mode
            python-mode markdown-mode latex-mode TeX-mode
            prog-mode html-mode css-mode yaml-mode))
  (setq font-lock-keywords-case-fold-search t)
  (font-lock-add-keywords
   mode
   '(("\\<\\(IMPORTANT\\|ATTENTION\\|NOTE\\|OBS\\|TODO\\|BAD\\|STOP\\|PROBLEM\\|FAIL\\|CAUTION\\)\\>"
      0 'caution-words t)
     ("\\<\\(COMMENT\\|IMPROVE\\|REVIEW\\|TIP\\|REMEMBER\\|QUESTION\\|EXPLANATION\\|INTERESTING\\|HYPHOTESIS\\|CONCEPT\\|DISCUSSION\\)\\>"
      0 'font-lock-constant-face t)
     ("\\<\\(BUG\\|WARNING\\|DANGER\\|FIXME\\|ERROR\\)\\>"
      0 'bad-words t)
     ("\\(^\\|[[:space:]]\\)@[[:alnum:]_.]+\\>" ;; @walmes, @param, @return
      0 'font-lock-function-name-face t)
     ("\\<\\(DONE\\|GOOD\\|WALMES\\|SOLVED\\|SOLUTION\\|AMAZING\\|COOL\\|NICE\\|BRILLIANT\\)\\>"
      0 'good-words t))
   ))

;;----------------------------------------------------------------------

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(bmkp-auto-light-when-jump (quote all-in-buffer))
 '(bmkp-auto-light-when-set (quote all-in-buffer))
 '(bmkp-last-as-first-bookmark-file nil)
 '(bmkp-light-style-autonamed (quote lfringe))
 '(bmkp-light-style-non-autonamed (quote lfringe))
 '(column-number-mode t)
 '(doc-view-continuous t)
)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
