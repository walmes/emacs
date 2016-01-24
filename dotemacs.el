;;======================================================================
;; Configuration file to Emacs (>=24.3.1) by Walmes Zeviani.
;;
;; This file is hosted at https://github.com/walmes/emacs2.
;;
;; Almost all the content available here was obtained/inspired by
;; queries on the internet. Please, send questions, problems and/or
;; suggestions as an issue on GitHub directory of this file.
;;======================================================================

;; http://www.emacswiki.org/wiki/EmacsNiftyTricks
;; “I’ve used Emacs for many years now, but have never reached its
;;    maximum potential.” -- Anon.
;;
;; http://www.mygooglest.com/fni/dot-emacs.html
;; “Show me your ~/.emacs and I will tell
;;    you who you are.” -- Bogdan Maryniuk.

;;----------------------------------------------------------------------
;; Basic definitions.
;;----------------------------------------------------------------------

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
(setq-default auto-fill-function
              'do-auto-fill)        ;; Auto break long lines.

;; Highlight whitespace.
(setq whitespace-line-column fill-column)
(setq whitespace-style
      '(face lines-tail trailing spaces tabs empty))
(global-whitespace-mode +1)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Open Emacs without start-up screen.
(setq inhibit-startup-screen t)
(add-hook 'emacs-startup-hook 'delete-other-windows)[/code]

;; Font and size.
(cond ((string-equal system-name "brother")
       (set-default-font "Ubuntu Mono-16"))
      ((string-equal system-name "youngest")
       (set-default-font "Ubuntu Mono-16"))
      ((string-equal system-name "first")
       (set-default-font "Ubuntu Mono-14"))
      ((string-equal system-name "class")
       (set-default-font "Ubuntu Mono-14")))

;; Turn ido-mode on.
(ido-mode t)

;;----------------------------------------------------------------------
;; Key bindings.
;;----------------------------------------------------------------------

;; C-TAB to move the cursor between visible buffers.
(global-set-key [(control tab)] 'other-window)

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
;; Functions.
;;----------------------------------------------------------------------

(require 'funcs)

;; (add-hook 'find-file-hook
;;           (lambda ()
;;              (replace-buffer-divisions-by-Walmes-style
;;               (point-min) (point-max) "#")
;;             ))

;;----------------------------------------------------------------------
;; Extensions.
;;----------------------------------------------------------------------

(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list 'package-archives
               '("melpa" . "http://melpa.org/packages/") t)
  (package-initialize))

;; (if (require 'quelpa nil t)
;;     (quelpa-self-upgrade)
;;   (with-temp-buffer
;;     (url-insert-file-contents
;;      "https://raw.github.com/quelpa/quelpa/master/bootstrap.el")
;;     (eval-buffer)))
;; (setq quelpa-upgrade-p nil)

;; (quelpa 'helm :upgrade nil)
;; (quelpa 'company :upgrade nil)
;; (quelpa 'eldoc-extension :upgrade nil)

;;----------------------------------------------------------------------
;; helm.
;; http://tuhdo.github.io/helm-intro.html

(require 'helm)
(require 'helm-config)

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action)
(define-key helm-map (kbd "C-z") 'helm-select-action)

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

(helm-mode 1)
(helm-autoresize-mode t)

(global-set-key (kbd "C-c h")   'helm-command-prefix)
(global-unset-key (kbd "C-x c"))
(global-set-key (kbd "M-x")     'helm-M-x)
(global-set-key (kbd "M-y")     'helm-show-kill-ring)
(global-set-key (kbd "C-x b")   'helm-mini)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-c h o") 'helm-occur)
(global-set-key (kbd "C-h SPC") 'helm-all-mark-rings)

(add-to-list 'helm-sources-using-default-as-input
             'helm-source-man-pages)

;;----------------------------------------------------------------------
;; company.

(require 'company)
;; (add-hook 'after-init-hook 'global-company-mode)
(setq company-idle-delay             0.2
      company-minimum-prefix-length  2
      company-require-match          nil
      company-dabbrev-ignore-case    nil
      company-dabbrev-downcase       nil
      company-frontends              '(company-pseudo-tooltip-frontend))

;;----------------------------------------------------------------------

(require 'iso-transl) ;; To work the accents on Sony Vaio.
(require 'eldoc-extension)

;;----------------------------------------------------------------------
;; Solarized color theme.

(require 'color-theme)
(color-theme-initialize)
(setq color-theme-is-global t)

(cond ((string-equal system-name "brother")
       (setq solarized-default-background-mode 'dark))
      ((string-equal system-name "youngest")
       (setq solarized-default-background-mode 'dark))
      ((string-equal system-name "first")
       (setq solarized-default-background-mode 'light))
      ((string-equal system-name "class")
       (setq solarized-default-background-mode 'light)))

(load-theme 'solarized t)

(add-hook 'after-make-frame-functions
          (lambda (frame)
            (set-background-mode
             frame
             solarized-default-background-mode)))

(set-background-mode nil solarized-default-background-mode)

;; Install spacemacs-theme by melpa.
;; https://github.com/nashamri/spacemacs-theme
;; (load-theme 'spacemacs-dark t)
;; (load-theme 'spacemacs-light t)
;; (custom-set-variables '(spacemacs-theme-custom-colors
;;                         '((act1 . "#ff0000")
;;                           (act2 . "#0000ff")
;;                           (base . "#ffffff"))))

;;----------------------------------------------------------------------
;; Magit.

(require 'magit)
(global-set-key (kbd "C-x g") 'magit-status)

;;----------------------------------------------------------------------
;; essh.el - ESS like shell mode. To eval line/regions in Emacs shell.

(require 'essh)
(add-hook
 'sh-mode-hook
 '(lambda ()
    (define-key sh-mode-map "\C-c\C-r" 'pipe-region-to-shell)
    (define-key sh-mode-map "\C-c\C-b" 'pipe-buffer-to-shell)
    (define-key sh-mode-map "\C-c\C-j" 'pipe-line-to-shell)
    (define-key sh-mode-map "\C-c\C-n" 'pipe-line-to-shell-and-step)
    (define-key sh-mode-map "\C-c\C-f" 'pipe-function-to-shell)
    (define-key sh-mode-map "\C-c\C-d" 'shell-cd-current-directory)))

;;----------------------------------------------------------------------
;; Bookmark-plus.

(setq bookmark-default-file "~/Dropbox/bookmarks"
      bookmark-save-flag 1)

(require 'bookmark+)

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

;;----------------------------------------------------------------------
;; Visible bookmarks. Easy movement.
;; https://marmalade-repo.org/packages/bm

(require 'bm)

;; Customize the colors by using M-x customize-group RET bm RET
(setq bm-marker 'bm-marker-left)
(setq bm-highlight-style 'bm-highlight-only-fringe)

(global-set-key (kbd "<C-f2>") 'bm-toggle)
(global-set-key (kbd "<f2>")   'bm-next)
(global-set-key (kbd "<S-f2>") 'bm-previous)

;;----------------------------------------------------------------------
;; Folding code blocks based on indentation.
;; git clone https://github.com/zenozeng/yafolding.el.git

(require 'yafolding)
(global-set-key [?\C-{] #'yafolding-hide-parent-element)
(global-set-key [?\C-}] #'yafolding-toggle-element)

;;----------------------------------------------------------------------
;; Smart Parenthesis.
;; https://github.com/Fuco1/smartparens.

(require 'smartparens)
(require 'smartparens-config)
(smartparens-global-mode 1)

(sp-pair "\"" nil :unless '(sp-point-after-word-p))
(sp-pair "'" nil :unless '(sp-point-after-word-p))

;;----------------------------------------------------------------------
;; MarkDown extensions.
;; (IT MUST BE BEFORE LATEX EXTENSIONS.)

(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'"       . markdown-mode))

;; Org-struct minor mode active in markdown mode.
(add-hook 'markdown-mode-hook 'turn-on-orgstruct)
(add-hook 'markdown-mode-hook 'turn-on-orgstruct++)

;;----------------------------------------------------------------------
;; R+MarkDown extensions (emacs >= 24.3.1).
;; (IT MUST BE BEFORE LATEX EXTENSIONS.)

(require 'poly-R)
(require 'poly-markdown)
(require 'poly-noweb)

(autoload 'poly-markdown-mode "poly-markdown-mode"
  "Major mode for editing R-Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.[Rr]md\\'" . poly-markdown-mode))
(add-to-list 'auto-mode-alist '("\\.Rpres\\'"  . poly-markdown-mode))

;; Movement across chunks in Rmd files.
(global-set-key (kbd "S-<f7>") 'polymode-previous-chunk)
(global-set-key (kbd "S-<f8>") 'polymode-next-chunk)

;;----------------------------------------------------------------------
;; ESS - Emacs Speaks Statistics.
;; http://ess.r-project.org/

(require 'ess-site)
(require 'ess-eldoc)
(setq-default ess-dialect "R")
(setq-default inferior-R-args "--no-restore-history --no-save ")

(defadvice ess-eval-buffer (before really-eval-buffer compile activate)
  "Prevent call ess-eval-buffer by accident, frequently by
   hitting C-c C-b instead of C-c C-n."
  (if (yes-or-no-p
       (format "Are you sure you want to evaluate the %s buffer?"
               buffer-file-name))
      (message "ess-eval-buffer started.")
    (error "ess-eval-buffer canceled!")))

(add-hook
 'ess-mode-hook
 '(lambda()
    ;; (auto-complete-mode -1)
    ;; (company-mode 1)
    (company-mode -1)
    (auto-complete-mode 1)
    (setq ess-indent-with-fancy-comments nil) ;; No indent levels.
    (setq-local comment-add 0)                ;; Single # as default.
    (setq ess-smart-operators t)              ;; Smart comma.
    (setq comint-scroll-to-bottom-on-input t)
    (setq comint-scroll-to-bottom-on-output t)
    (setq comint-move-point-for-output t)))

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
        ;; (ess-R-fl-keyword:%op% . t)
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
        ;; (ess-R-fl-keyword:%op% . t)
        (ess-R-fl-keyword:F&T . t)))

;; Movement across chunks in Rnw files.
(global-set-key (kbd "C-S-<f5>") 'ess-eval-chunk)
(global-set-key (kbd "C-S-<f6>") 'ess-eval-chunk-and-step)
(global-set-key (kbd "C-S-<f7>") 'ess-noweb-next-code-chunk)
(global-set-key (kbd "C-S-<f8>") 'ess-noweb-previous-code-chunk)
(global-set-key (kbd "C-S-<f9>") 'ess-noweb-goto-chunk)

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
;; Auto complete mode for Emacs.

(require 'auto-complete-config)
(ac-config-default)

(setq ac-auto-start 0
      ac-delay 0.2
      ac-quick-help-delay 1.
      ac-use-fuzzy t
      ac-fuzzy-enable t
      ;; use 'complete when auto-complete is disabled
      tab-always-indent 'complete
      ac-dwim t)

(setq-default ac-sources '(ac-source-abbrev
                           ac-source-dictionary
                           ac-source-words-in-same-mode-buffers))

;; To activate ESS auto-complete for R.
(setq ess-use-auto-complete 'script-only)

;; Change 'ac-complete from ENTER to TAB.
(define-key ac-completing-map "\r" nil)
(define-key ac-completing-map "\t" 'ac-complete)

;;----------------------------------------------------------------------
;; Smart operators with electric spacing.
;; https://github.com/walmes/electric-spacing (fork).

(require 'electric-spacing)
(add-hook 'ess-mode-hook #'electric-spacing-mode)

;;----------------------------------------------------------------------
;; Start yasnippet with emacs.
;; http://barisyuksel.com/cppmode/.emacs
;; https://www.youtube.com/watch?v=HTUE03LnaXA

;; (require 'yasnippet)
;; (yas-global-mode 1)

;;----------------------------------------------------------------------
;; R snippets.
;; https://github.com/mlf176f2/r-autoyas.el

;; (require 'r-autoyas)
;; (add-hook 'ess-mode-hook 'r-autoyas-ess-activate)

;;----------------------------------------------------------------------
;; Latex extensions.

(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq reftex-plug-into-AUCTeX t)

;; Open Tikz files (pgf and pgs extensions) in Tex mode.
(add-to-list 'auto-mode-alist '("\\.pgf" . latex-mode))
(add-to-list 'auto-mode-alist '("\\.pgs" . latex-mode))

;;----------------------------------------------------------------------
;; Org Mode.

;; http://emacswiki.org/emacs/OrgMode#toc7
(setq org-replace-disputed-keys t)
(setq org-return-follows-link t)

;; Fontify code in code blocks.
;; http://orgmode.org/worg/org-contrib/babel/examples/fontify-src-code-blocks.html
(setq org-src-fontify-natively t)

;; http://orgmode.org/worg/org-dependencies.html
(require 'ox-latex)
(setq org-latex-listings t)
(add-to-list 'org-latex-packages-alist '("" "listings"))
(add-to-list 'org-latex-packages-alist '("" "color"))

;; Babel.
(org-babel-do-load-languages 'org-babel-load-languages
                             '((emacs-lisp . t)
                               (R . t)
                               (sh . t)))
(setq org-confirm-babel-evaluate nil)

;;----------------------------------------------------------------------
;; Add highlighting for certain keywords.

;; http://lists.gnu.org/archive/html/emacs-orgmode/2010-09/txtb5ChQJCDny.txt
;; http://emacs.1067599.n5.nabble.com/Adding-keywords-for-font-lock-experts-td95645.html
(make-face 'special-words)
(set-face-attribute 'special-words nil
                    :foreground "White"
                    :background "Firebrick")

(dolist
    (mode '(fundamental-mode lisp-mode org-mode shell-mode
            sh-mode ess-mode polymode-mode markdown-mode
            latex-mode TeX-mode))
  (font-lock-add-keywords
   mode
   '(("\\<\\(IMPORTANT\\|ATTENTION\\|NOTE\\|OBS\\|TODO\\|DONE\\|STOP\\)"
      0 'font-lock-warning-face t)
     ("\\<\\(COMMENT\\|IMPROVE\\|REVIEW\\)"
      0 'font-lock-warning-face t)
     ("\\<\\(BUG\\|WARNING\\|DANGER\\|FIXME\\)"
      0 'special-words t))
   ))

;;----------------------------------------------------------------------
;; Check for packages.
;;----------------------------------------------------------------------

;; (require 'prelude-packages)

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
 '(doc-view-continuous t)
)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
