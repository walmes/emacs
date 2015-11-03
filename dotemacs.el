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
;;    maximum potential.” – Anon.
;;   
;; http://www.mygooglest.com/fni/dot-emacs.html
;; “Show me your ~/.emacs and I will tell
;;    you who you are.” - Bogdan Maryniuk.

;;----------------------------------------------------------------------
;; Basic definitions.
;;----------------------------------------------------------------------

;; Add directory with supplementary configuration files.
(add-to-list 'load-path "~/.emacs.d/lisp/")

;; Highlight the cursor line.
(global-hl-line-mode 1)

;; Allows delete selected text region.
(delete-selection-mode 1)

;; Cursor position.
(setq column-number-mode t)

;; Highlight matching pairs.
(show-paren-mode 1)

;; Auto break line at 72 characters.
(setq-default fill-column 72)

;; http://emacsredux.com/blog/2013/05/31/highlight-lines-that-exceed-a-certain-length-limit/
;; (require 'whitespace)
(setq whitespace-line-column fill-column)
(setq whitespace-style '(face lines-tail))
(global-whitespace-mode +1)

;; Activate auto-fill-mode to make auto break lines.
(setq-default auto-fill-function 'do-auto-fill)

;; Screen lines instead of logical lines.
(visual-line-mode 1)
 
;; Allow shared transfer area.
(setq x-select-enable-clipboard t)

;; Comment even in empty lines.
(setq comment-empty-lines t)

;; Turn off auto save and auto backup.
(setq auto-save-default nil) ;; Para o #autosave#.
(setq make-backup-files nil) ;; Para o backup~.

;; Spaces to indent.
;; http://xenon.stanford.edu/~manku/dotemacs.html
(setq-default indent-tabs-mode nil)

;; Font and size.
(cond
 ((string-equal system-name "brother")
  (set-default-font "Ubuntu Mono-16"))
 ((string-equal system-name "youngest")
  (set-default-font "Ubuntu Mono-16"))
 ((string-equal system-name "first")
  (set-default-font "Ubuntu Mono-14"))
 ((string-equal system-name "class")
  (set-default-font "Ubuntu Mono-14")))

;; Turn ido-mode on.
;; http://www.emacswiki.org/InteractivelyDoThings
(ido-mode t)

;; To work the accents on Sony Vaio.
(require 'iso-transl)

;; Open Emacs without start-up screen.
(setq inhibit-startup-screen t)
(add-hook 'emacs-startup-hook 'delete-other-windows)[/code]

;; Org-mode definitions.
;; http://emacswiki.org/emacs/OrgMode#toc7
(setq org-replace-disputed-keys t)
(setq org-return-follows-link t)

;; Fontify code in code blocks.
;; http://orgmode.org/worg/org-contrib/babel/examples/fontify-src-code-blocks.html
(setq org-src-fontify-natively t)

;;----------------------------------------------------------------------
;; Add melpa repository.
;;----------------------------------------------------------------------

(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list 'package-archives
               '("melpa" . "http://melpa.org/packages/") t)
  (package-initialize))

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

;; M-= to get division line with 70 =.
(global-set-key [?\M-=] (kbd "C-u 7 0 ="))

;; M-- to get division line with 70 -.
(global-set-key [?\M--] (kbd "C-u 7 0 -"))

;; C-- to get division line with 43 -.
(global-set-key [?\C--] (kbd "C-u 4 3 -"))

;; S-F11 and S-F12 to show/hide menu bar and tool bar.
(global-set-key (kbd "<S-f11>") 'toggle-menu-bar-mode-from-frame)
(global-set-key (kbd "<S-f12>") 'toggle-tool-bar-mode-from-frame)

;;----------------------------------------------------------------------
;; Functions.
;;----------------------------------------------------------------------

;; (load "~/.emacs.d/lisp/functions")
(require 'functions)

;;----------------------------------------------------------------------
;; Extensions.
;;----------------------------------------------------------------------

;;----------------------------------------------------------------------
;; Solarized color theme.

(require 'color-theme)
(color-theme-initialize)
(setq color-theme-is-global t)

;; (add-to-list 'load-path
;;              "~/.emacs.d/emacs-color-theme-solarized")
(require 'color-theme-solarized)

;; Color theme according to machine name.
(if (not (string-equal system-name "class"))
    (color-theme-solarized-dark))

;;----------------------------------------------------------------------
;; MarkDown extensions.
;; (IT MUST BE BEFORE LATEX EXTENSIONS.)

;; (setq load-path
;;       (append '("~/.emacs.d/markdown-mode")
;;               load-path))

(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; Org-struct minor mode active in markdown mode.
(add-hook 'markdown-mode-hook 'turn-on-orgstruct)
(add-hook 'markdown-mode-hook 'turn-on-orgstruct++)

;;----------------------------------------------------------------------
;; R+MarkDown extensions (emacs >= 24.3.1).
;; (IT MUST BE BEFORE LATEX EXTENSIONS.)

;; (setq load-path
;;       (append '("~/.emacs.d/polymode/"
;;                 "~/.emacs.d/polymode/modes")
;;               load-path))

(require 'poly-R)
(require 'poly-markdown)
(require 'poly-noweb)

(autoload 'poly-markdown-mode "poly-markdown-mode"
  "Major mode for editing R-Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.Rmd\\'" . poly-markdown-mode))
(add-to-list 'auto-mode-alist '("\\.Rpres\\'" . poly-markdown-mode))

;; Insert a new (empty) chunk to R markdown.
(defun insert-chunk ()
  "Insert chunk environment Rmd sessions."
  (interactive)
  (insert "```{r}\n\n```")
  (forward-line -1)
  )

(global-set-key (kbd "C-c i") 'insert-chunk)

;;----------------------------------------------------------------------
;; ESS - Emacs Speaks Statistics.

;; http://ess.r-project.org/
;; git clone git@github.com:emacs-ess/ESS.git

(require 'ess-site)
(setq-default ess-dialect "R")
(require 'ess-eldoc)

;;----------------------------------------------------------------------
;; Key bindings for R, Rnw and Rmd sessions.
;; http://stackoverflow.com/questions/2901198/useful-keyboard-shortcuts-and-tips-for-ess-r

;; Movement across chunks in Rnw files.
(global-set-key (kbd "C-S-<f5>") 'ess-eval-chunk)
(global-set-key (kbd "C-S-<f6>") 'ess-eval-chunk-and-step)
(global-set-key (kbd "C-S-<f7>") 'ess-noweb-next-code-chunk)
(global-set-key (kbd "C-S-<f8>") 'ess-noweb-previous-code-chunk)
(global-set-key (kbd "C-S-<f9>") 'ess-noweb-goto-chunk)

;; Movement across chunks in Rmd files.
(global-set-key (kbd "S-<f7>") 'polymode-previous-chunk)
(global-set-key (kbd "S-<f8>") 'polymode-next-chunk)
(global-set-key (kbd "S-<f9>") 'polymode-insert-new-chunk)

(setq-default inferior-R-args "--no-restore-history --no-save  ")

(defadvice ess-eval-buffer (before really-eval-buffer compile activate)
  "Prevent call ess-eval-buffer by accident, frequently by
   hitting C-c C-b instead of C-c C-n."
  (if (yes-or-no-p
       (format "Are you sure you want to evaluate the %s buffer?"
               buffer-file-name))
      (message "ess-eval-buffer started.")
    (error "ess-eval-buffer canceled!")
    )
  )

;; http://www.kieranhealy.org/blog/archives/2009/10/12/make-shift-enter-do-a-lot-in-ess/
(add-hook 'ess-mode-hook
          '(lambda()
             (setq comint-scroll-to-bottom-on-input t)
             (setq comint-scroll-to-bottom-on-output t)
             (setq comint-move-point-for-output t)))

;; http://permalink.gmane.org/gmane.emacs.ess.general/8419
;; Script font lock highlight.
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
        (ess-R-fl-keyword:F&T . t)
        ;; (ess-R-fl-keyword:%op% . t)
        ))

;; Console font lock highlight.
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
        (ess-R-fl-keyword:F&T . t)
        ;; (ess-R-fl-keyword:%op% . t)
        ))

;;----------------------------------------------------------------------
;; Add highlighting for certain keywords.

;; http://lists.gnu.org/archive/html/emacs-orgmode/2010-09/txtb5ChQJCDny.txt
;; http://emacs.1067599.n5.nabble.com/Adding-keywords-for-font-lock-experts-td95645.html
(make-face 'special-words) 
(set-face-attribute 'special-words nil :foreground "White" :background "Firebrick") 

(dolist
    (mode '(fundamental-mode
            gnus-article-mode
            org-mode
            shell-mode
            sh-mode
            muse-mode
            ess-mode
            polymode-mode
            markdown-mode
            latex-mode
            TeX-mode)) 
  (font-lock-add-keywords
   mode 
   '(("\\<\\(COMMENT\\|DONE\\|TODO\\|STOP\\|IMPORTANT\\|NOTE\\|OBS\\|ATTENTION\\|REVIEW\\)" 
      0 'font-lock-warning-face t) 
     ("\\<\\(BUG\\|WARNING\\|DANGER\\|FIXME\\)" 
      0 'special-words t)))
  ) 

;;----------------------------------------------------------------------
;; To eval line/regions in terminal open in Emacs.

(require 'essh)

;; (global-set-key [?\C-*] 'pipe-line-to-shell-and-step)
;; (global-set-key [?\C-&] 'pipe-region-to-shell)

;; essh.el - ESS like shell mode
(defun essh-sh-hook ()                                             
  (define-key sh-mode-map "\C-c\C-r" 'pipe-region-to-shell)        
  (define-key sh-mode-map "\C-c\C-b" 'pipe-buffer-to-shell)        
  (define-key sh-mode-map "\C-c\C-j" 'pipe-line-to-shell)          
  (define-key sh-mode-map "\C-c\C-n" 'pipe-line-to-shell-and-step) 
  (define-key sh-mode-map "\C-c\C-f" 'pipe-function-to-shell)      
  (define-key sh-mode-map "\C-c\C-d" 'shell-cd-current-directory)) 
(add-hook 'sh-mode-hook 'essh-sh-hook)

;;----------------------------------------------------------------------
;; Auto complete mode for Emacs.

(require 'auto-complete-config)
(ac-config-default)

;; To activate ESS auto-complete for R.
(setq ess-use-auto-complete 'script-only)

;; Change 'ac-complete from ENTER to TAB.
(define-key ac-completing-map "\r" nil)
(define-key ac-completing-map "\t" 'ac-complete)

;;----------------------------------------------------------------------
;; Bookmark-plus.

;; (setq bookmark-default-file "~/.emacs.d/bookmarks")
(setq
 bookmark-default-file "~/Dropbox/bookmarks"
 bookmark-save-flag 1)

;; (add-to-list 'load-path
;;              "~/.emacs.d/bookmark-plus/")
(require 'bookmark+)

;; Create an autonamed bookmark.
(global-set-key
 (kbd "<C-f3>") 'bmkp-toggle-autonamed-bookmark-set/delete)
;; Go to the next bookmark in file.
(global-set-key
 (kbd "<f3>") 'bmkp-next-bookmark-this-file/buffer-repeat)
;; Go to the previous bookmark in file.
(global-set-key
 (kbd "<f4>") 'bmkp-previous-bookmark-this-file/buffer-repeat)
;; Toggle temporary/permanent bookmark.
(global-set-key
 (kbd "<S-f3>") 'bmkp-toggle-temporary-bookmark)

;;----------------------------------------------------------------------
;; Visible bookmarks. Easy movement.
;; https://marmalade-repo.org/packages/bm

;; (add-to-list 'load-path "~/.emacs.d/bm/")
(require 'bm)

;; http://emacsworld.blogspot.com.br/2008/09/visual-bookmarks-package-for-emacs.html
;; Customize the colors by using M-x customize-group RET bm RET
(setq bm-marker 'bm-marker-left)
(setq bm-highlight-style 'bm-highlight-only-fringe)

(global-set-key (kbd "<C-f2>") 'bm-toggle)
(global-set-key (kbd "<f2>") 'bm-next)
(global-set-key (kbd "<S-f2>") 'bm-previous)

;;----------------------------------------------------------------------
;; Folding code blocks based on indentation.
;; git clone https://github.com/zenozeng/yafolding.el.git

;; (add-to-list 'load-path "~/.emacs.d/yafolding.el/")
(require 'yafolding)

(global-set-key [?\C-{] #'yafolding-hide-parent-element)
(global-set-key [?\C-}] #'yafolding-toggle-element)

;;----------------------------------------------------------------------
;; Start yasnippet with emacs.
;; http://barisyuksel.com/cppmode/.emacs
;; https://www.youtube.com/watch?v=HTUE03LnaXA

;; (require 'yasnippet)
;; (yas-global-mode 1)

;; ;;----------------------------------------------------------------------
;; ;; R snippets.
;; ;; https://github.com/mlf176f2/r-autoyas.el
;; 
;; (require 'r-autoyas)
;; (add-hook 'ess-mode-hook 'r-autoyas-ess-activate)

;;----------------------------------------------------------------------
;; Auto complete mode for Emacs.

(require 'auto-complete-config)
(ac-config-default)

;; To activate ESS auto-complete for R.
(setq ess-use-auto-complete 'script-only)

;; Change 'ac-complete from ENTER to TAB.
(define-key ac-completing-map "\r" nil)
(define-key ac-completing-map "\t" 'ac-complete)

;;----------------------------------------------------------------------
;; Latex extensions.

(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq reftex-plug-into-AUCTeX t)

;; Open Tikz files (pgf and pgs extensions) in Tex mode.
(add-to-list 'auto-mode-alist '("\\.pgf" . tex-mode))
(add-to-list 'auto-mode-alist '("\\.pgs" . tex-mode))

;;----------------------------------------------------------------------
;; Babel.

(org-babel-do-load-languages 'org-babel-load-languages
                             '((R . t)
                               (emacs-lisp . t)
                               (python . t)))
;; (org-confirm-babel-evaluate nil)
(setq org-confirm-babel-evaluate nil)

;;----------------------------------------------------------------------
;; Check for packages.
;;----------------------------------------------------------------------

;; (require 'prelude-packages)

;; ;;----------------------------------------------------------------------
;; ;; Load the local or parent bookmark file, if exists.
;; 
;; (defun switch-to-local-bookmark-file ()
;;   "This function search for a file that has the same na'me of the
;;    current buffer that call it, append the extention `bmk' and
;;    check if a such file exists in the current directory to load
;;    it as a bookmark file. If it fails, then tries a file named
;;    `bookmark'. If fails, tries a `bookmark' at the parent
;;    directory. If fails, nothing happens."
;;   (defvar current-file-dir
;;     (file-name-directory (or load-file-name buffer-file-name)))
;;   (let ((local-file-with-bmk-extension
;;          (concat current-file-dir (file-name-base) ".bmk"))
;;         (local-bookmark-file
;;          (concat current-file-dir "bookmark"))
;;         (parent-bookmark-file
;;          (concat (file-name-directory
;;                   (directory-file-name current-file-dir))
;;                  "bookmark")))
;;     ;; Nested if statments to search, check and load a bookmark file.
;;     (if (file-exists-p local-file-with-bmk-extension)
;;         (bmkp-switch-bookmark-file-create local-file-with-bmk-extension)
;;       (if (file-exists-p local-bookmark-file)
;;           (bmkp-switch-bookmark-file-create local-bookmark-file)
;;         (if (file-exists-p parent-bookmark-file)
;;             (bmkp-switch-bookmark-file-create parent-bookmark-file)
;;           )
;;         )
;;       )
;;     ))
;; 
;; (add-hook 'find-file-hook 'switch-to-local-bookmark-file)
;; 
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(bmkp-auto-light-when-jump (quote all-in-buffer))
 '(bmkp-auto-light-when-set (quote all-in-buffer))
 '(bmkp-last-as-first-bookmark-file nil)
 '(bmkp-light-style-autonamed (quote lfringe))
 '(bmkp-light-style-non-autonamed (quote lfringe)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
