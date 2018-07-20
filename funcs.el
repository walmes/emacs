;;----------------------------------------------------------------------
;; Functions.
;;----------------------------------------------------------------------

;;----------------------------------------------------------------------
;; From Fernando Mayer:
;; http://git.leg.ufpr.br/fernandomayer/emacs/blob/master/emacs.el

;; (defun set-background-mode (frame mode)
;;   (set-frame-parameter frame 'background-mode mode)
;;   (when
;;       (not (display-graphic-p frame))
;;     (set-terminal-parameter
;;      (frame-terminal frame) 'background-mode mode))
;;   (enable-theme 'solarized))

;; (defun switch-theme ()
;;   (interactive)
;;   (let ((mode
;;          (if (eq (frame-parameter nil 'background-mode) 'dark)
;;              'light 'dark)))
;;     (set-background-mode nil mode)))

;;----------------------------------------------------------------------
;; Split window and open shell.

(defun open-shell-split-window ()
  "Open shell and split window."
  (interactive)
  (split-window)
  (shell)
  (previous-buffer) ;; 1
  (other-window 1)
  (next-buffer)     ;; 2
  (other-window 1))

;;----------------------------------------------------------------------
;; Duplicate lines (like in Geany).
;; http://stackoverflow.com/questions/88399/how-do-i-duplicate-a-whole-line-in-emacs

(defun duplicate-line ()
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (newline)
  (yank))

;;----------------------------------------------------------------------
;; Cut and copy without selection.
;; http://ergoemacs.org/emacs/emacs_copy_cut_current_line.html

(defun copy-line-or-region ()
  "Copy current line, or current text selection."
  (interactive)
  (if (region-active-p)
      (kill-ring-save
       (region-beginning)
       (region-end))
    (kill-ring-save
     (line-beginning-position)
     (line-beginning-position 2))))

(defun cut-line-or-region ()
  "Cut the current line, or current text selection."
  (interactive)
  (if (region-active-p)
      (kill-region
       (region-beginning)
       (region-end))
    (kill-region
     (line-beginning-position)
     (line-beginning-position 2))))

;;----------------------------------------------------------------------
;; (un)Comment without selection.

(defun comment-line-or-region ()
  "Comment or uncomment current line, or current text selection."
  (interactive)
  (if (region-active-p)
      (comment-or-uncomment-region
       (region-beginning)
       (region-end))
    (comment-or-uncomment-region
     (line-beginning-position)
     (line-beginning-position 2))))

;;----------------------------------------------------------------------
;; Mark the word where the point is. -- Walmes Zeviani.

(defun mark-whole-word ()
  "Mark the word where the point is."
  (interactive)
  (skip-chars-backward "[[:alnum:]]._")
  (set-mark (point))
  (skip-chars-forward "[[:alnum:]]._"))

;;----------------------------------------------------------------------
;; Move lines.
;; http://www.emacswiki.org/emacs/MoveLine

(defun move-line (n)
  "Move the current line up or down by N lines."
  (interactive "p")
  (setq col (current-column))
  (beginning-of-line) (setq start (point))
  (end-of-line) (forward-char) (setq end (point))
  (let ((line-text (delete-and-extract-region start end)))
    (forward-line n)
    (insert line-text)
    ;; restore point to original column in moved line
    (forward-line -1)
    (forward-char col)))

(defun move-line-up (n)
  "Move the current line up by N lines."
  (interactive "p")
  (move-line (if (null n) -1 (- n))))

(defun move-line-down (n)
  "Move the current line down by N lines."
  (interactive "p")
  (move-line (if (null n) 1 n)))

;;----------------------------------------------------------------------
;; Move regions.

(defun move-region (start end n)
  "Move the current region up or down by N lines."
  (interactive "r\np")
  (let ((line-text (delete-and-extract-region start end)))
    (forward-line n)
    (let ((start (point)))
      (insert line-text)
      (setq deactivate-mark nil)
      (set-mark start))))

(defun move-region-up (start end n)
  "Move the current line up by N lines."
  (interactive "r\np")
  (move-region start end (if (null n) -1 (- n))))

(defun move-region-down (start end n)
  "Move the current line down by N lines."
  (interactive "r\np")
  (move-region start end (if (null n) 1 n)))

;;----------------------------------------------------------------------
;; Return the font face at point.
;; http://stackoverflow.com/questions/1242352/get-font-face-under-cursor-in-emacs

;; (defun what-face (pos)
;;   (interactive "d")
;;   (let ((face (or (get-char-property (point) 'read-face-name)
;;                   (get-char-property (point) 'face))))
;;     (if face (message "Face: %s" face) (message "No face at %d" pos))))

;;----------------------------------------------------------------------
;; Infill paragraph.
;; http://www.emacswiki.org/emacs/UnfillParagraph

(defun unfill-paragraph ()
  "Takes a multi-line paragraph and makes it into a single line
   of text."
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))

;; http://ergoemacs.org/emacs/emacs_unfill-paragraph.html
(defun unfill-region (start end)
  "Replace newline chars in region by single spaces.
   This command does the inverse of `fill-region'."
  (interactive "r")
  (let ((fill-column 90002000))
    (fill-region start end)))

;;----------------------------------------------------------------------
;; Load the local or parent bookmark file, if exists.

;; (defun wz-switch-to-local-bookmark-file ()
;;   "This function search for a file that has the same name of the
;;    current buffer and append the extention `bmk'. So, it check if
;;    a such file exists in the current directory to load it as a
;;    bookmark file. If it fails, then tries a file named
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
;;           )))))

;; (add-hook 'find-file-hook 'wz-switch-to-local-bookmark-file)

;;----------------------------------------------------------------------
;; Commented rules to divide code.

;; t in line is empty (oly whitespaces), nil otherwise.
;; https://lists.gnu.org/archive/html/help-gnu-emacs/2015-11/msg00212.html
(defun blank-line-p ()
  (string-match "^[[:space:]]*$"
                (buffer-substring-no-properties
                 (line-beginning-position)
                 (line-end-position))))

(defun wz-insert-rule-from-point-to-margin (&optional char)
  "Insert a commented rule with dashes (-) from the `point' to
   the `fill-column' if the line has only spaces. If the line has
   text, fill with dashes until the `fill-column'. Useful to
   divide your code in sections. If a non nil optional argument is
   passed, then it is used instead."
  (interactive)
  (if (blank-line-p)
      (progn (insert "-")
             (comment-line-or-region)
             (delete-char -2))
    nil)
  (if char
      (insert (make-string (- fill-column (current-column)) char))
    (insert (make-string (- fill-column (current-column)) ?-))))

(defun wz-insert-rule-and-comment-3 ()
  "Insert a commented rule with 43 dashes (-). Useful to divide
   your code in sections."
  (interactive)
  (if (blank-line-p)
      (progn (insert "-")
             (comment-line-or-region)
             (delete-char -2))
    nil)
  (let ((column-middle (floor (* 0.625 fill-column))))
    (if (< (current-column) column-middle)
        (insert (make-string (- column-middle (current-column)) ?-)))))

;;----------------------------------------------------------------------

;; (defun replace-buffer-divisions-by-Walmes-style (beg end &optional char)
;;   "This functions replace divisions in R code by Walmes's style
;;    code division: start with single # and have 71 dashes, total
;;    length is `fill-column'. All rules greater than 44 characters
;;    will be replaced until complete margin."
;;   (interactive "r")
;;   (save-excursion
;;     (goto-char beg)
;;     (let ((comment-char
;;            (if char
;;                char
;;              (read-from-minibuffer "Comment char: "))))
;;       (while
;;           ;; To have a prompt to pass the comment char.
;;           (re-search-forward
;;            (concat "^" comment-char ".-\\{43,\\}")
;;            nil t)
;;         (replace-match
;;          (concat comment-char
;;                  (make-string
;;                   (- fill-column (string-width comment-char)) ?-))
;;          nil nil)))))

;; (defun wz-make-line-end-dashes-fill-column (beg end)
;;   "This function fix those dashes at end of lines used as
;;    decoration making them have a end at `fill-column'. At least
;;    must have five dashes after a space, because 3 dashes are yaml
;;    header and 4 are markdown horizontal rule."
;;   (interactive "r")
;;   (save-excursion
;;     (goto-char beg)
;;     (while (re-search-forward " -\\{5,\\}" end t)
;;       (let ((xmax fill-column)
;;             (xval (match-beginning 0) )
;;             (null (beginning-of-line))
;;             (xmin (point)))
;;         (replace-match
;;          (concat
;;           " "
;;           (make-string (- xmax (- xval xmin) 1) ?-)) nil nil)))))

;;----------------------------------------------------------------------
;; Header.

(defun wz-right-align-commented-text (text comment-char-size)
  "Write text aligned to the right margin at `fill-column' and
   comment it out."
  (let ((number-of-spaces (- fill-column (length text)))
        (string-length (length text)))
    (insert (concat "\n" text))
    (comment-line-or-region)
    (backward-char string-length)
    (insert (make-string (- number-of-spaces comment-char-size) ? ))
    (forward-char string-length)))

(defun wz-header ()
  "Insert a header."
  (interactive)
  (wz-insert-rule-from-point-to-margin)
  ;; Get the number of characters used as comment.
  (let ((comment-char-size
         (- (+ fill-column 1)
            (how-many "-" (line-beginning-position) (point) t))))
    (wz-right-align-commented-text
     "Prof. Dr. Walmes M. Zeviani"
     comment-char-size)
    (wz-right-align-commented-text
     "leg.ufpr.br/~walmes · github.com/walmes"
     comment-char-size)
    (wz-right-align-commented-text
     "walmes@ufpr.br · @walmeszeviani"
     comment-char-size)
    (wz-right-align-commented-text
     "Laboratory of Statistics and Geoinformation (LEG)"
     comment-char-size)
    (wz-right-align-commented-text
     "Department of Statistics · Federal University of Paraná"
     comment-char-size)
    (wz-right-align-commented-text
     (concat (format-time-string "%Y-%b-%d") " · Curitiba/PR/Brazil")
     comment-char-size)
    )
  (insert "\n")
  (wz-insert-rule-from-point-to-margin))

;;----------------------------------------------------------------------
;; Code based on
;; http://www.emacswiki.org/emacs/CamelCase.

(defun split-name (s)
  (split-string
   (let ((case-fold-search nil))
     (downcase
      (replace-regexp-in-string
       "\\([a-z]\\)\\([A-Z]\\)" "\\1 \\2" s)))
   "[^A-Za-z0-9]+"))

(defun camel-case (s)
  (concat (car (split-name s))
          (mapconcat 'capitalize (cdr (split-name s)) "")))
(defun dot-case (s)
  (mapconcat 'downcase (split-name s) "."))
(defun snake-case (s)
  (mapconcat 'downcase (split-name s) "_"))

(defun camel-dot-snake ()
  "Cycle among camelCase, dot.case and snake_case in words. If
   the region is not active the current word at point is used."
  (interactive)
  (setq trs (and transient-mark-mode mark-active))
  (unless trs
    (skip-chars-backward "[[:alnum:]]._")
    (set-mark (point))
    (skip-chars-forward "[[:alnum:]]._"))
  (let* ((beg (region-beginning))
         (end (region-end))
         (str (buffer-substring-no-properties beg end)))
    (if (string-match "^\s*$" str)
        (message "Not a word at point")
      (delete-region beg end)
      (insert
       (cond ((string-match-p "\\." str) (snake-case str))
             ((string-match-p "_"   str) (camel-case str))
             (t                          (dot-case   str))))
      (if trs (setq deactivate-mark nil)))))

;; Another interesting implementation:
;; https://www.bunkus.org/blog/2009/12/switching-identifier-naming
;; -style-between-camel-case-and-c-style-in-emacs/

;;----------------------------------------------------------------------
;; Functions related do Rmd files.

;; Insert a new (empty) chunk to R markdown.
(defun wz-insert-chunk ()
  "Insert chunk environment Rmd sessions."
  (interactive)
  (if (derived-mode-p 'ess-mode)
      (insert "```\n\n```{r}\n")
    (insert "```{r}\n\n```")
    (forward-line -1)))

;; Goes to next chunk.
(defun wz-polymode-next-chunk ()
  "Go to next chunk. This function is not general because is
   assumes all chunks are of R language."
  (interactive)
  (search-forward-regexp "^```{.*}$" nil t)
  (forward-line 1))

;; Goes to previous chunk.
(defun wz-polymode-previous-chunk ()
  "Go to previous chunk. This function is not general because is
   assumes all chunks are of R language."
  (interactive)
  (search-backward-regexp "^```$" nil t)
  (search-backward-regexp "^```{.*}$" nil t)
  (forward-line 1))

;; Evals current R chunk.
(defun wz-polymode-eval-R-chunk ()
  "Evals all code in R chunks in a polymode document (Rmd files)."
  (interactive)
  (if (derived-mode-p 'ess-mode)
      (let ((ptn (point))
            (beg (progn
                   (search-backward-regexp "^```{r.*}$" nil t)
                   (forward-line 1)
                   (line-beginning-position)))
            (end (progn
                   (search-forward-regexp "^```$" nil t)
                   (forward-line -1)
                   (line-end-position))))
        (ess-eval-region beg end nil)
        (goto-char ptn))
    (message "ess-mode weren't detected.")))

;; Evals R chunk and goes to next chunk.
(defun wz-polymode-eval-R-chunk-and-next ()
  "Evals a R chunk and move point to next chunk."
  (interactive)
  (wz-polymode-eval-R-chunk)
  (wz-polymode-next-chunk))

(defun wz-polymode-eval-line-by-line (end)
  "This function evaluates only R code inside Rmd chunks from
   `point' to `end'. The function determine the boundaries of a
   chunk based on regex. If the point is a chunk, then each line
   is evaluated and the point go to the next line."
  (interactive)
  ;; Turn on/off the `inside-chunk' based on the major-mode at point.
  (if (derived-mode-p 'ess-mode)
      (setq inside-chunk t)
    (setq inside-chunk nil))
  ;; Go through each line till reach `end'.
  (while (< (point) end)
    ;; (beginning-of-line)
    ;; Test if point is at the chunk header.
    (if (looking-at "```{r")
        (progn
          (setq inside-chunk t)
          (forward-line 1)))
    ;; If inside-chunk is t then evaluate the line.
    (if inside-chunk
        (ess-eval-line t))
    (forward-line 1)
    ;; (beginning-of-line)
    ;; Test if point is at the chunk tail.
    (if (looking-at "```$")
        (setq inside-chunk nil))))

(defun wz-polymode-eval-chunks-on-region ()
  "This function calls `wz-polymode-eval-line-by-line'.
   It only determines if region is activated. Then it evaluates
   the code inside the chunks in the region."
  (interactive)
  (if (region-active-p)
      ;; If region is activated.
      (let ((beg (region-beginning))
            (end (region-end)))
        (goto-char beg)
        ;; Calls the function.
        (wz-polymode-eval-line-by-line end))
    ;; If the region is not activated.
    (message "Region must be activated.")))

;;----------------------------------------------------------------------

;; ;; Based on:
;; ;; http://stackoverflow.com/questions/4697322/elisp-call-command-on-current-file
;; (defun wz-ess-rmarkdown-render ()
;;   "Run rmarkdown::render() in the buffer. Tested only in Rmd files."
;;   (interactive)
;;   (shell-command
;;    (format "Rscript -e 'library(rmarkdown); render(\"%s\")'"
;;            (shell-quote-argument (buffer-file-name))))
;;   (revert-buffer t t t))

;; ;; Mark a word at a point.
;; ;; http://www.emacswiki.org/emacs/ess-edit.el
;; (defun ess-edit-word-at-point ()
;;   (save-excursion
;;     (buffer-substring
;;      (+ (point) (skip-chars-backward "a-zA-Z0-9._"))
;;      (+ (point) (skip-chars-forward "a-zA-Z0-9._")))))

;; ;; Eval any word where the cursor is (objects, functions, etc).
;; (defun ess-eval-word ()
;;   (interactive)
;;   (let ((x (ess-edit-word-at-point)))
;;     (ess-eval-linewise (concat x))))

(defun wz-ess-forward-R-assigment-symbol ()
  "Move cursor to the next occurrence of 「<-」 「=」. Adapted from:
   URL `http://ergoemacs.org/emacs/emacs_jump_to_punctuations.html'."
  (interactive)
  (search-forward-regexp "=+\\|<-" nil t))

(defun wz-ess-backward-R-assigment-symbol ()
  "Move cursor to the previous occurrence of 「<-」 「=」. Adapted from:
   `http://ergoemacs.org/emacs/emacs_jump_to_punctuations.html'."
  (interactive)
  (search-backward-regexp "=+\\|<-" nil t))

(defun wz-ess-align-R-assigment-operators ()
  "Função que alinha a região com a primeira ocorrência de sinais
   ` <- ' e ` = '. Baseado em:
   http://stackoverflow.com/questions/13315064/
   marker-does-not-point-anywhere-from-align-regexp-emacs"
  (interactive)
  (save-excursion
    (align-regexp
     (region-beginning) (region-end)
     "\\(\\s-*\\) *\\(<-\\|=\\) *" 1 1 nil)))

(defun wz-ess-backward-break-line-here ()
  "Searches a point backward where a break there is allowed."
  (interactive)
  (re-search-backward "\\([-+*/%<>(,]\\|[<>=!]=\\) *[[:alnum:]({]")
  (forward-char 1))

(defun wz-ess-forward-break-line-here ()
  "Searches a point forward where a break there is allowed. I
   don't know why, but the forward some times skips correct
   points that backward get."
  (interactive)
  (re-search-forward "\\([-+*/%<>(,]\\|[<>=!]=\\) *[[:alnum:]({]")
  (forward-char -1))

(defun string-face-p ()
  "Return t if font-face at point is string-face, nil otherwise."
  (eq 'font-lock-string-face (get-char-property (point) 'face)))

(defun comment-face-p ()
  "Return t if font-face at point is comment-face, nil otherwise."
  (eq 'font-lock-comment-face (get-char-property (point) 'face)))

(defun wz-ess-break-or-join-lines-wizard ()
  "Break line wizard in R scripts. This function helps break and indent
   or join lines in R code. The keybings are:
   <right>    : go to next matching;
   <left>     : go to previous matching;
   <down>     : go to next line;
   <up>       : go to previous line;
   <return>   : break and indent newline;
   <delete>   : join line below;
   <backspace>: join line above;
   <C-z>      : undo;
   <any>      : unhighlight and exit."
  (interactive)
  (setq rgxp "\\([-+*/%<>(,]\\|[<>=!]=\\) *[[:alnum:]({]")
  (highlight-regexp rgxp)
  (let (done event)
    (while (not done)
      (let ((inhibit-quit t))
        (setq event (read-event
                     "Press: right|left|down|up|RET|DEL|BKS|any"))
        (if inhibit-quit (setq quit-flag nil))
        (cond ((eq event 'right)
               (progn (re-search-forward rgxp)
                      (while (or (string-face-p) (comment-face-p))
                        (re-search-forward rgxp))
                      (forward-char -1)))
              ((eq event 'left)
               (progn (re-search-backward rgxp)
                      (while (or (string-face-p) (comment-face-p))
                        (re-search-backward rgxp))
                      (forward-char 1)
                      ))
              ((eq event 'return)
               (progn (indent-new-comment-line)
                      (re-search-forward rgxp)
                      (forward-char -1)))
              ((eq event 'delete)
               (progn (forward-line)
                      (delete-indentation)
                      (re-search-backward rgxp)
                      (forward-char 1)))
              ((eq event 'backspace)
               (progn (delete-indentation)
                      (re-search-backward rgxp)
                      (forward-char 1)))
              ((eq event 'down)
               (forward-line))
              ((eq event 'up)
               (forward-line -1))
              ((eq event ?\C-z)
               (undo))
              ((eq event 'escape)
               (setq done t))
              (t
               (setq done t)))))
    (unhighlight-regexp rgxp)))

;;----------------------------------------------------------------------
;; Function based in the bm-bookmark-regexp-region.
;; This function bookmark all chunks in *.Rnw and *.Rmd buffers.

(defun wz-bm-bookmark-chunk-in-buffer ()
  "Set bookmark on chunk header lines in Rnw and Rmd files."
  (interactive)
  (let ((regexp "^<<.*>>=$\\|^```{.*}$")
        (annotation nil)
        (count 0))
    (save-excursion
      (if bm-annotate-on-create
          (setq annotation
                (read-from-minibuffer
                 "Annotation: " nil nil nil 'bm-annotation-history)))
      (goto-char (point-min))
      (while (and (< (point) (point-max))
                  (re-search-forward regexp (point-max) t))
        (bm-bookmark-add annotation)
        (setq count (1+ count))
        (forward-line 1)))
    (message "%d bookmark(s) created." count)))

;;----------------------------------------------------------------------
;; Font:
;; https://github.com/basille/.emacs.d/blob/master/functions/ess-indent-region-as-R-function.el

;; The function below is a modification of the original to use
;; formatR::tidy_source(). This is because formatR have functions with
;; arguments to control the output, as keep comments and set the width
;; cutoff.

(defun ess-indent-region-with-formatR-tidy-source (beg end)
  "Format region of code R using formatR::tidy_source()."
  (interactive "r")
  (let ((string
         (replace-regexp-in-string
          "\"" "\\\\\\&"
          (replace-regexp-in-string ;; how to avoid this double matching?
           "\\\\\"" "\\\\\\&"
           (buffer-substring-no-properties beg end))))
        (buf (get-buffer-create "*ess-command-output*")))
    (ess-force-buffer-current "Process to load into:")
    (ess-command
     (format
      "local({
          formatR::tidy_source(text=\"\n%s\",
                               arrow=TRUE, width.cutoff=60) })\n"
      string) buf)
    (with-current-buffer buf
      (goto-char (point-max))
      ;; (skip-chars-backward "\n")
      (let ((end (point)))
        (goto-char (point-min))
        (goto-char (1+ (point-at-eol)))
        (setq string (buffer-substring-no-properties (point) end))))
    (delete-region beg end)
    (insert string)))

(defun wz-ess-stringi-escape-unicode (beg end)
  "Replace non-ASCII by the corresponding unicode. Select the text
   without the quotes and apply the function. By Walmes Zeviani."
  (interactive "r")
  (let ((string
         (replace-regexp-in-string
          "\"" "\\\\\\&"
          (replace-regexp-in-string
           "\\\\\"" "\\\\\\&"
           (buffer-substring-no-properties beg end))))
        (buf (get-buffer-create "*ess-command-output*")))
    (ess-force-buffer-current "Process to load into:")
    (ess-command
     (format
      "local({
          cat(stringi::stri_escape_unicode(\"%s\"),
             \"\\n\") })\n"
      string) buf)
    (with-current-buffer buf
      (goto-char (point-max))
      (let ((end (point)))
        (goto-char (point-min))
        (skip-chars-forward " +")
        (setq string (buffer-substring-no-properties (point) end))))
    (delete-region beg end)
    (insert string)
    (delete-char -2)))

;;----------------------------------------------------------------------
;; Improved version of occur. Quick navigation.
;; http://ignaciopp.wordpress.com/2009/06/10/customizing-emacs-occur/

(defun my-occur (&optional arg)
  "Make sure to always put occur in a vertical split, into a
   narrower buffer at the side. I didn't like the default
   horizontal split, nor the way it messes up the arrangement of
   windows in the frame or the way in which the standard way uses
   a neighbor window."
  (interactive "P")
  ;; store whatever frame configuration we are currently in
  (window-configuration-to-register ?y)
  (occur (read-from-minibuffer "Regexp: "))
  (if (occur-check-existence)
      (progn
        (delete-other-windows)
        ;; (split-window-horizontally)
        ;; (enlarge-window-horizontally -30)
        (split-window-vertically)
        (enlarge-window -10)
        ;; (set-cursor-color "green")
        )
    )
  (occur-procede-accordingly)
  (next-error-follow-minor-mode) ;;+
  )

(defun occur-procede-accordingly ()
  "Switch to occur buffer or prevent opening of the occur window
   if no matches occurred."
  (interactive "P")
  (if (not(get-buffer "*Occur*"))
      (message "There are no results.")
    (switch-to-buffer "*Occur*")))

(defun occur-check-existence()
  "Signal the existence of an occur buffer depending on the
   number of matches."
  (interactive)
  (if (not(get-buffer "*Occur*")) nil t))

;; http://www.emacswiki.org/emacs/OccurMode
;; To show more context lines, use
;; C-U 5 M-x occur regexp-to-search

(defun occur-mode-quit ()
  "Quit and close occur window. I want to press 'q' and leave
   things as they were before in regard of the split of windows
   in the frame. This is the equivalent of pressing C-x 0 and
   reset windows in the frame, in whatever way they were, plus
   jumping to the latest position of the cursor which might have
   been changed by using the links out of any of the matches
   found in occur."
  (interactive)
  (switch-to-buffer "*Occur*")
  ;; in order to know where we put the cursor they might have jumped from occur
  (other-window 1)                  ;; go to the main window
  (point-to-register ?1)            ;; store the latest cursor position
  (switch-to-buffer "*Occur*")      ;; go back to the occur window
  (kill-buffer "*Occur*")           ;; delete it
  (jump-to-register ?y)             ;; reset the original frame state
  ;; (set-cursor-color "rgb:ff/fb/53") ;; reset cursor color
  (register-to-point ?1))           ;; re-position cursor

;; Some key bindings defined below. Use "p" ans "n" as in dired mode
;; (without Cntrl key) for previous and next line; just show occurrence
;; without leaving the "occur" buffer; use RET to display the line of
;; the given occurrence, instead of jumping to i,t which you do clicking
;; instead; also quit mode with Ctrl-g.

;;----------------------------------------------------------------------
;; All functions defined below were copied from:
;; http://www.emacswiki.org/emacs/ess-edit.el
;; https://github.com/emacsmirror/ess-edit/blob/master/ess-edit.el

;; (defun ess-edit-backward-move-out-of-comments (lim)
;;   "If inside comments, move the point backwards out."
;;   (let ((opoint (point)) stop)
;;     (if (save-excursion
;;           (beginning-of-line)
;;           (search-forward "#" opoint 'move))
;;         (while (not stop)
;;           (skip-chars-backward " \t\n\f" lim)
;;           (setq opoint (point))
;;           (beginning-of-line)
;;           (search-forward "#" opoint 'move)
;;           (skip-chars-backward " \t#")
;;           (setq stop (or (/= (preceding-char) ?\n) (<= (point) lim)))
;;           (if stop (point)
;;             (beginning-of-line))))))
;;
;; (defun ess-edit-backward-move-out-of-quotes ()
;;   "If inside quotes, move the point backwards out."
;;   (let ((start
;;          (save-excursion
;;            (beginning-of-line) (point))))
;;     (if (ess-edit-within-quotes start (point))
;;         (re-search-backward "[\'\"]" nil t))))
;;
;; (defun ess-edit-within-quotes (beg end)
;;   "Return t if the number of quotes between BEG and END is odd.
;;    Quotes are single and double."
;;   (let (
;;         ;; (countsq (ess-edit-how-many-quotes-region "\\(^\\|[^\\\\]\\)\'" beg end))
;;         ;; (countdq (ess-edit-how-many-quotes-region "\\(^\\|[^\\\\]\\|^\"\"\\)\"" beg end)))
;;         (countsq (ess-edit-how-many-quotes-region beg end))
;;         (countdq (ess-edit-how-many-quotes-region beg end)))
;;     ;; (countsq (ess-edit-how-many-region "\'" beg end))
;;     ;; (countdq (ess-edit-how-many-region "\"" beg end)))
;;     (or (= (mod countsq 2) 1) (= (mod countdq 2) 1))))
;;
;; ;; modified copy of comint-how-many-region
;; (defun ess-edit-how-many-quotes-region (beg end)
;;   "Return number of matches for quotes skipping double quotes and escaped quotes from BEG to END."
;;   (let ((count 0))
;;     (save-excursion
;;       (save-match-data
;;         (goto-char beg)
;;         (while (re-search-forward "\"\\|\'" end t)
;;           (if (or (save-excursion
;;                     (backward-char 3)
;;                     (looking-at "\\\\"))
;;                   (looking-at "\"\\|\'"))
;;               (forward-char 1)
;;             (setq count (1+ count))))))
;;     count))
;;
;; (defun ess-edit-read-call (&optional arg move all)
;;   "Return the name of the R-function call at point as a string.
;;    If ARG return name of function call which is ARG function
;;    calls above point. If MOVE is non-nil leave point after
;;    opening parentheses of call. If all is non-nil return the full
;;    string."
;;   (interactive "p")
;;   (or arg (setq arg 1))
;;   (if (< arg 0)
;;       (error "Only backward reading of function calls possible."))
;;   (add-hook 'pre-command-hook 'ess-edit-pre-command-hook)
;;   ;; assume correct syntax, at least beyond previous paragraph-start
;;   (let ((oldpoint (point))
;;         (lim (save-excursion
;;                (backward-paragraph 1) (point)))
;;         fun beg end)
;;     ;; move outside comments and quotes first
;;     (ess-edit-backward-move-out-of-comments lim)
;;     (ess-edit-backward-move-out-of-quotes)
;;     ;;what if we are sitting on a function call?
;;     (if (save-excursion
;;           (skip-chars-backward "a-zA-Z0-9.")
;;           (looking-at "\\([a-zA-Z0-9.]+\\)\\((\\)"))
;;         (setq beg (match-beginning 1) end (match-end 1)
;;               fun (list (match-string 1))
;;               arg (- arg 1)))
;;     (while
;;         (and (> arg 0)
;;              (re-search-backward "[\"\'()]" lim t)
;;              (let ((matchcar (char-before (match-end 0)))
;;                    matchcall)
;;                (if (eq ?\( matchcar)
;;                    ;; test if sitting on proper function call
;;                    (if (not
;;                         (progn
;;                           (skip-chars-backward "a-zA-Z0-9.")
;;                           (looking-at "\\([a-zA-Z0-9.]+\\)\\((\\)")))
;;                        nil
;;                      (if (string= "\\(if\\|else\\|for\\)"
;;                                   (setq matchcall (match-string 1)))
;;                          t
;;                        (setq beg (match-beginning 1)
;;                              end (match-end 1)
;;                              fun (append (list matchcall) fun))
;;                        (if (= arg 1) nil (setq arg (- arg 1)))))
;;                  ;; skip balanced parentheses or quotes
;;                  (if (not (= ?\) matchcar))
;;                      (re-search-backward
;;                       (char-to-string matchcar) lim t)
;;                    (condition-case nil
;;                        (progn
;;                          (forward-char 1)
;;                          (backward-sexp) t)
;;                      (t (goto-char oldpoint)
;;                         (error (concat "Point is not in a proper"
;;                                        "function call or unbalanced"
;;                                        "parentheses paragraph.")))))))))
;;     (if (not fun)
;;         (progn
;;           (goto-char oldpoint)
;;           (error (concat "Point is not in a proper"
;;                          "function call or unbalanced"
;;                          "parentheses paragraph.")))
;;       (ess-edit-highlight 0 beg end)
;;       (message (car fun))
;;       (goto-char (if move
;;                      (+ (point) (skip-chars-forward "a-zA-Z0-9."))
;;                    oldpoint))
;;       (if all fun (car fun)))))
;;
;; ;; Two functions for activating and deactivation highlight overlays
;; (defun ess-edit-highlight (index begin end &optional buffer)
;;   "Highlight a region with overlay INDEX."
;;   (move-overlay (aref ess-edit-highlight-overlays index)
;;                 begin end (or buffer (current-buffer))))
;;
;; ;; We keep a vector with several different overlays to do our highlighting.
;; (defvar ess-edit-highlight-overlays [nil nil])
;;
;; ;; Initialize the overlays
;; (aset ess-edit-highlight-overlays 0 (make-overlay 1 1))
;; (overlay-put (aref ess-edit-highlight-overlays 0) 'face 'highlight)
;; (aset ess-edit-highlight-overlays 1 (make-overlay 1 1))
;; (overlay-put (aref ess-edit-highlight-overlays 1) 'face 'highlight)
;;
;; (defun ess-edit-indent-call-sophisticatedly (&optional arg force)
;;   (interactive "p")
;;   (let* ((arg (or arg 1))
;;          (fun (ess-edit-read-call arg 'go))
;;          (beg (+ (point) 1))
;;          (end (progn (forward-sexp) (point)))
;;          breaks
;;          delete-p)
;;     ;; (eq last-command 'ess-edit-indent-call-sophisticatedly)
;;     (goto-char beg)
;;     (while (setq match (re-search-forward "[\"\'{([,]" end t))
;;       (if (string= (match-string 0) ",")
;;           (setq breaks
;;                 (cons (cons (point)
;;                             (if (looking-at "[ \t]*\n") t nil))
;;                       breaks))
;;         (if (or (string= (match-string 0) "\"")
;;                 (string= (match-string 0) "\'"))
;;             (re-search-forward (match-string 0) nil t)
;;           (backward-char 1)
;;           (forward-sexp))))
;;     ;; if there are more breaks than half the number of
;;     ;; arguments then delete breaks else add linebreaks
;;     (setq delete-p
;;           (if force
;;               nil
;;             (> (length (delete nil (mapcar 'cdr breaks)))
;;                (* 0.5 (length breaks)))))
;;     (while breaks
;;       (goto-char (caar breaks))
;;       (if delete-p
;;           (if (cdar breaks)
;;               (delete-region
;;                (caar breaks) (+ (point) (skip-chars-forward " \t\n"))))
;;         (if (not (cdar breaks))
;;             (insert "\n")))
;;       (setq breaks (cdr breaks)))
;;     (goto-char (- beg 1))
;;     (ess-indent-exp)
;;     (ess-edit-read-call arg 'go)))

;;----------------------------------------------------------------------

(define-key global-map "\M-Q" 'unfill-region)
(define-key global-map (kbd "C-S-o") 'my-occur)
(define-key occur-mode-map (kbd "q") 'occur-mode-quit)

(global-set-key (kbd "S-<delete>") 'cut-line-or-region)  ; cut.
(global-set-key (kbd "C-<insert>") 'copy-line-or-region) ; copy.
(global-set-key (kbd "C-c d") 'duplicate-line)
(global-set-key (kbd "C-x w") 'mark-whole-word)
(global-set-key (kbd "C-x t") 'open-shell-split-window)
(global-set-key (kbd "M-;") 'comment-line-or-region)
(global-set-key (kbd "M-<") 'move-line-up)
(global-set-key (kbd "M->") 'move-line-down)
(global-set-key (kbd "M-[") 'move-region-up)
(global-set-key (kbd "M-]") 'move-region-down)
(global-set-key (kbd "C-ç") 'camel-dot-snake)
(global-set-key (kbd "C--") 'wz-insert-rule-from-point-to-margin)
(global-set-key (kbd "C-M--") 'wz-insert-rule-and-comment-3)
(global-set-key (kbd "M-=")
                (lambda ()
                  (interactive)
                  (wz-insert-rule-from-point-to-margin ?=)))

(add-hook
 'markdown-mode-hook
 (lambda ()
   (local-set-key (kbd "C-c i")   'wz-insert-chunk)
   (local-set-key (kbd "<f6>")    'wz-polymode-eval-R-chunk)
   (local-set-key (kbd "S-<f6>")  'wz-polymode-eval-R-chunk-and-next)
   (local-set-key (kbd "S-<f7>")  'wz-polymode-previous-chunk)
   (local-set-key (kbd "S-<f8>")  'wz-polymode-next-chunk)))

(add-hook
 'ess-mode-hook
 (lambda ()
   (local-set-key (kbd "C-c i")   'wz-insert-chunk)
   (local-set-key (kbd "<f6>")    'wz-polymode-eval-R-chunk)
   (local-set-key (kbd "S-<f6>")  'wz-polymode-eval-R-chunk-and-next)
   (local-set-key (kbd "S-<f7>")  'wz-polymode-previous-chunk)
   (local-set-key (kbd "S-<f8>")  'wz-polymode-next-chunk)
   (local-set-key (kbd "C-c r")   'ess-eval-word)
   (local-set-key (kbd "C-c a")   'wz-ess-align-R-assigment-operators)
   (local-set-key (kbd "C-,")     'wz-ess-backward-break-line-here)
   (local-set-key (kbd "C-.")     'wz-ess-forward-break-line-here)
   (local-set-key (kbd "<f7>")    'wz-ess-break-or-join-lines-wizard)
   ;; (local-set-key (kbd "<S-f6>")  'wz-ess-rmarkdown-render)
   (local-set-key (kbd "<S-f9>")  'wz-ess-backward-R-assigment-symbol)
   (local-set-key (kbd "<S-f10>") 'wz-ess-forward-R-assigment-symbol)
   ;; (local-set-key (kbd "C-c C-h") 'ess-edit-indent-call-sophisticatedly)
   (local-set-key (kbd "C-|")
                  'ess-indent-region-with-formatR-tidy-source)
   (local-set-key (kbd "C-?")
                  'wz-ess-stringi-escape-unicode)))

;;----------------------------------------------------------------------

(provide 'funcs)
