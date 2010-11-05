(defadvice cperl-lineup (before cperl-hacked-lineup)
  (unless (ad-get-arg 3) (ad-set-arg 3 0)))
(ad-activate 'cperl-lineup)

(defun cperl-hacked-lineup (beg end &optional step minshift)
  "Lineup construction in a region.
Beginning of region should be at the start of a construction.
All first occurrences of this construction in the lines that are
partially contained in the region are lined up at the same column.

MINSHIFT is the minimal amount of space to insert before the construction.
STEP is the tabwidth to position constructions.
If STEP is nil, `cperl-lineup-step' will be used
\(or `cperl-indent-level', if `cperl-lineup-step' is nil).
Will not move the position at the start to the left."
  (interactive "r")
  (let (search col tcol seen b)
    (save-excursion
      (goto-char end)
      (end-of-line)
      (setq end (point-marker))
      (goto-char beg)
      (skip-chars-forward " \t\f")
      (setq beg (point-marker))
      (indent-region beg end nil)
      (goto-char beg)
      (setq col (current-column))
      (if (looking-at "[a-zA-Z0-9_]")
          (if (looking-at "\\<[a-zA-Z0-9_]+\\>")
              (setq search
                    (concat "\\<"
                            (regexp-quote
                             (buffer-substring (match-beginning 0)
                                               (match-end 0))) "\\>"))
            (error "Cannot line up in a middle of the word"))
        (if (looking-at "$")
            (error "Cannot line up end of line"))
        (setq search (regexp-quote (char-to-string (following-char)))))
      (setq step (or step cperl-lineup-step cperl-indent-level))
      ;; HACK ;; all I wanted to do was change this to zero!
      (or minshift (setq minshift 0))
      (while (progn
               (beginning-of-line 2)
               (and (< (point) end)
                    (re-search-forward search end t)
                    (goto-char (match-beginning 0))))
        (setq tcol (current-column) seen t)
        (if (> tcol col) (setq col tcol)))
      (or seen
          (error "The construction to line up occurred only once"))
      (goto-char beg)
      (setq col (+ col minshift))
      (if (/= (% col step) 0) (setq step (* step (1+ (/ col step)))))
      (while
          (progn
            (cperl-make-indent col)
            (beginning-of-line 2)
            (and (< (point) end)
                 (re-search-forward search end t)
                 (goto-char (match-beginning 0)))))))) ; No body

;; (defun cperl-hacks-hook ()
;;   ;; (cperl-define-key [?\C-\M-\|] 'cperl-hacked-lineup
;;   ;;                   [(control meta |)])
;;   (cperl-define-key "\C-ct"  'perl-hack-tidy-buffer)
;;   (cperl-define-key "\C-cit" 'perl-insert-template)
;;   (cperl-define-key "\C-cis" 'perl-simple-template)
;;   (cperl-define-key "\C-cim" 'perl-module-template)
;;   (cperl-define-key "\C-cfn" 'flymake-goto-next-error)
;;   (cperl-define-key "\C-cfp" 'flymake-goto-prev-error)
;;   (cperl-define-key "\C-cff" 'flymake-display-err-menu-for-current-line)
;;   (setq add-log-mailing-address "juster@cpan.org"))

;; (add-hook 'cperl-mode-hook 'cperl-hacks-hook)

(defun split-path (file-path)
"Splits the path string FILE-PATH into a list of directories and a file.

If FILE-PATH ends in a forward-slash the result is still the same.
If FILE-PATH is absolute, the first element of the result is \"/\".
If FILE-PATH is a tramp path, the first element of the result is
a tramp prefix.
"
  (if (zerop (length file-path)) '()
    ;; path-sans-slash: `directory-file-name` removes the trailing "/"
    ;; path-comp      : the path component at the very end (may be a dir)
    (let* ((path-sans-slash (directory-file-name file-path))
           (path-comp       (file-name-nondirectory path-sans-slash)))
      (if (> (length path-comp) 0)
          ;; Recursively split the parent directory and append
          ;; our directory component
          (let ((parent-dir-splitup
                 (split-path (file-name-directory path-sans-slash))))
            (append parent-dir-splitup (list path-comp)))
        ;; Stop recursion at the root directory. This can be "/" or it
        ;; can be a tramp prefix (eg: "/sshx:juster.info:/").
        (list (file-name-directory path-sans-slash))))))

(defun join-path (path-comps)
  (directory-file-name
   (mapconcat (lambda (path-component)
                (file-name-as-directory path-component))
              path-comps "")))

(defun perl-dist-mod-path (dist-name)
  (let ((dist-comps (split-string dist-name "-")))
    (concat (join-path (cons "lib" dist-comps)) ".pm")))

(defun perl-project-path (file-path)
  (let ((proj-path-comps '())
        (find-path-comps (reverse
                          (split-path (expand-file-name file-path)))))
    (setq proj-path-comps (cons (car find-path-comps) nil)
          find-path-comps (cdr find-path-comps))
    (catch 'project-path
      (while find-path-comps
        (let ((current-dir (file-name-as-directory
                            (join-path (reverse find-path-comps)))))
          (if (or (file-exists-p (concat current-dir "Makefile.PL"))
                  (file-exists-p (concat current-dir "Build.PL")))
              (throw 'project-path
                     (directory-file-name
                      (mapconcat (lambda (path-comp)
                                   (file-name-as-directory path-comp))
                                 proj-path-comps "")))
            (setq proj-path-comps (cons (car find-path-comps) proj-path-comps)
                  find-path-comps (cdr find-path-comps)))))
      nil)))

(defun perl-guess-module-name (pm-file-path)
  (unless (string= (file-name-extension pm-file-path) "pm")
    (error "%s is not a module (.pm) file" pm-file-path))
  (setq pm-file-path (file-name-sans-extension pm-file-path))
  (let ((mod-path-comps (split-path (perl-project-path pm-file-path))))
    (when (string= (car mod-path-comps) "lib")
      (setq mod-path-comps (cdr mod-path-comps)))
    (mapconcat 'identity mod-path-comps "::")))

(defun perl-buffer-module-name (&optional buffer)
  (unless buffer
    (setq buffer (current-buffer)))
  (perl-guess-module-name
   (or (buffer-file-name)
       (error "Current buffer has no file name"))))

(defun perl-hack-tidy-buffer ()
  (interactive)
  (with-temp-message "Running PerlTidy on buffer..."
    (shell-command-on-region (point-min) (point-max)
                             "perltidy -lp -cti=1" (current-buffer) t
                             "*PerlTidy Errors*" t)))

(require 'flymake)
(defun perl-hack-flymake-init ()
  ;; Mostly copies the flymake original function `flymake-perl-init'
  (let* ((temp-file   (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
         (local-file  (file-relative-name
                       temp-file
                       (file-name-directory buffer-file-name)))
         (perl-project-dir (car (find-perl-project-dir
                                 buffer-file-name)))
         (argument-list (list "-c" local-file)))
    (when perl-project-dir
      (setq argument-list
            (append (list (concat "-I" perl-project-dir "blib/lib")
                          (concat "-I" perl-project-dir "blib/arch/auto"))
                    argument-list)))
    ;; (message "DEBUG argument-list = %s" argument-list)
    (list "perl" argument-list)))

(defalias 'flymake-perl-init 'perl-hack-flymake-init)

;;(setcdr (assoc "\\.p[ml]\\'" flymake-allowed-file-name-masks)
;;        'perl-hack-flymake-init)

;; (add-hook 'find-file-hook 'flymake-find-file-hook)

(defun search-backward-for-my (point)
  (goto-char point)
  (save-match-data
    (catch 'found-my
      (while t
        (unless (re-search-backward "\\bmy\\b" 0 t)
          (throw 'found-my nil))
        (when (eq (get-text-property (point) 'face) 'font-lock-keyword-face)
          (throw 'found-my (point)))))))

(defun search-backward-for-bracket (point)
  (goto-char point)
  (save-match-data
    (catch 'found-bracket
      (while t
        (unless (re-search-backward "\\([{}]\\)" 0 t)
          (throw 'found-bracket nil))
        (unless (eq (get-text-property (point) 'face) 'font-lock-string-face)
          (throw 'found-bracket (cons (elt (match-string 1) 0) (point))))))))

(defun perl-extract-my-names ()
  "Extract the names (with sigil) of variables defined by the
given 'my' statement, which must start at the current buffer
position. Returns a list of names. Removes all text properties
from names."
  (when (looking-at
         (concat "\\=my\\s-+"
                 "\\(?:"
                 "\\([$@%]\\w+\\)"       ; matches a single my entry
                 "\\|"                   ; or
                 "(\\s-*\\([$@%]\\w+\\)" ; multiple entries in parens
                 "\\s-*"                 ; grok whitespace
                 "\\(?:,\\s-*\\([$@%]\\w+\\)\\)*"
                 ")"
                 "\\)"
                 "\\s-*[;=]" ; add these terminators so that we don't
                             ; match incomplete lines (ie the user is
                             ; still typing them)

                 ))

    (let (var-names)
      ;; We either have a match in string 1 or 2 and above...
      (if (match-string 1)
          (setq var-names (list (match-string 1)))
        (let ((i 2))
          (setq var-names '())
          (while (match-string i)
            (let ((name (match-string i)))
              (setq var-names (cons name var-names)))
            (setq i (1+ i)))))
      
      (mapc (lambda (name) (set-text-properties 0 (length name) nil name))
            var-names))))

(defun perl-lexicals-at-point (&optional point)
  "Parses any \"my\" statements defined previously in the current
buffer to generate a list of lexicals which are visible from
POINT. If POINT is not given, the position of the current buffer
is used."
  (unless point (setq point (point)))
  
  (let ((bracket-point      (save-excursion
                              (search-backward-for-bracket point)))
        (my-point           (save-excursion
                              (search-backward-for-my point)))
        (found-scope-count  0)
        (found-lexicals    '()))
    (while my-point
      (if (and bracket-point (> (cdr bracket-point) my-point))

          ;; Keep track of how many scopes are opening and closing...
          (progn
            (if (char-equal (car bracket-point) ?})
                (setq found-scope-count (- found-scope-count 1))
              (setq found-scope-count (1+ found-scope-count)))

            ;; Find the next bracket (earlier in the file)...
            (setq bracket-point
                  (save-excursion
                    (search-backward-for-bracket (cdr bracket-point)))))

        ;; No found brackets or they are before the 'my'...
        (when (>= found-scope-count 0)
          (save-excursion
            (goto-char my-point)
            (setq found-lexicals (append (perl-extract-my-names)
                                         found-lexicals))))

        ;; Find the next my (earlier in the file)...
        (setq my-point
              (save-excursion
                (search-backward-for-my my-point)))))
    (delete-dups found-lexicals)))

;; (require 'sepia)
;; (defalias 'perl-mode 'sepia-mode)

;; run perl on the current region, updating the region
(defun perl-replace-region (start end)     
  "Apply perl command to region"
  (interactive "r")
  (save-excursion
    (shell-command-on-region
     start end
     (concat "perl -pe '"
             (read-from-minibuffer "Replace region perl: ")
             "'")
     t t)))

;; run perl on the current buffer, updating the buffer
(defun perl-replace-buffer ()    
  "Apply perl command to buffer"
  (interactive)
  (let ((ptline   (count-lines (point-min) (point)))
        (ptcol    (current-column))
        (markline  0)
        (markcol   0)
        (command  (read-from-minibuffer "Replace buffer command: "
                                        '("perl -ple \"\"" . 11 ))))
    (exchange-point-and-mark)
    (setq markline (count-lines (point-min) (point)))
    (setq markcol  (current-column))
    (mark-whole-buffer)
    (let ((new-start (region-beginning))
          (new-end   (region-end)))
      (shell-command-on-region  new-start new-end command t t ))
    (goto-line markline)
    (move-to-column markcol)
    (exchange-point-and-mark)
    (goto-line ptline)
    (move-to-column ptcol)))

(defalias 'perl-mode 'cperl-mode)

(setq cperl-close-paren-offset          0
      cperl-continued-brace-offset     -4
      cperl-continued-statement-offset  4
      cperl-extra-newline-before-brace  nil
      cperl-extra-newline-before-brace-multiline nil
      cperl-indent-level                4
      cperl-indent-parens-as-block      nil
      cperl-label-offset                t
      cperl-merge-trailing-else         nil
      cperl-tab-always-indent           t)
