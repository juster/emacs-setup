(add-to-list 'load-path "~/emacs/egg")
(require 'ibuffer)
(require 'perlhacks)                    ; load my own hacks
(require 'egg)                          ; load Emacs Got Git

;; Generic Emacs Stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq default-buffer-coding-system 'utf-8-unix)

(setq c-basic-offset       4		; Override tabsize to 4
      default-tab-width    4
      tex-dvi-view-command "xdvi"
      transient-mark-mode  t
      fill-column          78		; Set line width to 78 columns
      auto-fill-mode       t)

(setq-default indent-tabs-mode nil)

(menu-bar-mode   0)                    ; wtf I dont know how to use it

;; (iswitchb-mode   1)

(global-set-key (kbd "C-x C-b") 'ibuffer)

(ido-mode 1)
(show-paren-mode 1)
;; (tool-bar-mode  -1)		; Don't show the toolbar, never use it
(add-hook 'term-mode-hook (lambda () (term-set-escape-char ?\C-x)))

;; Lua mode
(add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
(autoload 'lua-mode "lua-mode" "Lua editing mode." t)
(setq lua-indent-level 4)

;; for stunnel
;; (setenv "PATH" (concat "~/bin:" (getenv "PATH")))

(add-to-list 'load-path "~/projects/emud")
(require 'emud)

;; Ruby
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (require 'inf-ruby)


;; Chicken Scheme
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (require 'cluck)
;; (autoload 'scheme-smart-complete "scheme-complete" nil t)
;; (eval-after-load 'scheme 
;;   '(define-key scheme-mode-map "\e\t" 'scheme-smart-complete))


;; Gambit-C Scheme
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (autoload 'gambit-inferior-mode "gambit"
;;           "Hook Gambit mode into cmuscheme.")
;; (autoload 'gambit-mode "gambit"
;;           "Hook Gambit mode into scheme.")
;; (add-hook 'inferior-scheme-mode-hook (function gambit-inferior-mode))
;; (add-hook 'scheme-mode-hook (function gambit-mode))
;; (setq scheme-program-name "gsi -:d-")

;;(add-to-list 'auto-mode-alist '("\\`PKGBUILD\\'" . shell-script-mode))

;; LaTeX
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun abbrev-ellipsis-to-ldots (insert-count)
  "Bind \".\" to this to replace \"...\" with \"\\ldots\" while typing.

Check the current buffer to see if the preceding characters at
point are two periods. If so, delete them and replaces them with
\"\\ldots\". If not, insert a period into the buffer.

If INSERT-COUNT is provided, insert that many periods
always. When called as a key-binding, this is specified with the
C-u prefix.

Installation:
\(add-hook 'latex-mode-hook
           (lambda () (local-set-key \".\" 'abbrev-ellipsis-to-ldots)))"

  (interactive "p")
  (insert (cond ((condition-case nil
                     (save-excursion
                       (and (char-equal (preceding-char) ?.)
                            (progn (backward-char)
                                   (char-equal (preceding-char) ?.)))))
                 (delete-char -2) "\\ldots")
                (insert-count (make-string insert-count ?.))
                (t ?.))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ErgoEmacs -- just the keybindings please kthx
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setenv "ERGOEMACS_KEYBOARD_LAYOUT" "us") ; US layout
(load "~/emacs/ergoemacs-keybindings/ergoemacs-mode")
(ergoemacs-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; YASnippet
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'load-path "~/emacs/yasnippet")
(require 'yasnippet)
(yas/initialize)
(yas/load-directory "~/emacs/yasnippet/snippets")
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )

;; PHP-Mode
(require 'php-mode)
(add-hook 'php-mode-hook
          (lambda () (setq c-electric-flag nil)))