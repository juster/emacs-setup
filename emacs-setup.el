;; Library loading
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar emacs-setup-dir "~/emacs-setup")

(defun make-setup-dir (path)
  (concat emacs-setup-dir "/" path))

(setq load-path
      (append (list emacs-setup-dir)
              (mapcar (lambda (path)
                        (concat emacs-setup-dir "/" path))
                      '("egg" "yasnippet" "ergoemacs-keybindings"))
	      load-path))

;; ErgoEmacs -- just the keybindings, load us quick!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setenv "ERGOEMACS_KEYBOARD_LAYOUT" "us") ; US layout
(load "ergoemacs-mode")
(ergoemacs-mode 1)
(unless (boundp 'recenter-top-bottom)
  (defalias 'recenter-top-bottom 'recenter))

;; Disable toolbar and tabbar for Aquamacs
(menu-bar-mode 0)
(when (boundp 'tool-bar-mode) (tool-bar-mode -1))
(when (boundp 'tabbar-mode)   (tabbar-mode -1))

;; YASnippet
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'yasnippet)
(yas/initialize)
(yas/load-directory (make-setup-dir "snippets"))

;; Emacs Got Git!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'egg)

;; Generic Emacs Stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Unicode, tabs, indent...
(setq default-buffer-coding-system 'utf-8-unix
      c-basic-offset               4    ; Override tabsize to 4
      default-tab-width            4
      transient-mark-mode          t
      fill-column                  78   ; Set line width to 78 columns
      auto-fill-mode               t)

(setq-default indent-tabs-mode nil)

;; Built-in minor modes...
(require 'ibuffer)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(ido-mode 1)
(show-paren-mode 1)

;; Don't use terminal mode much anymore but I will leave this
(add-hook 'term-mode-hook (lambda () (term-set-escape-char ?\C-x)))

;; Lua
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
(autoload 'lua-mode "lua-mode" "Lua editing mode." t)
(setq lua-indent-level 4)

;; PHP-Mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'php-mode)
(add-hook 'php-mode-hook
          (lambda () (setq c-electric-flag nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'perlhacks)

;; My own EMUD project, should probably move under this ~/emacs dir...
(when (file-exists-p (expand-file-name "~/projects/emud"))
  (add-to-list 'load-path "~/projects/emud")
  (require 'emud))

(server-start) ; emacs server rocks!
