;; Library loading
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq load-path
      (append load-path "~/emacs/egg" "~/emacs/yasnippet"))
(require 'ibuffer)
(require 'perlhacks)                    ; load my own hacks
(require 'egg)                          ; load Emacs Got Git
(require 'yasnippet)

;; My own EMUD project, should probably move under this ~/emacs dir...
(when (file-name-exists-p (expand-file-name "~/projects/emud"))
  (add-to-list 'load-path "~/projects/emud")
  (require 'emud))

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
(menu-bar-mode 0)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(ido-mode 1)
(show-paren-mode 1)

;; Disable toolbar for Aquamacs
(when (boundp 'tool-bar-mode) (tool-bar-mode -1))

;; Don't use terminal mode much anymore but I will leave this
(add-hook 'term-mode-hook (lambda () (term-set-escape-char ?\C-x)))

;; Lua
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
(autoload 'lua-mode "lua-mode" "Lua editing mode." t)
(setq lua-indent-level 4)

;; ErgoEmacs -- just the keybindings please kthx
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setenv "ERGOEMACS_KEYBOARD_LAYOUT" "us") ; US layout
(load "~/emacs/ergoemacs-keybindings/ergoemacs-mode")
(ergoemacs-mode 1)

;; YASnippet
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(yas/initialize)
(yas/load-directory "~/emacs/yasnippet/snippets")

;; PHP-Mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'php-mode)
(add-hook 'php-mode-hook
          (lambda () (setq c-electric-flag nil)))

