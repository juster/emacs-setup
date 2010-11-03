;; I have setup screen to start on login according to this article:
;; http://taint.org/wk/RemoteLoginAutoScreen

(server-start)                          ; emacs server rocks!

;; We create a new screen whenever emacsclient is used. This
;; removes the old screen.
(add-hook 'server-done-hook
          (lambda ()
	    (shell-command
	     "screen -r -X select `cat ~/.emacsclient-caller`")))
