;;; Directory Local Variables
;;; For more information see (info "(emacs) Directory Variables")

((elm-mode
  (eval local-set-key
        (kbd "M-e")
        '(lambda ()
           (interactive)
           (require 'magit)
           (save-buffer)
           (recompile)))
  (compile-command . (format "cd %s ; make" (magit-get-top-dir)))))
