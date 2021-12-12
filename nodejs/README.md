Notes to self
-------------

## nodejs repl

Installed emacs package `nodejs-repl` https://github.com/abicky/nodejs-repl.el

Open nodejs repl in emacs:

    M-x nodejs-repl

https://nodejs.dev/learn/how-to-use-the-nodejs-repl
https://nodejs.org/api/repl.html

keybindings (`C-x C-e` these, restart `js2-mode`)

    (add-hook 'js2-mode-hook
          (lambda ()
            (define-key js-mode-map (kbd "C-x C-e") 'nodejs-repl-send-last-expression)
            (define-key js-mode-map (kbd "C-c C-k") 'nodejs-repl-send-buffer)
            (define-key js-mode-map (kbd "C-c M-j") 'nodejs-repl)
            (define-key js-mode-map (kbd "C-c M-c") 'nodejs-repl)
            ))

Can't really use `const` or else reloading the buffer fails.
