# redtime.el
Track time for Redmine project with Emacs

Add following to your init.el:

``` emacs-lisp
(add-to-list 'load-path "/path/to/redtime.el/")

(require 'redtime)
(add-to-list 'redtime/hosts '("https://your-redmine-host.org" . "api-key-a"))
(add-to-list 'redtime/hosts '("https://another-redmine.org" . "api-key-b"))
```

It is highly recommended to have `flx` package installed alongside with `redtime`.

Following functions are available to use with M-x:
`redtime-start`, `redtime-stop`, `redtime-discard`.
