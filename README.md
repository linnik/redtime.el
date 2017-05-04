# redtime.el
Track time for Redmine project with Emacs

Add following to your init.el:

``` emacs-lisp
(setq redtime/host "https://your-redmine-host.org")
(setq redtime/api-key "your-api-key")
```

It is highly recommended to have `flx` package installed alongside with `redtime`.

Following functions are available to use with M-x:
`redtime-start`, `redtime-stop`, `redtime-discard`.
