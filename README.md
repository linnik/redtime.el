# redtime.el
Track time for Redmine project with Emacs

## Setup

Add following to your init.el:

``` emacs-lisp
(add-to-list 'load-path "/path/to/redtime.el/")

(require 'redtime)
(add-to-list 'redtime/hosts '("https://your-redmine-host.org" . "api-key-a"))
(add-to-list 'redtime/hosts '("https://another-redmine.org" . "api-key-b"))
```
It is highly recommended to have `flx` package installed alongside with `redtime`.

## Time tracking

To start tracking: `M-x redtime-start`

To stop tracking and save to Redmine: `M-x redtime-start`

To discard currently tracked time: `M-x redtime-discard`

## Project switching

Switch Redmine hosts with `M-x redtime-switch-host`
and projects with `M-x redtime-switch-project`.

If only one host was added to 'redtime/hosts, then it will be used by default.
Otherwise, you will be prompted to select one from list.

## Reports table

Invoke reports table with: `M-x redtime-last-entries`,
or for a specific date with `M-x redtime-day-entries`
(You will be prompted with default org-mode calendar to pick a date).

Use prefix to show last N records: `C-u 30 M-x redtime-last-entries`,
same goes for day report: `C-u 10 M-x redtime-day-entries`.

Keybinding | Description
-----------|------------
<kbd>q</kbd> | Quit buffer
<kbd>?</kbd> | Show keybindings
<kbd>f u</kbd> | Filter table by user
<kbd>f x</kbd> | Filter reset
<kbd>s a</kbd> | Sort by activity
<kbd>s d</kbd> | Sort by date
<kbd>s u</kbd> | Sort by username
<kbd>s i</kbd> | Sort by issue
<kbd>s h</kbd> | Sort by hours

Sorting order is reversed when you hit same keybinding twice, like `s d s d`.

Due to default limitation of Redmine API,
users list recived through project's membership,
so you need to select project first
(from given prompt or `M-x redtime-switch-project`).

## Calendar keybindings

Keybinding | Description
-----------|------------
<kbd>Return</kbd> | Choose date at cursor in calendar.
<kbd>mouse-1</kbd> | Select date by clicking on it.
<kbd>S-→</kbd> / <kbd>S-←</kbd>  | One day forward/backward.
<kbd>S-↑</kbd> / <kbd>S-↓</kbd> | One week forward/backward.
<kbd>M-S-→</kbd> / <kbd>M-S-←</kbd> | One month forward/backward.
<kbd>></kbd> / <kbd><</kbd> | Scroll calendar forward/backward by one month.
<kbd>M-v</kbd> / <kbd>C-v</kbd> | Scroll calendar forward/backward by 3 months.
<kbd>M-S-↑</kbd> / <kbd>M-S-↓</kbd> | Scroll calendar forward/backward by one year.

Note: S is for <kbd>Shift</kbd> here.
