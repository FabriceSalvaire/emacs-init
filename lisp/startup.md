# Emacs Startup

- [Startup Summary](https://www.gnu.org/software/emacs/manual/html_node/elisp/Startup-Summary.html#Startup-Summary)

-  1. ...
-  2. set `before-init-time`
-  6. load `early-init.el`
-  7. call `package-activate-all`
-  8. initialize the window system
-  9. `before-init-hook`
- 14. load `init.el`
- 17. set `after-init-time`
- 18. run `after-init-hook` and `delayed-warnings-hook`
- 26. run emacs-startup-hook
- 28. run window-setup-hook
- 30. If a daemon was requested, it calls server-start
- 31. call emacs-session-restore
