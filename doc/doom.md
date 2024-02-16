- command summary on first page
- ask to quit y/n
- comment-region ?
- disable evil

# Startup

The overall load order is as follows:

```
> $EMACSDIR/ early-init.el
  > $EMACSDIR/lisp/ doom.el PERF
    - $EMACSDIR/lisp/ doom-lib.el
  > $EMACSDIR/lisp/ doom-start.el
    - hook: `doom-before-init-hook'
    - $DOOMDIR/ init.el
      doom-keybinds
      doom-ui
      doom-projects
      doom-editor

- hook: `before-init-hook'

> $XDG_DATA_HOME/doom/$PROFILE/@/$VERSION/init.el   (replaces $EMACSDIR/init.el)
  - $EMACSDIR/doom-{keybinds,ui,projects,editor}.el

  @ doom-start

  - hook: `doom-before-modules-init-hook'
  - {$DOOMDIR,$EMACSDIR}/modules/*/*/init.el
  - hook: `doom-after-modules-init-hook'
  - hook: `doom-before-modules-config-hook'
  - {$DOOMDIR,$EMACSDIR}/modules/*/*/config.el
  - hook: `doom-after-modules-config-hook'
  - $DOOMDIR/config.el
  - `custom-file' or $DOOMDIR/custom.el

- hook: `after-init-hook'
- hook: `emacs-startup-hook'
- hook: `window-setup-hook'
- hook: `doom-init-ui-hook'
- hook: `doom-after-init-hook'
> After startup is complete:
  - On first input:              `doom-first-input-hook'
  - On first switched-to buffer: `doom-first-buffer-hook'
  - On first opened file:        `doom-first-file-hook'
```

- `config.el` is loaded at the end

# Debug

- [How to debug issues - Guides & Tutorials - Doom Emacs Discourse](https://discourse.doomemacs.org/t/how-to-debug-issues/55)

The *Messages* buffer can be accessed with either:
- `M-x view-echo-area-messages`
- `M-x switch-to-buffer` RET *Messages*.
- `C-h e` 

# Func/Macro name

- `...!`
- `+...--...`
- `...--...`
- `doom-...`
- `.../...`

# Customisation

[zzamboni.org | My Doom Emacs configuration, with commentary](https://zzamboni.org/post/my-doom-emacs-configuration-with-commentary)

# Shortcuts

## Dired

- `^` dired-up-directory

# Lib

- `~/.config/doom` config
- `~/.config/doom-emacs` codes

- `use-package!` `modules/config/use-package` patch use-package

- `early-init.el` PERF...
- `doom-lib.el` functions
- `doom.el` PERF...
- `doom-editor.el`
- `doom-start.el`
- `doom-ui.el`
- `doom-projects.el` projectile

# Modes

## Dired

Minor modes enabled in this buffer:
- Better-Jumper-Local
- Company
- Dired-Omit
- Diredfl
- Dtrt-Indent
- Font-Lock
- Hl-Line
- Solaire
- Undo-Fu-Session
- Ws-Butler
- Yas

The major mode is Dired by name mode defined in dired.el:

## Markdown

Minor modes enabled in this buffer:
- +Emacs-Lisp-Non-Package
- Auto-Save
- Better-Jumper-Local
- Company
- Display-Line-Numbers
- Dtrt-Indent
- Eldoc
- Flycheck
- Flycheck-Popup-Tip
- Font-Lock
- Hl-Line
- Smartparens
- Undo-Fu-Session
- Vi-Tilde-Fringe
- Visual-Line
- Whitespace
- Ws-Butler
- Yas

The major mode is Markdown mode defined in markdown-mode.el:

## Lisp

Minor modes enabled in this buffer:
- +Emacs-Lisp--Flycheck-Non-Package
- +Emacs-Lisp-Non-Package
- Auto-Save
- Better-Jumper-Local
- Company
- Display-Line-Numbers
- Eldoc
- Flycheck
- Flycheck-Popup-Tip
- Font-Lock
- Git-Gutter
- Highlight-Numbers
- Highlight-Quoted
- Hl-Line
- Hl-Todo
- Outline
- Rainbow-Delimiters
- Smartparens
- Undo-Fu-Session
- Vi-Tilde-Fringe
- View
- Whitespace
- Ws-Butler
- Yas

The major mode is ELisp/d mode defined in elisp-mode.el:

## Python

Minor modes enabled in this buffer:
- +Emacs-Lisp-Non-Package
- Auto-Save
- Better-Jumper-Local
- Company
- Display-Line-Numbers
- Dtrt-Indent
- Eldoc
- Flycheck
- Flycheck-Popup-Tip
- Font-Lock
- Git-Gutter
- Highlight-Numbers
- Hl-Line
- Hl-Todo
- Smartparens
- Undo-Fu-Session
- Vi-Tilde-Fringe
- Whitespace
- Ws-Butler
- Yas

The major mode is Python mode defined in python.el:

Global minor modes enabled:
+Popup
Auto-Encryption
Better-Jumper
Column-Number
Delete-Selection
Doom-Modeline
Electric-Indent
File-Name-Shadow
Gcmh
General-Override
Global-Company
Global-Eldoc
Global-Flycheck
Global-Font-Lock
Global-Git-Commit
Global-Hl-Line
Global-So-Long
Marginalia
Persp
Projectile
Recentf
Save-Place
Savehist
Server
Show-Paren
Size-Indication
Smartparens-Global
Solaire-Global
Transient-Mark
Undo-Fu
Undo-Fu-Session-Global
Vertico
Volatile-Highlights
Which-Key
Window-Divider
Winner
Ws-Butler-Global
Yas-Global
