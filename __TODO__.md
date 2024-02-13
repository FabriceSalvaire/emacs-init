---

- Error in post-command-hook (vertico--exhibit): (void-function +vertico-basic-remote-all-completions)
  Error in pre-command-hook (vertico--prepare): (void-function +vertico-basic-remote-all-completions)
- C-s vs ivy ?
- look doom config
- check module autoload.el
- vertico
  [X] fix color
  strange behaviour for C-x f
    ...dir/ completion bloqué ???
- Error (use-package): markdown-mode/:config: Symbol’s function definition is void: set-flyspell-predicate!
- répertorier les fonctions utiles de doom lib
- clone older setup to compare...
- click link ?
- [x] parent match color ()
- [X] which-key tout n'est pas affiché
- color paren match / cursor
- M-x history
- Alt-Space just-one-space / plasma wayland issue ???
- buffer h split no so visible
- use-package custom:
- emacs daemon
- [X] flyspell _ not readable
- [X] bug; on peut copier indent char ou bien quand on commente la ligne

---
For doom
- C-x b switch buffer ??? 
- C-x f ne garde pas en mémoire le dernier répertoire visité ?
- dired / file buffer : a double q -> q exit dired

---

Why does Doom not use dash, f, s, or similar libraries?

subr-x, seq, map, pcase, and cl-lib are all built into Emacs and, since Emacs 27.1 (the minimum version Doom supports), have made Dash and co (mostly) redundant. One of Doom’s goals is to prefer native functionality where possible or trivial. That said, many third-party packages depend on them, so chances are they are already installed on your system.

---

[mickeynp/ligature.el: Display typographical ligatures in Emacs](https://github.com/mickeynp/ligature.el)
This package maps ordinary graphemes (characters) to fancy ligatures, if both your version of Emacs and the font supports it.
[Unicode, Ligatures and Color Emoji - Mastering Emacs](https://www.masteringemacs.org/article/unicode-ligatures-color-emoji)

- https://copr.fedorainfracloud.org/coprs/deathwish/emacs-pgtk-nativecomp/
- https://github.com/A6GibKm/emacs-pgtk-nativecomp-copr/tree/master
- https://copr.fedorainfracloud.org/coprs/stevenlin/emacs-pgtk-nativecomp

- [mina86/auto-dim-other-buffers.el: Visually makes non-selected windows less prominent](https://github.com/mina86/auto-dim-other-buffers.el)
- [Org mode for GNU Emacs](https://orgmode.org)

---

# .el review

- [ ] `./doom-lib/buffers.el`
- [ ] `./doom-lib/doom-lib.el`
- [ ] `./doom-lib/fonts.el`
- [ ] `./doom-lib/ui.el`

- [ ] `./checker.el`
- [ ] `./code-completion.el`
- [ ] `./completion.el`
- [ ] `./core.el`
- [ ] `./edition.el`
- [ ] `./file-manager.el`
- [ ] `./frame.el`
- [ ] `./git.el`
- [ ] `./keys.el`
- [ ] `./lang/c-java.el`
- [ ] `./lang/lang.el`
- [ ] `./lang/python.el`
- [ ] `./lang/web.el`
- [ ] `./lsp-start-plain.el`
- [ ] `./packages.el`
- [ ] `./packages-straight.el`
- [ ] `./polyglote.el`
- [ ] `./speller.el`
- [ ] `./startup.el`
- [ ] `./sysadmin.el`
- [ ] `./tree-sitter.el`
- [ ] `./undo.el`
- [ ] `./user-functions.el`

### Doom

- [ ] `./cli/....el`

- [ ] `./lib/autoloads.el`
- [ ] `./lib/buffers.el`
- [ ] `./lib/config.el`
- [ ] `./lib/debug.el`
- [ ] `./lib/docs.el`
- [ ] `./lib/files.el`
- [ ] `./lib/fonts.el`
- [ ] `./lib/git.el`
- [ ] `./lib/help.el`
- [ ] `./lib/packages.el`
- [ ] `./lib/plist.el`
- [ ] `./lib/print.el`
- [ ] `./lib/process.el`
- [ ] `./lib/projects.el`
- [ ] `./lib/sandbox.el`
- [ ] `./lib/scratch.el`
- [ ] `./lib/sessions.el`
- [ ] `./lib/store.el`
- [ ] `./lib/system.el`
- [ ] `./lib/text.el`
- [ ] `./lib/themes.el`
- [ ] `./lib/ui.el`

- [!] `./init.el`
- [!] `./packages.el`
- [ ] `./doom-cli.el`
- [!] `./doom-editor.el`
  - HACK Emacs generates long file paths for...
  - recentf
- [!] `./doom.el`
- [ ] `./doom-keybinds.el`
- [C] `./doom-lib.el`
- [ ] `./doom-modules.el`
- [ ] `./doom-packages.el`
- [ ] `./doom-profiles.el`
- [!] `./doom-projects.el`
- [!] `./doom-start.el`
- [!] `./doom-ui.el`
  - doom-switch-buffer doom-emacs/lisp/doom-ui.el
  - bootstrap...
  - ediff
  - [EmacsWiki : Winner Mode](https://www.emacswiki.org/emacs/WinnerMode)
  - highlight-numbers
  - Hide the mode line in completion popups

- [ ] `./app/calendar/config.el`
- [ ] `./app/emms/config.el`
- [ ] `./app/everywhere/config.el`
- [ ] `./app/irc/config.el`
- [ ] `./app/rss/config.el`
- [ ] `./app/twitter/config.el`

- [!] `./checkers/grammar/config.el`
- [!] `./checkers/spell/config.el`
- [!] `./checkers/syntax/config.el`
- [!] `./completion/company/config.el`
       [tumashu/company-posframe](https://github.com/tumashu/company-posframe)
- [ ] `./completion/helm/config.el`
- [ ] `./completion/ido/config.el`
- [!] `./completion/ivy/config.el`
- [ ] `./completion/vertico/config.el`

- [!] `./config/default/config.el`

- [ ] `./editor/evil/config.el`
- [ ] `./editor/file-templates/config.el`
- [ ] `./editor/fold/config.el`
- [ ] `./editor/format/config.el`
- [ ] `./editor/god/config.el`
- [ ] `./editor/lispy/config.el`
- [!] `./editor/multiple-cursors/config.el`
- [ ] `./editor/objed/config.el`
- [ ] `./editor/parinfer/config.el`
- [!] `./editor/snippets/config.el`
- [ ] `./editor/word-wrap/config.el`

- [!] `./emacs/dired/config.el`
- [ ] `./emacs/electric/config.el`
- [?] `./emacs/ibuffer/config.el`
- ! ] `./emacs/undo/config.el`
- [ ] `./emacs/vc/config.el`
- [ ] `./email/mu4e/config.el`
- [ ] `./email/notmuch/config.el`
- [ ] `./email/wanderlust/config.el`

- [ ] `./input/bidi/config.el`
- [ ] `./input/chinese/config.el`
- [ ] `./input/japanese/config.el`
- [ ] `./input/layout/config.el`

- [ ] `./lang/agda/config.el`
- [ ] `./lang/beancount/config.el`
- [ ] `./lang/cc/config.el`
- [ ] `./lang/clojure/config.el`
- [ ] `./lang/common-lisp/config.el`
- [ ] `./lang/coq/config.el`
- [ ] `./lang/crystal/config.el`
- [ ] `./lang/csharp/config.el`
- [ ] `./lang/dart/config.el`
- [ ] `./lang/data/config.el`
- [ ] `./lang/dhall/config.el`
- [ ] `./lang/elixir/config.el`
- [ ] `./lang/elm/config.el`
- [!] `./lang/emacs-lisp/config.el`
- [ ] `./lang/erlang/config.el`
- [ ] `./lang/ess/config.el`
- [ ] `./lang/factor/config.el`
- [ ] `./lang/faust/config.el`
- [ ] `./lang/fortran/config.el`
- [ ] `./lang/fsharp/config.el`
- [ ] `./lang/fstar/config.el`
- [ ] `./lang/gdscript/config.el`
- [ ] `./lang/go/config.el`
- [ ] `./lang/graphql/config.el`
- [ ] `./lang/haskell/config.el`
- [ ] `./lang/hy/config.el`
- [ ] `./lang/idris/config.el`
- [!] `./lang/java/config.el`
- [!] `./lang/javascript/config.el`
- [!] `./lang/json/config.el`
- [ ] `./lang/julia/config.el`
- [ ] `./lang/kotlin/config.el`
- [!] `./lang/latex/config.el`
- [ ] `./lang/lean/config.el`
- [ ] `./lang/ledger/config.el`
- [ ] `./lang/lua/config.el`
- [!] `./lang/markdown/config.el`
- [ ] `./lang/nim/config.el`
- [ ] `./lang/nix/config.el`
- [ ] `./lang/ocaml/config.el`
- [ ] `./lang/org/config.el`
- [ ] `./lang/php/config.el`
- [ ] `./lang/plantuml/config.el`
- [ ] `./lang/purescript/config.el`
- [!] `./lang/python/config.el`
- [ ] `./lang/racket/config.el`
- [ ] `./lang/raku/config.el`
- [ ] `./lang/rest/config.el`
- [!] `./lang/rst/config.el`
- [ ] `./lang/ruby/config.el`
- [ ] `./lang/rust/config.el`
- [ ] `./lang/scala/config.el`
- [ ] `./lang/scheme/config.el`
- [!] `./lang/sh/config.el`
- [ ] `./lang/sml/config.el`
- [ ] `./lang/solidity/config.el`
- [ ] `./lang/swift/config.el`
- [ ] `./lang/terra/config.el`
- [!] `./lang/web/config.el`
- [!] `./lang/yaml/config.el`
- [ ] `./lang/zig/config.el`

- [ ] `./os/macos/config.el`
- [ ] `./os/tty/config.el`

- [ ] `./term/eshell/config.el`
- [ ] `./term/shell/config.el`
- [ ] `./term/term/config.el`
- [ ] `./term/vterm/config.el`

- [ ] `./tools/ansible/config.el`
- [ ] `./tools/biblio/config.el`
- [ ] `./tools/collab/config.el`
- [ ] `./tools/debugger/config.el`
- [ ] `./tools/direnv/config.el`
- [ ] `./tools/docker/config.el`
- [ ] `./tools/editorconfig/config.el`
- [ ] `./tools/ein/config.el`
- [ ] `./tools/eval/config.el`
- [ ] `./tools/lookup/config.el`
- [!] `./tools/lsp/config.el`
- [!] `./tools/magit/config.el`
- [ ] `./tools/pass/config.el`
- [ ] `./tools/pdf/config.el`
- [ ] `./tools/prodigy/config.el`
- [ ] `./tools/taskrunner/config.el`
- [ ] `./tools/terraform/config.el`
- [!] `./tools/tree-sitter/config.el`
- [ ] `./tools/upload/config.el`

- [ ] `./ui/deft/config.el`
- [ ] `./ui/doom/config.el`
- [ ] `./ui/doom-dashboard/config.el`
- [ ] `./ui/doom-quit/config.el`
- [ ] `./ui/emoji/config.el`
- [ ] `./ui/hl-todo/config.el`
- [ ] `./ui/hydra/config.el`
- [!] `./ui/indent-guides/config.el`
- [!] `./ui/ligatures/config.el`
- [!] `./ui/minimap/config.el`
- [!] `./ui/modeline/config.el`
- [ ] `./ui/nav-flash/config.el`
- [ ] `./ui/neotree/config.el`
- [ ] `./ui/ophints/config.el`
- [ ] `./ui/popup/config.el`
- [ ] `./ui/tabs/config.el`
- [!] `./ui/treemacs/config.el`
- [ ] `./ui/vc-gutter/config.el`
- [ ] `./ui/window-select/config.el`
- [ ] `./ui/workspaces/config.el`
- [ ] `./ui/zen/config.el`
