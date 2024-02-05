# To look

* https://github.com/tam17aki/ace-isearch — provides a minor mode that combines isearch,
  ace-jump-mode or avy and helm-swoop or swiper.
* https://github.com/winterTTr/ace-jump-mode — Ace jump mode is a minor mode of emacs, which help
  you to move the cursor within Emacs.
* https://github.com/abo-abo/avy — Jump to things in Emacs tree-style
* https://github.com/emacsorphanage/helm-swoop — Efficiently hopping squeezed lines powered by Emacs
  helm interface
* https://github.com/abo-abo/swiper — Ivy - a generic completion frontend for Emacs, Swiper -
  isearch with an overview, and more.


# Doom Emacs

Doom is a customised Emacs with a VI like experience.

* https://github.com/doomemacs/doomemacs
* https://docs.doomemacs.org/latest/#/modules

* https://github.com/abo-abo/hydra
  This is a package for GNU Emacs that can be used to tie related commands into a family of short
  bindings with a common prefix - a Hydra.
* https://github.com/orgs/doomemacs/projects/5


# Config Examples

* https://github.com/jwiegley/dot-emacs/blob/master/init.el
* https://github.com/angrybacon/dotemacs/blob/master/dotemacs.org


# Ivy / Counsel / Swiper - Completion Mechanism for Emacs

* **Ivy** is a generic completion mechanism for Emacs.
* **Counsel** takes this further, providing versions of common Emacs commands that are customised to
  make the best use of Ivy.
* **Swiper** is an alternative to isearch that uses Ivy to show an overview of all matches.

* https://oremacs.com/2015/04/16/ivy-mode
* https://oremacs.com/swiper/#global-key-bindings

`ivy-minibuffer-map` has full editing capability where the familiar
`C-a`, `C-f`, `M-d`, `M-DEL`, `M-b`, `M-w`, `C-k`, `C-y`
key bindings work the same as in fundamental-mode.

**Key bindings for navigation**
|---|---|---|
| C-n or Up   	  | ivy-next-line           | selects the next candidate       |
| C-p or Down 	  | ivy-previous-line       | selects the previous candidate   |
| M-< 			  | ivy-beginning-of-buffer | selects the first candidate	   |
| M-> 			  | ivy-end-of-buffer       | selects the last candidate	   |
| C-v or PageUp   | ivy-scroll-up-command   | scrolls up by ivy-height lines   |
| M-v or PageDown | ivy-scroll-down-command | scrolls down by ivy-height lines |

**Key bindings for single selection, action, then exit minibuffer**
|---|---|---|
| C-m or RET | ivy-done             | Calls the default action and then exits the minibuffer. |
| M-o 		 | ivy-dispatching-done	| Presents valid actions from which to choose. |
| C-j 		 | ivy-alt-done			| When completing file names, selects the current directory candidate and starts a new completion session there. |
| TAB 		 | ivy-partial-or-done	| Attempts partial completion, extending current input as much as possible. |
| C-M-j      | ivy-immediate-done	| Exits with the current input instead of the current candidate (like other commands).
| C-'        | ivy-avy              | Uses avy to select one of the candidates on the current candidate page. |

**Key bindings that alter the minibuffer input**
|---|---|
| M-n   | ivy-next-history-element |
| M-p   | ivy-previous-history-element |
| M-i   | ivy-insert-current |
| M-j   | ivy-yank-word |
| S-SPC | ivy-restrict-to-matches |
| C-r   | ivy-reverse-i-search|

## File name completion

When ivy-mode is on, find-file will also use it. The completion is considerably different from all
other cases, since it's done in stages, just like ido-find-file does it.

The key bindings are:

* **RET** will select the current candidate and finish.
* **C-j** will try to continue the completion, i.e. if the current candidate is a directory, move to
    that directory.  But if the current candidate is a file or ./, then finish.
* **/** will switch to completing the sub-directories of /, but if the candidate is a perfect match,
  it will act like C-j.
* **~** will switch to completing the sub-directories of ~/.
* **C-n** and **C-p** naturally select the next and the previous candidate.
* **DEL** move to the parent directory (counsel-find-file)


# Company - Text completion

* https://company-mode.github.io/manual

To initiate completion manually, use the command `M-x company-complete`.

To select next or previous of the shown completion candidates, use respectively key bindings
`C-n / M-n` and `C-p / M-p`, then do one of the following:

* Hit `RET` to choose a selected candidate for completion.
* Hit `TAB` to complete with the common part: characters present at the beginning of all the candidates.
* Hit `C-g` (or `<ESC ESC ESC>`) to stop activity of Company `company-abord` .

* `C-h` `<f1>` Display a buffer with the documentation for the selected candidate (company-show-doc-buffer).
* `C-w` Display a buffer with the definition of the selected candidate (company-show-location).


# Treemacs — a tree layout file explorer for Emacs

* https://github.com/Alexander-Miller/treemacs

**commands**
* treemacs


# Projectile - a project interaction library for Emacs

* https://docs.projectile.mx/projectile/index.html
* https://github.com/ericdanan/counsel-projectile

Usage: open some file in a version-controlled (e.g. git)

|---|---|
| s-p C-h     | Projectile’s keybindings |

| s-p p   	  | Switch project |
| s-p q   	  | Switch between open projects |
| s-p f   	  | Find file in current project |
| s-p a   	  | Toggle between related files (e.g. foo.h <→ foo.c and Gemfile <→ Gemfile.lock) |

| s-p s g 	  | Grep (search for text/regexp) in project |
| s-p r   	  | Replace in project |

| s-p t   	  | Toggle between implementation and test |
| s-p m   	  | Invoke any Projectile command via the Projectile Commander |
| s-p !   	  | Run a shell command in the root of the project (s-p ! for a sync command and s-p & for an async command) |
| s-p c   	  | build/compile project |
| s-p T   	  | test project |

| s-p f       | Display a list of all files in the project. With a prefix argument it will clear the cache first. |
| s-p F       | Display a list of all files in all known projects. |
| s-p g       | Display a list of all files at point in the project. With a prefix argument it will clear the cache first. |
| s-p 4 f     | Jump to a project’s file using completion and show it in another window. |
| s-p 4 g     | Jump to a project’s file based on context at point and show it in another window. |
| s-p 5 f     | Jump to a project’s file using completion and show it in another frame. |
| s-p 5 g     | Jump to a project’s file based on context at point and show it in another frame. |
| s-p d       | Display a list of all directories in the project. With a prefix argument it will clear the cache first. |
| s-p 4 d     | Switch to a project directory and show it in another window. |
| s-p 5 d     | Switch to a project directory and show it in another frame. |
| s-p T       | Display a list of all test files(specs, features, etc) in the project. |
| s-p l       | Display a list of all files in a directory (that’s not necessarily a project) |
| s-p s g     | Run grep on the files in the project. |
| M-- s-p s g | Run grep on projectile-grep-default-files in the project. |
| s-p s s     | Runs ag ( the_silver_searcher ) on the project, performing a literal search. Requires the presence of ag.el . With a prefix argument it will perform a regex search. |
| s-p s r     | Runs rg ( ripgrep ) on the project, performing a literal search. Requires the presence of rg.el or ripgrep.el . With a prefix argument it will perform a regex search. |
| s-p v       | Run vc-dir on the root directory of the project. |
| s-p V       | Browse dirty version controlled projects. |
| s-p b       | Display a list of all project buffers currently open. |
| s-p 4 b     | Switch to a project buffer and show it in another window. |
| s-p 5 b     | Switch to a project buffer and show it in another frame. |
| s-p 4 C-o   | Display a project buffer in another window without selecting it. |
| s-p a       | Switch between files with the same name but different extensions. |
| s-p 4 a     | Switch between files with the same name but different extensions in other window. |
| s-p 5 a     | Switch between files with the same name but different extensions in other frame. |
| s-p o       | Runs multi-occur on all project buffers currently open. |
| s-p r       | Runs interactive query-replace on all files in the projects. |
| s-p i       | Invalidates the project cache (if existing). |
| s-p R       | Regenerates the projects TAGS file. |
| s-p j       | Find tag in project’s TAGS file. |
| s-p k       | Kills all project buffers. |
| s-p D       | Opens the root of the project in dired . |
| s-p 4 D     | Opens the root of the project in dired in another window. |
| s-p 5 D     | Opens the root of the project in dired in another frame. |
| s-p e       | Shows a list of recently visited project files. |
| s-p left    | Switch to the previous project buffer. |
| s-p right   | Switch to the next project buffer. |
| s-p E       | Opens the root dir-locals-file of the project. |
| s-p !       | Runs shell-command in the root directory of the project. |
| s-p &       | Runs async-shell-command in the root directory of the project. |
| s-p C       | Runs a standard configure command for your type of project. |
| s-p c       | Runs a standard compilation command for your type of project. |
| s-p P       | Runs a standard test command for your type of project. |
| s-p t       | Toggle between an implementation file and its test file. |
| s-p 4 t     | Jump to implementation or test file in other window. |
| s-p 5 t     | Jump to implementation or test file in other frame. |
| s-p z       | Adds the currently visited file to the cache. |
| s-p p       | Display a list of known projects you can switch to. |
| s-p q       | Display a list of open projects you can switch to. |
| s-p S       | Save all project buffers. |
| s-p m       | Run the commander (an interface to run commands with a single key). |
| s-p x e     | Start or visit an eshell for the project. |
| s-p x i     | Start or visit an ielm (Elisp REPL) for the project. |
| s-p x t     | Start or visit an ansi-term for the project. |
| s-p x s     | Start or visit a shell for the project. |
| s-p x g     | Start or visit a gdb for the project. |
| s-p x v     | Start or visit a vterm for the project. |
| s-p ESC     | Switch to the most recently selected Projectile buffer. |

* `completion-at-point`
* `lsp-find-definition`
* `lsp-find-references`

# Eglot

# LSP - Emacs client/library for the Language Server Protocol

See also eglot, an alternative minimal LSP implementation.

lsp-mode aims to provide IDE-like experience by providing optional integration with the most popular
Emacs packages like company, flycheck and projectile.

* What's the reason ? LSP :: one.py not in project or it is blacklisted.
  https://github.com/emacs-lsp/lsp-mode/issues/2392

* https://clangd.llvm.org/config
  clangd searches for **compile_commands.json** in parents of the source file.

# Magit

|---|---|
| C-x g | magit-status |

# Abbrev — to expand abbreviations

* https://www.emacswiki.org/emacs/AbbrevMode

# Markdown

Preview ne marche pas bien, c'est moche, les tables ne sont pas misent en forme.
