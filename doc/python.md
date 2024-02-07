Look at [EmacsWiki : Python Programming In Emacs](https://www.emacswiki.org/emacs/PythonProgrammingInEmacs)

---

[Frustrating python lsp experience : r/emacs](https://www.reddit.com/r/emacs/comments/17uyy08/frustrating_python_lsp_experience/)
-> [Dynamically-calculated eglot-workspace-configuration for python poetry venv's, what am I doing wrong?](https://github.com/joaotavora/eglot/discussions/967#discussioncomment-6500223)
```
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               `((python-mode python-ts-mode) .
		 ,(eglot-alternatives '("pylsp" "pyls" ("poetry" "run" "pyright-langserver" "--stdio")  ("pyright-langserver" "--stdio") "jedi-language-server")))))
```

[Poetry - Python dependency management and packaging made easy](https://python-poetry.org/)
[GitHub - microsoft/pyright: Static Type Checker for Python](https://github.com/microsoft/pyright)

[pycodestyle](https://pycodestyle.pycqa.org/en/latest/)
formerly pep8

[GitHub - emacs-lsp/emacs: Mirror of GNU Emacs](https://github.com/emacs-lsp/emacs)
A Emacs fork implementing non-blocking and async JSONRPC support

---

**Attention, il y a deux `python-mode` celui d'Emacs et le package externe !**

---

[jorgenschaefer/elpy: Emacs Python Development Environment](https://github.com/jorgenschaefer/elpy)
looking for maintainer

[davidhalter/jedi: Awesome autocompletion, static analysis and refactoring library for python](https://github.com/davidhalter/jedi)

---

M-x python-shift-right (C-c >)
M-x python-shift-left  (C-c <)

https://emacs.stackexchange.com/questions/69291/how-can-i-make-python-mode-correctly-indent-pythons-match-statement
How can I make python-mode correctly indent python's match statement?

# python-mode

backward-kill-word efface tout le mot au lieu de s’arrêter à _
https://github.com/emacs-mirror/emacs/blob/master/lisp/simple.el
https://emacs.stackexchange.com/questions/30401/backward-kill-word-kills-too-much-how-to-make-it-more-intelligent
zap-to-char
https://www.gnu.org/software/emacs/manual/html_node/eintr/Complete-zap_002dto_002dchar.html
https://github.com/mrkkrp/zzz-to-char

<,> tab ouvre une liste ???

comment se placer au début de la ligne ?
pas C-a C-e move-beginning-of-line move-end-of-line
-> C-M-left C-M-right backward-sexp forward-sexp

https://github.com/abo-abo/avy
https://karthinks.com/software/avy-can-do-anything

# LSP...

flycheck or lsp ???

https://github.com/emacs-lsp/lsp-mode/issues/new?assignees=&labels=bug&template=bug_report.yml
Python: lsp must report the source and not "lsp"
I just experienced troubles using lsp-mode and I noticed a lot of users are complaining about spam.

For python-lsp, a lot of checkers are enabled by default under the hood.
Usually, users should be firstly interested in a reporting of syntax errors and not by a flood of cosmetic warnings.
In order to avoid a very bad experience with lsp-mode, it should report the source and not "lsp" as the checker.
This would help to disable the unwanted checks.
For my case, I discovered in the JSON log that pydocstyle was enabled and flooding me.

Moreover, I think the verify-setup tool is not exhaustive.
And we could also improve the documentation.

---

LSP and Python: flake8 settings are not honored
https://github.com/syl20bnr/spacemacs/issues/15137
lsp blackend is enabled whenever lsp layer is enabled. you have to explicitly set python layer variable to disable it.
 (add-hook 'python-mode-hook
            (lambda ()
              ;; Enable fill column indicator
              ;; (display-fill-column-indicator-mode)
              (display-fill-column-indicator-mode)
              ;; Turn on line numbering
              ;; (linum-mode t)
              (setq fill-column 79)
              ;; set in-block indentation scale
              (setq python-indent-def-block-scale 1)
              ;; Set tab-width to 4
              (setq tab-width 4
                    evil-shift-width 4)
              (setq flycheck-checker 'python-flake8)
              ;; Enable automatic line wrapping at fill column
              (auto-fill-mode t)))

https://github.com/sublimelsp/LSP/issues/239

https://github.com/python-lsp/python-lsp-server

python-lsp-server depends on other tools, like flake8 and pycodestyle. These tools can be configured via settings passed from the client (as above), or alternatively from other configuration sources. The following sources are available:

    pycodestyle: discovered in ~/.config/pycodestyle, setup.cfg, tox.ini and pycodestyle.cfg.
    flake8: discovered in ~/.config/flake8, .flake8, setup.cfg and tox.ini

The default configuration sources are pycodestyle and pyflakes. If you would like to use flake8, you will need to:

    Disable pycodestyle, mccabe, and pyflakes, by setting their corresponding enabled configurations, e.g. pylsp.plugins.pycodestyle.enabled, to false. This will prevent duplicate linting messages as flake8 includes these tools.
    Set pylsp.plugins.flake8.enabled to true.
    Change the pylsp.configurationSources setting (in the value passed in from your client) to ['flake8'] in order to use the flake8 configuration instead.
