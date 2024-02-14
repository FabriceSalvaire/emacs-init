;;; .el --- ... -*- lexical-binding: t; -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Packages
;;   https://melpa.org/#/getting-started

;; https://www.olivertaylor.net/emacs/notes-on-package-el.html

;; Emacs "activates" all installed packages before reading the user-init-file unless
;; package-enable-at-startup is set to nil in the early init file.

;; When we interactively install a package (via M-x package-install) Emacs adds that package to
;; package-selected-packages in the custom-file.

;; (setq package-enable-at-startup nil) ; faster ???

(require 'package)

(add-to-list 'package-archives '("melpa"        . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)

;; Prefer GNU over MELPA
(setq package-archive-priorities '(("gnu" . 20) ("melpa" . 10)))

;; required if package-enable-at-startup is set to nil
;; (package-initialize)

;; Install packages with (package-install-selected-packages)
;; Remove packages with (package-autoremove)
(setq package-selected-packages
  '(
    all-the-icons-ivy
    anything
    apache-mode
    auctex-lua
    auto-complete
    auto-complete-c-headers
    cff
    clang-format
    clean-aindent-mode
    cmake-font-lock
    cmake-ide
    cmake-mode
    cmake-project
    column-marker
    company-anaconda
    company-auctex
    company-box
    company-c-headers
    company-irony
    company-irony-c-headers
    company-jedi
    company-math
    company-qml
    company-shell
    cpputils-cmake
    csharp-mode
    cuda-mode
    dap-mode
    django-mode
    docker-compose-mode
    dockerfile-mode
    doom-modeline
    dotenv-mode
    flycheck-color-mode-line
    flycheck-grammalecte
    flycheck-irony
    flycheck-kotlin
    flycheck-pos-tip
    flycheck-pycheckers
    flycheck-tip
    flymake-shell
    flyspell-correct-ivy
    flyspell-lazy
    glsl-mode
    go-mode
    google-translate
    gradle-mode
    graphql-mode
    graphviz-dot-mode
    haskell-mode
    highlight-indentation
    icicles
    icomplete+
    importmagic
    indent-guide
    irony-eldoc
    jinja2-mode
    json-mode
    kotlin-mode
    language-detection
    languagetool
    live-py-mode
    magit
    markdown-mode
    markdown-mode+
    markdown-preview-mode
    markdown-toc
    modern-cpp-font-lock
    php-mode
    pippel
    powerline
    projectile
    projectile-ripgrep
    projectile-speedbar
    protobuf-mode
    pyimport
    pylint
    python-django
    pyvenv
    qml-mode
    react-snippets
    rjsx-mode
    sass-mode
    scss-mode
    smartparens
    spice-mode
    string-inflection
    systemd
    tern
    treemacs-all-the-icons
    treemacs-icons-dired
    treemacs-magit
    treemacs-perspective
    treemacs-projectile
    treemacs-tab-bar
    typoscript-mode
    vue-mode
    which-key
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; use-package
;;   https://github.com/jwiegley/use-package

;; use-package.el is not needed at runtime
;; to further reduce load time
(eval-when-compile
  (require 'use-package))
;;(require 'diminish)

;; Information about package loads in the *Messages* buffer
(setq use-package-verbose t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Take too much time !
;; (package-refresh-contents)
