;;; .el --- ... -*- lexical-binding: t; -*-

;; Fixme: takes 8s to do ???

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; straight
;;   https://github.com/radian-software/straight.el

;; To configure an alternate way for straight.el to check for modifications made to package source
;; code, rather than the default (which is 100% reliable, but has a minor cost to startup time).
(setq straight-check-for-modifications nil)

(doom--log "before straight bootstrap")
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (doom--log "get straight.el")
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (doom--log "load straight.el")
  (load bootstrap-file nil 'nomessage))
(doom--log "after straight bootstrap")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst _used_packages
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

(defconst _packages_to_load
  '(
    gcmh
    ;; //github.com/noctuid/general.el
    ;; More convenient key definitions in emacs
    general
    ))

;; Fixme: useless ?
;;        snippets issue ?
;; (doom--log "before straight-use-package")
(dolist (_ _packages_to_load)
  (straight-use-package _))
;; (doom--log "after straight-use-package")

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

;; Configure use-package to use straight.el by default
;; instead of `straight t`
(use-package straight
  :custom
  (straight-use-package-by-default t))

(doom--log "package-straight done")
(provide 'package-straight)
