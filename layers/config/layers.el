;;; -*- lexical-binding: t; -*-

(configuration-layer/declare-layers
 '(;; Core
   (auto-completion :variables
                    auto-completion-return-key-behavior 'complete
                    auto-completion-tab-key-behavior 'complete
                    auto-completion-enable-snippets-in-popup t
                    auto-completion-enable-sort-by-usage t
                    )
   better-defaults
   git
   (dap :variables
        dap-enable-mouse-support t)
   (ivy :variables
        ivy-extra-directories nil)
   (org :variables
        org-want-todo-bindings t
        org-enable-hugo-support t)
   (shell :variables
          shell-default-shell 'eshell)
   syntax-checking
   (version-control :variables
                    version-control-global-margin t
                    version-control-diff-tool 'git-gutter+)

   ;; Misc
   graphviz
   ranger
   (ibuffer :variables
            ibuffer-group-buffers-by 'projects)
   ;; web-service
   search-engine

   ;; Markups
   csv
   html
   markdown
   yaml

   ;; Languages
   lsp
   ;;clojure
   emacs-lisp
   hy
   java
   prettier
   (javascript :variables
               javascript-import-tool 'import-js
               javascript-fmt-on-save t
               javascript-fmt-tool 'prettier
               javascript-backend 'lsp
               )
   go
   (c-c++ :variables
          c-c++-backend 'lsp-clangd
          c-c++-default-mode-for-headers 'c++-mode
          c++-enable-organize-includes-on-save t
          c-c++-enable-clang-format-on-save t
          c-c++-enable-google-style t
          c-c++-enable-google-newline t)

   (python :variables
           python-backend 'lsp
           python-lsp-server 'pyls
           python-formatter 'yapf
           python-pipenv-activate t
           python-test-runner 'pytest
           python-spacemacs-indent-guess nil)
   ))
