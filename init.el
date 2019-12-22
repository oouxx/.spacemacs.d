;;; Setup -*- lexical-binding: t; -*-
;;;; Commentary
;;;; Constants

;;(defconst eric?    (string= "Eric Kaschalk" (user-full-name)) "Am I me?")
(defconst linux?   (eq system-type 'gnu/linux) "Are we on a linux machine?")
(defconst mac?     (eq system-type 'darwin)    "Are we on a macOS machine?")
(defconst windows? (not (or linux? mac?))      "Are we on windows machine?")

;;;; Configuration

(defvar server? t
  "Alias `dotspacemacs-enable-server'. Defaults to nil for non-eric users.")

(defvar redo-bindings? nil
  "Redo spacemacs bindings? Defaults to, and I recommend, nil to non-eric users.

See the commentary in the config layer's local pkg `redo-spacemacs'.")

;;; Spacemacs/
;;;; Spacemacs/init

(defun dotspacemacs/init ()
  "Instantiate Spacemacs core settings.

   All `dotspacemacs-' variables with values set different than their defaults.

They are all defined in `~/.emacs.d/core/core-dotspacemacs.el'.
Check `dotspacemacs/get-variable-string-list' for all vars you can configure."
  (setq-default
   dotspacemacs-default-font '("Source Code Pro"
                               :size 14
                               :weight normal
                               :width normal)
   dotspacemacs-themes       '(
                               monokai
                               spacemacs-light
                               spacemacs-dark
                               solarized-dark
                               solarized-light
                               dracula
                               )

   ;; General
   dotspacemacs-auto-generate-layout-names t
   dotspacemacs-editing-style              '(vim :variables
                                                 vim-style-visual-feedback t
                                                 vim-style-remap-Y-to-y$ t)
   dotspacemacs-elpa-https                 t
   dotspacemacs-elpa-subdirectory          nil
   dotspacemacs-enable-server              server?
   dotspacemacs-fullscreen-at-startup      t
   dotspacemacs-large-file-size            5
   dotspacemacs-persistent-server          server?
   dotspacemacs-pretty-docs                t
   dotspacemacs-search-tools               '("ag" "rg" "pt" "ack" "grep")
   dotspacemacs-scratch-mode               'org-mode
   dotspacemacs-startup-lists              '((recents . 5)
                                             (projects . 7))
   dotspacemacs-whitespace-cleanup         'trailing


   ;; The following are unchanged but are still required for reloading via
   ;; 'SPC f e R' `dotspacemacs/sync-configuration-layers' to not throw warnings
   dotspacemacs-emacs-leader-key  "M-m"
   dotspacemacs-emacs-command-key "SPC"
   dotspacemacs-leader-key        "SPC"
   dotspacemacs-mode-line-theme   'spacemacs))

;;;; Spacemacs/layers

(defun dotspacemacs/layers ()
  "Instantiate Spacemacs layers declarations and package configurations."
  (setq-default
   dotspacemacs-configuration-layers     '(
                                          (auto-completion :variables auto-completion-enable-sort-by-usage t
                                                                 auto-completion-enable-snippets-in-popup t
                                                                 auto-completion-tab-key-behavior 'cycle
                                                                 :disabled-for org markdown)
                                          better-defaults
                                          (git :variables
                                               git-magit-status-fullscreen t
                                               magit-push-always-verify nil
                                               magit-save-repository-buffers 'dontask
                                               magit-revert-buffers 'silent
                                               magit-refs-show-commit-count 'all
                                               magit-revision-show-gravatars nil)
                                          (ivy :variables
                                               ivy-extra-directories nil
                                               ivy-enable-advanced-buffer-information nil)
                                          (org :variables
                                              org-want-todo-bindings t)
                                          syntax-checking
                                          (version-control :variables
                                                          version-control-global-margin t
                                                          version-control-diff-tool 'git-gutter+)
                                          (spacemacs-layouts :variables layouts-enable-autosave nil
                                                             layouts-autosave-delay 300)
                                          colors
                                          graphviz
                                          ranger
                                          (ibuffer :variables
                                                  ibuffer-group-buffers-by 'projects)

                                          html
                                          latex
                                          markdown
                                          yaml
                                          lsp
                                          emacs-lisp
                                          go
                                          (c-c++ :variables
                                                c-c++-backend 'lsp-clangd
                                                c-c++-enable-google-style t
                                                c-c++-enable-google-newline t)

                                          (python :variables
                                                  python-backend 'lsp
                                                  python-lsp-server 'pyls
                                                  python-test-runner 'pytest
                                                  python-spacemacs-indent-guess nil)
                                           java
                                           (chinese :variables chinese-default-input-method 'pinyin
                                                    chinese-enable-youdao-dict t)
                                           ;;(default-optimize :location local)
                                           ;;(misc :location local)
                                           ;;(display  :location local)
                                           ;;(programming :location local)
                                           ;;(my-org :location local)
                                           )
   dotspacemacs-configuration-layer-path '("~/.spacemacs.d/layers/")
   dotspacemacs-additional-packages      '()
   dotspacemacs-install-packages 'used-only
   dotspacemacs-frozen-packages          '()
   dotspacemacs-excluded-packages
   '(;; Must Exclude (for styling, functionality, bug-fixing reasons) fringe importmagic scss-mode vi-tilde-fringe
                    org-projectile org-brain magit-gh-pulls magit-gitflow  evil-mc realgud tern company-tern
                    evil-args evil-ediff evil-exchange evil-unimpaired
                    evil-indent-plus volatile-highlights smartparens
                    spaceline holy-mode skewer-mode rainbow-delimiters
                    highlight-indentation vi-tilde-fringe eyebrowse ws-butler
                    org-bullets smooth-scrolling org-repo-todo org-download org-timer
                    livid-mode git-gutter git-gutter-fringe  evil-escape
                    leuven-theme gh-md evil-lisp-state spray lorem-ipsum symon
                    ac-ispell ace-jump-mode auto-complete auto-dictionary
                    clang-format define-word google-translate disaster epic
                    fancy-battery org-present orgit orglue spacemacs-theme
                    helm-flyspell flyspell-correct-helm clean-aindent-mode
                    helm-c-yasnippet ace-jump-helm-line helm-make magithub
                    helm-themes helm-swoop helm-spacemacs-help smeargle
                    ido-vertical-mode flx-ido company-quickhelp ivy-rich helm-purpose
     )))

;;;; Spacemacs/user-init

(defun dotspacemacs/user-init ()
  "Package independent settings to run before `dotspacemacs/user-config'."
  (fringe-mode 0)
  (setq custom-file "~/.spacemacs.d/.custom-settings.el")
  (setq configuration-layer-elpa-archives
        '(("melpa-cn" . "http://mirrors.cloud.tencent.com/elpa/melpa/")
          ("org-cn"   . "http://mirrors.cloud.tencent.com/elpa/org/")
          ("gnu-cn"   . "http://mirrors.cloud.tencent.com/elpa/gnu/")))
  ;; enable proxy
  ;; anacond-mode will fail
  ;;(setq socks-server '("Default server" "127.0.0.1" 1080 5))
  )

;;;; Spacemacs/user-config
;;;;; Post Layer Load

(defun dotspacemacs/user-config/post-layer-load-config ()
  "Configuration to take place *after all* layers/pkgs are instantiated."
  (when (and (boundp 'redo-bindings?) redo-bindings?
             (configuration-layer/package-used-p 'redo-spacemacs))
    (redo-spacemacs-bindings))

  ;; While toggling with `toggle-frame-fullscreen' works, I could not get
  ;; it to work as a hook attached to the frame-make or window-setup.
  ;; Depending on your OS, you may need a different/not-at-all need this.
  (when (and linux? server?)
    (add-to-list 'default-frame-alist '(fullscreen . fullboth)))
  )


;;;;; Core

(defun dotspacemacs/user-config ()
  "Configuration that cannot be delegated to layers."
  (dotspacemacs/user-config/post-layer-load-config)

  ;;解决org表格里面中英文对齐的问题 
  (when (configuration-layer/layer-usedp 'chinese)
    (when (and (spacemacs/system-is-mac) window-system)
      (spacemacs//set-monospaced-font "Source Code Pro" "Hiragino Sans GB" 14 16)))

  ;; force horizontal split window
  (setq split-width-threshold 120)
  ;; (linum-relative-on)

  (spacemacs|add-company-backends :modes text-mode)
  (add-hook 'doc-view-mode-hook 'auto-revert-mode)

  (global-hungry-delete-mode t)
  (spacemacs|diminish helm-gtags-mode)
  (spacemacs|diminish ggtags-mode)
  (spacemacs|diminish which-key-mode)
  (spacemacs|diminish spacemacs-whitespace-cleanup-mode)
  (spacemacs|diminish counsel-mode)
  )

