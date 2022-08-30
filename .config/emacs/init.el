;;; init.el --- My init.el  -*- lexical-binding: t; -*-


;; Profile emacs startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "*** Emacs loaded in %s seconds with %d garbage collections."
                     (emacs-init-time "%.2f")
                     gcs-done)))

(prog1 'leaf-setup
  (eval-and-compile
    ;; Setup straight
    (defvar bootstrap-version)
    (let ((bootstrap-file
           (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
          (bootstrap-version 5))
      (unless (file-exists-p bootstrap-file)
        (with-current-buffer
            (url-retrieve-synchronously
             "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
             'silent 'inhibit-cookies)
          (goto-char (point-max))
          (eval-print-last-sexp)))
      (load bootstrap-file nil 'nomessage))

    ;; Install leaf and leaf-keywords
    (straight-use-package 'leaf)
    (straight-use-package 'leaf-keywords)
    (straight-use-package 'leaf-convert)
    (straight-use-package 'hydra)
    (straight-use-package 'blackout)
    (leaf leaf-keywords
      :require t
      :config (leaf-keywords-init))
    ))

(leaf cus-start
  :doc "define customization properties of builtins"
  :tag "builtin" "internal"
  :bind ("C-x C-w" . kill-this-buffer)
  :preface (defun general-init-hook nil
              (menu-bar-mode -1)
              (when-let ((gls (executable-find "gls")))
                (setq insert-directory-program gls dired-use-ls-dired t)
                (setq dired-listing-switches "-al --group-directories-first")))
            (defun on-after-init ()
              (unless (display-graphic-p (selected-frame))
                (set-face-background 'default "unspecified-bg" (selected-frame))))
            (defun copy-from-osx ()
              (shell-command-to-string "pbpaste"))
            (defun paste-to-osx (text &optional push)
              (let ((process-connection-type nil))
                (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
                  (process-send-string proc text)
                  (process-send-eof proc))))
  :hook ((after-init-hook . general-init-hook)
         (window-setup-hook . on-after-init))
  :setq (make-backup-files . nil)
  :custom ((tab-width . 2)
           (frame-resize-pixelwise . t)
           (enable-recursive-minibuffers . t)
           (create-lockfiles)
           (use-dialog-box)
           (use-file-dialog)
           (history-length . 1000)
           (history-delete-duplicates . t)
           (scroll-preserve-screen-position . t)
           (scroll-conservatively . 100)
           (mouse-wheel-scroll-amount quote (1 ((control). 5)))
           (ring-bell-function . 'ignore)
           (visible-bell . t)
           (text-quoting-style . 'straight)
           (truncate-lines . t)
           (fringe-mode . 10)
           (blink-cursor-mode . t)
           (show-paren-mode . t)
           (savehist-mode . t)
           (recentf-auto-cleanup . 'never)
           (save-place-mode . t)
           (save-interprogram-paste-before-kill . t)
           (indent-tabs-mode . nil)
           (interprogram-cut-function . 'paste-to-osx)
           (interprogram-paste-function . 'copy-from-osx))
  :config (defalias 'yes-or-no-p 'y-or-n-p))

(leaf exec-path-from-shell
  :doc "Make Emacs use the $PATH set up by the user's shell"
  :url "https://github.com/purcell/exec-path-from-shell"
  :straight t
  :custom
  (exec-path-from-shell-variables . '("DEEPL_API_KEY" "FLUTTER_ROOT" "PATH"))
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize))
  )

(leaf autorevert
  :doc "revert buffers when files on disk change"
  :tag "builtin"
  :global-minor-mode global-auto-revert-mode
  :custom ((auto-revert-interval . 1)
           (global-auto-revert-non-file-buffers . t)))

(leaf meow
  :straight t
  :load-path "~/.config/emacs/elisp/meow/"
  :config
  (require 'meow-keybindings)
  (meow-setup)
  (meow-global-mode 1))

(leaf magit
  :doc "a git porcelain inside Emacs."
  :straight t
  :bind ("C-c M-m" . magit-status)
  :custom ((magit-refresh-verbose . t)
           (magit-commit-ask-to-stage quote stage)
           (magit-log-margin-show-committer-date . t)
           (magit-log-margin . '(t "%m/%d/%Y %H:%M " magit-log-margin-width t 12)))
  :config
  (leaf magit-delta
    :straight t
    :ensure-system-package (delta . git-delta)
    :after magit
    :hook (magit-mode-hook))
  )

(leaf git-gutter
  :doc "Port of Sublime Text plugin GitGutter"
  :straight t
  :custom
  ((git-gutter:modified-sign . "~")
   (git-gutter:added-sign . "+")
   (git-gutter:deleted-sign . "-")
   (git-gutter:update-interval . 2))
  :custom-face
  ((git-gutter:modified . '((t (:foreground "#f1fa8c"))))
   (git-gutter:added . '((t (:foreground "#50fa7b"))))
   (git-gutter:deleted . '((t (:foreground "#ff79c6")))))
  :global-minor-mode global-git-gutter-mode)

(leaf all-the-icons
  :doc "A utility package to collect various Icon Fonts and propertize them within Emacs"
  :url "https://github.com/domtronn/all-the-icons.el"
  :straight t)

(leaf all-the-icons-completion
  :doc "Add icons to completion candidates in Emacs"
  :url "https://github.com/iyefrat/all-the-icons-completion"
  :straight t
  :after (marginalia all-the-icons)
  :hook (marginalia-mode . all-the-icons-completion-marginalia-setup)
  :init (all-the-icons-completion-mode))

(leaf icons-in-terminal
  :doc "A utility package to propertize Icon Fonts in both GUI and TUI with Emacs."
  :url "https://github.com/tosaka07/icons-in-terminal.el"
  :straight (icons-in-terminal :type git :host github :repo "tosaka07/icons-in-terminal.el"))

(leaf modus-themes
  :disabled t
  :doc "Highly accessible themes (WCAG AAA)"
  :url "https://github.com/protesilaos/modus-themes"
  :straight t
  :init
  (setq
        modus-themes-mode-line '(borderless)
        modus-themes-italic-constructs t
        modus-themes-bold-constructs t
        modus-themes-mixed-fonts nil
        modus-themes-subtle-line-numbers nil
        modus-themes-intense-mouseovers nil
        modus-themes-deuteranopia t
        modus-themes-tabs-accented t
        modus-themes-variable-pitch-ui nil
        modus-themes-inhibit-reload t
        modus-themes-region '(accented)
        modus-themes-syntax '(alt-syntax faint)
        modus-themes-headings
        '((1 . (rainbow overline background 1.4))
          (2 . (rainbow background 1.3))
          (3 . (rainbow bold 1.2))
          (t . (semilight 1.1))))
  (modus-themes-load-vivendi))

(leaf doom-themes
  :doc "A megapack of themes for GNU Emacs."
  :straight t
  :custom
  (doom-themes-enable-bold . t)
  (doom-themes-enable-italic . t)
  (doom-ayu-dark-brighter-comments . t)
  :config
  (load-theme 'doom-tomorrow-night t)
  )

(leaf doom-modeline
  :doc "A fancy and fast mode-line inspired by minimalism design."
  :url "https://github.com/seagle0128/doom-modeline"
  :straight t
  :global-minor-mode (doom-modeline-mode)
  :preface
  (defun my-icon-displayable-p ()
    t)
  :advice ((:before-until doom-modeline-icon-displayable-p my-icon-displayable-p))
  :custom
  (doom-modeline-buffer-file-name-style . 'truncate-with-project)
  (doom-modeline-icon . t)
  (doom-modeline-unicode-fallback . nil)
  (doom-modeline-major-mode-icon . t)
  (doom-modeline-minor-modes . nil)
  (doom-modeline-lsp . t)
  (doom-modeline-modal-icon . t)
  (doom-modeline-env-version . t)
  )

(leaf rainbow-delimiters
  :doc "Highlight brackets according to their depth"
  :url "https://github.com/Fanael/rainbow-delimiters"
  :straight t
  :hook (prog-mode-hook . rainbow-delimiters-mode))

(leaf highlight-indent-guides
  :doc "Emacs minor mode to highlight indentation"
  :url "https://github.com/DarthFennec/highlight-indent-guides"
  :straight t
  :hook (prog-mode-hook . highlight-indent-guides-mode)
  :custom
  ((highlight-indent-guides-auto-enabled . t)
   (highlight-indent-guides-responsive . t)
   (highlight-indent-guides-method . 'character)
   (highlight-indent-guides-suppress-auto-error . t)))

(leaf rainbow-mode
  :doc "Colorize color names in buffers"
  :straight t
  :leaf-defer t
  :hook
  (web-mode-hook . rainbow-mode))

(leaf paren
  :doc "highlight matching paren"
  :tag "builtin"
  :custom
  ((show-paren-delay . 0)
   (show-paren-style . 'mixed))
  :global-minor-mode global-auto-revert-mode)

(leaf posframe
  :straight t)

(leaf go-translate
  :doc "Powerful translator on Emacs. Supports multiple translation engines such as Google, Bing, deepL."
  :url "https://github.com/lorniu/go-translate"
  :straight t
  :bind (("C-c t" . gts-do-translate))
  :defer-config
  (setq gts-translate-list '(("en" "ja") ("ja" "en")))
  (setq gts-default-translator
        (gts-translator
         :picker (gts-prompt-picker)
         :engines (list
                   (gts-deepl-engine :auth-key (getenv "DEEPL_API_KEY") :pro nil))
         :render (gts-buffer-render))))

(leaf beacon
  :doc "A light that follows your cursor around so you don't lose it!"
  :url "https://github.com/Malabarba/beacon"
  :straight t
  :global-minor-mode beacon-mode)

(leaf smartparens
  :straight t
  :require smartparens-config
  :global-minor-mode (smartparens-global-mode)
  :hook
  (prog-mode-hook . turn-on-smartparens-mode))

(leaf ace-window
  :doc "Quickly switch windows in Emacs"
  :url "https://github.com/abo-abo/ace-window"
  :straight t
  :bind ("C-c w" . ace-window)
  :setq-default (aw-keys . '(?j ?k ?h ?l ?a ?s ?d ?f ?g))
  :custom-face (aw-leading-char-face . '((t (:height 4.0 :foreground "#f1fa8c")))))

;; 標準機能の fido-vertical-mode でもいいが、
;; Tab を押すと Window に変換候補が表示されるため修正されるまでこちらを使う
(leaf vertico
  :doc "VERTical Interactive COmpletion"
  :url "https://github.com/minad/vertico"
  :straight t
  :global-minor-mode vertico-mode
  :config
  (leaf vertico-directory
    :after vertico
    :straight nil
    :bind
    (:vertico-map
      ("RET" . vertico-directory-enter)
      ("DEL" . vertico-directory-delete-char)
      ("M-DEL" . vertico-directory-delete-word))
    :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))
)

(leaf consult
  :doc "Consulting completing-read"
  :url "https://github.com/minad/consult"
  :straight t
  :preface
  (defun my-consult-line (&optional at-point)
    "Consult-line uses things-at-point."
    (interactive "P")
    (if at-point
        (consult-line (thing-at-point 'symbol))
      (consult-line)))
  :bind
  ([remap switch-to-buffer] . consult-buffer)
  ([remap list-buffers] . consult-buffer)
  ([remap yank-pop] . consult-yank-pop)
  ([remap goto-line] . consult-goto-line)
  ([remap repeat-complex-command] . consult-complex-command)
  ([remap org-open-at-point] . consult-outline)
  ("C-M-r" . consult-recent-file)
  ("C-c C-r" . consult-file-externally)
  ("C-c v" . my-consult-line)
  ("C-c C-p" . consult-yank-from-kill-ring)
  :config
  (leaf consult-dir
    :doc "Insert paths into the minibuffer prompt in Emacs"
    :url "https://github.com/karthink/consult-dir"
    :after consult
    :straight t
    :bind (("C-c d" . consult-dir)
           (:vertico-map
            ("C-c d" . consult-dir)
            ("C-x j" . consult-dir-jump-file))))
  (leaf consult-ghq
    :after consult
    :bind ("M-g g" . consult-ghq-find)
    :straight t)
  :setq
  ((xref-show-xrefs-function . 'consult-xref)
   (xref-show-definitions-function . 'consult-xref)
   (consult-project-root-function . #'projectile-project-root)
   (consult-ripgrep-command . "rg --null --line-buffered --color=ansi --max-columns=1000 --no-heading --line-number --ignore-case . -e ARG OPTS"))
  )

(leaf affe
  :doc "Asynchronous Fuzzy Finder for Emacs"
  :url "https://github.com/minad/affe"
  :straight t
  :bind
  ("C-c f" . affe-find)
  ("C-c C-f" . affe-grep))

(leaf marginalia
  :doc "Marginalia in the minibuffer"
  :url "https://github.com/minad/marginalia"
  :straight t
  :global-minor-mode marginalia-mode)

(leaf orderless
  :doc "Emacs completion style that matches multiple regexps in any order"
  :url "https://github.com/oantolin/orderless"
  :straight t
  :setq
  (completion-styles . '(orderless))
  (completion-category-overrides . '((file (styles basic partial-completion)))))

(leaf corfu
  :disabled t
  :doc "Completion Overlay Region FUnction"
  :url "https://github.com/minad/corfu"
  :straight t
  :global-minor-mode global-corfu-mode
  :custom
  (corfu-auto . t)
  (corfu-cycle . t)
  (corfu-quit-at-boundary . nil)
  (corfu-quit-no-match . nil)
  (completion-cycle-threshold . 3)
  :config
  (leaf corfu-doc
    :doc "Documentation popup for Corfu"
    :url "https://github.com/galeo/corfu-doc"
    :straight t
    :hook (corfu-mode-hook . corfu-doc-mode))
  (leaf corfu-terminal
    :straight (popon :type git :repo "https://codeberg.org/akib/emacs-popon.git")
    :straight (corfu-terminal :type git :repo "https://codeberg.org/akib/emacs-corfu-terminal.git")
    :straight (corfu-doc-terminal :type git :repo "https://codeberg.org/akib/emacs-corfu-doc-terminal.git")
    :init
    (unless (display-graphic-p)
      (corfu-terminal-mode +1)
      (corfu-doc-terminal-mode +1)))
  )

(leaf cape
  :disabled t
  :doc "Completion At Point Extensions"
  :url "https://github.com/minad/cape"
  :straight t
  :bind (("C-c p p" . completion-at-point) ;; capf
         ("C-c p t" . complete-tag)        ;; etags
         ("C-c p d" . cape-dabbrev)        ;; or dabbrev-completion
         ("C-c p h" . cape-history)
         ("C-c p f" . cape-file)
         ("C-c p k" . cape-keyword)
         ("C-c p s" . cape-symbol)
         ("C-c p a" . cape-abbrev)
         ("C-c p i" . cape-ispell)
         ("C-c p l" . cape-line)
         ("C-c p w" . cape-dict)
         ("C-c p \\" . cape-tex)
         ("C-c p _" . cape-tex)
         ("C-c p ^" . cape-tex)
         ("C-c p &" . cape-sgml)
         ("C-c p r" . cape-rfc1345))
  :defer-config
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  )

(leaf projectile
  :doc "Project Interaction Library for Emacs"
  :url "https://github.com/bbatsov/projectile"
  :straight t)

(leaf treemacs
  :doc "a tree layout file explorer for Emacs"
  :url "https://github.com/Alexander-Miller/treemacs"
  :straight t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :bind (("C-c e e" . treemacs-select-window)
         ("C-c e b" . treemacs)
         ("C-c e s" . treemacs-switch-workspace)
         ("C-c e w" . treemacs-edit-workspaces)
         ("C-c e a" . treemacs-add-project-to-workspace))
  :config
  (treemacs-resize-icons 14)
  :setq
  (treemacs-follow-mode . t)
  (treemacs-indent-guide-mode . t)
  (treemacs-indent-guide-style . 'block)
  (treemacs-project-follow-mode . t)
  (treemacs-filewatch-mode . t))

(leaf vundo
  :doc "Visualize the undo tree."
  :url "https://github.com/casouri/vundo"
  :straight t
  :bind ("C-c u" . vundo))

(leaf whitespace
  :straight t
  :global-minor-mode global-whitespace-mode
  :custom ((whitespace-styles . '(face
                                 trailing
                                 tabs
                                 spaces
                                 empty
                                 space-mark
                                 tab-mark))
           (whitespace-display-mappings . '((space-mark ?\u3000 [?\u25a1])
                                            (tab-mark ?\t [?\u00BB ?\t] [?\\ ?\t])))
           (whitespace-space-regexp . "\\(\u3000+\\)")
           (whitespace-global-modes . '(emacs-lisp-mode shell-script-mode sh-mode python-mode org-mode))
           ))

(leaf yasnippet
  :doc "A template system for Emacs"
  :url "https://github.com/joaotavora/yasnippet"
  :straight t
  :global-minor-mode yas-global-mode)

(leaf markdown-mode
  :doc "Emacs Markdown Mode"
  :url "https://github.com/jrblevin/markdown-mode"
  :straight t
  :mode ("\\.md\\'" . gfm-mode))

(leaf lsp-mode
  :disabled t
  :doc "LSP mode"
  :url "https://github.com/emacs-lsp/lsp-mode"
  :straight t
  :after orderless cape
  :commands lsp lsp-deferred
  :custom `((lsp-keymap-prefix . "C-c l")
            (read-process-output-max . ,(* 1 1024 1024))  ;; 1MB
            (lsp-auto-guess-root . nil)
            (lsp-headerline-breadcrumb-enable . t)
            (lsp-log-io . nil)
            (lsp-trace . nil)
            (lsp-print-performance . nil)
            (lsp-idle-delay . 0.5)
            (lsp-document-sync-method . 2)
            (lsp-response-timeout . 5)
            (lsp-prefer-flymake . nil)
            (lsp-completion-enable . t)
            (lsp-enable-indentation . nil)
            (lsp-restart . 'ignore)
            (lsp-completion-provider . :none)))

(leaf lsp-ui
  :disabled t
  :doc "UI modules for lsp-mode"
  :url "https://github.com/emacs-lsp/lsp-ui"
  :straight t
  :hook (lsp-mode-hook . lsp-ui-mode)
  :preface
  (defun ladicle/toggle-lsp-ui-doc ()
    (interactive)
    (if lsp-ui-doc-mode
        (progn
          (lsp-ui-doc-mode -1)
          (lsp-ui-doc--hide-frame))
      (lsp-ui-doc-mode 1)))

  :bind (lsp-mode-map
         :package lsp-mode
         ("C-c C-r" . lsp-ui-peek-find-references)
         ("C-c C-j" . lsp-ui-peek-find-definitions)
         ("C-c s"   . lsp-ui-sideline-mode)
         ("C-c d"   . ladicle/toggle-lsp-ui-doc)
         ("C-c i"   . lsp-ui-doc-focus-frame))
  :custom ((lsp-ui-doc-header . t)
           (lsp-ui-doc-delay . 2)
           (lsp-ui-doc-include-signature . t)
           (lsp-ui-doc-alignment . 'window)
           (lsp-ui-doc-max-height . 30)
           (lsp-ui-doc-show-with-mouse . nil)
           (lsp-ui-doc-show-with-cursor . t)
           (lsp-ui-sideline-enable . nil)
           (lsp-ui-sideline-ignore-duplicate . t)
           (lsp-ui-sideline-show-symbol . t)
           (lsp-ui-sideline-show-hover . t)
           (lsp-ui-sideline-show-diagnostics . nil)
           (lsp-ui-sideline-show-code-actions . nil)
           (lsp-ui-imenu-enable . nil)))

(leaf lsp-dart
  :straight t
  :hook (dart-mode . lsp)
  )

(leaf lsp-bridge
  :doc "Fastest LSP client for Emacs"
  :url "https://github.com/manateelazycat/lsp-bridge"
  :load-path ("~/workspace/sources/github.com/manateelazycat/lsp-bridge/")
  :require t
  :bind (("C-c ; d" . lsp-bridge-find-def-other-window)
         ("C-c ; i" . lsp-bridge-find-impl-other-window)
         ("C-c ; ;" . lsp-bridge-lookup-documentation)
         ("C-c ; r" . lsp-bridge-rename)
         ("C-c ; l" . lsp-bridge-list-diagnostics)
         ("C-c ; h" . lsp-bridge-signature-help-fetch)
         ("C-c ; a" . lsp-bridge-code-action)
         ("C-c ; n" . lsp-bridge-popup-complete))
  :custom (lsp-bridge-lookup-doc-tooltip-max-height . 40)
  :config (global-lsp-bridge-mode)
  )

;; (leaf dart-mode
;;   :straight t
;;   :mode ("\\.dart\\'"))

(leaf tree-sitter-langs
  :straight (tree-sitter-langs :type git :host github :repo "tosaka07/tree-sitter-langs"))

(leaf tree-sitter
    :url "https://github.com/ubolonton/emacs-tree-sitter"
    :straight t
    :require tree-sitter-langs
    :global-minor-mode (global-tree-sitter-mode)
    :hook (tree-sitter-after-on-hook . tree-sitter-hl-mode)
    :config
    (tree-sitter-require 'tsx) ;; TSX
    (add-to-list 'tree-sitter-major-mode-language-alist '(typescript-tsx-mode . tsx))
    ;; Hightlight
    (tree-sitter-hl-add-patterns 'tsx
      [
       ;; styled.div``
       (call_expression
        function: (member_expression
                   object: (identifier) @function.call
                   (.eq? @function.call "styled"))
        arguments: ((template_string) @property.definition
                    (.offset! @property.definition 0 1 0 -1)))
       ;; styled(Component)``
       (call_expression
        function: (call_expression
                   function: (identifier) @function.call
                   (.eq? @function.call "styled"))
        arguments: ((template_string) @property.definition
                    (.offset! @property.definition 0 1 0 -1)))
       ])
    )

;; (leaf dart-server
;;   :straight t
;;   :mode ("\\.dart\\'")
;;   :custom
;;   (dart-server-format-on-save . t)
;;   )

(provide 'init)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(beacon go-translate posframe rainbow-mode highlight-indent-guides rainbow-delimiters doom-modeline modus-themes git-gutter magit-delta magit meow blackout el-get hydra leaf-keywords leaf)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(git-gutter:added ((t (:foreground "#50fa7b"))) nil "Customized with leaf in `git-gutter' block at `/Users/shogo/.config/emacs/init.el'")
 '(git-gutter:deleted ((t (:foreground "#ff79c6"))) nil "Customized with leaf in `git-gutter' block at `/Users/shogo/.config/emacs/init.el'")
 '(git-gutter:modified ((t (:foreground "#f1fa8c"))) nil "Customized with leaf in `git-gutter' block at `/Users/shogo/.config/emacs/init.el'"))
