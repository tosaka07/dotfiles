;; early-init.el --- Early Initialization. -*- lexical-binding: t no-byte-compile: t -*-

;; Reduce overhead
;; https://zenn.dev/zk_phi/books/cba129aacd4c1418ade4/viewer/dcebc13578d42055f8a4
(defvar default-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)
(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'emacs-startup-hook
          (lambda ()
            "Restore defalut values after startup."
            (setq file-name-handler-alist default-file-name-handler-alist
                  gc-cons-threshold (* 512 1024 1024))))

;; Basic
(prefer-coding-system 'utf-8)
(set-file-name-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-clipboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(set-default 'buffer-file-coding-system 'utf-8)

;; Keymap
(global-set-key (kbd "C-h") 'delete-backward-char)

;; UI
(push '(vertical-scroll-bars) default-frame-alist)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(scroll-bar-mode . nil) default-frame-alist)
(push '(blink-cursor-mode . nil) default-frame-alist)
(push '(column-number-mode . nil) default-frame-alist)

(provide 'early-init)
