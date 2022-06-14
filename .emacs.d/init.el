;;; init.el --- My init.el  -*- lexical-binding: t; -*-


;; ---------------------------------
;; Performance
;; ---------------------------------

;; The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))

;; Profile emacs startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "*** Emacs loaded in %s seconds with %d garbage collections."
                     (emacs-init-time "%.2f")
                     gcs-done)))


;; ---------------------------------
;; Package manager
;; ---------------------------------

;; <leaf-install-code>
(eval-and-compile
  (customize-set-variable
   'package-archives '(("org" . "https://orgmode.org/elpa/")
                       ("melpa" . "https://melpa.org/packages/")
                       ("gnu" . "https://elpa.gnu.org/packages/")))
  (package-initialize)
  (unless (package-installed-p 'leaf)
    (package-refresh-contents)
    (package-install 'leaf))

  (leaf leaf-keywords
    :ensure t
    :init
    (leaf hydra :ensure t)
    (leaf el-get :ensure t)
    (leaf blackout :ensure t)

    :config
    ;; initialize leaf-keywords.el
    (leaf-keywords-init)))
;; </leaf-install-code>

;; s(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
;;  '(package-selected-packages '(blackout el-get hydra leaf-keywords leaf)))
;; (custom-set-faces
 ;; cujjjjjjjkkkkkkkkjhhlhjjkkuSTOMu-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
;;  )


;; ---------------------------------
;; Package: meow
;; ---------------------------------

(leaf meow
  :ensure t
  :load-path "~/.emacs.d/elisp/meow/"
  :config
  (require 'meow-keybindings)
  (meow-setup)
  (meow-global-mode 1))


;; ---------------------------------
;; Package: google translate
;; ---------------------------------

(leaf google-translate
  :require t
  :bind ("C-c t" . google-translate-smooth-translate)
  :custom
  (google-translate-translation-directions-alist . '(("en" . "ja")
                                                     ("ja" . "en")))  
  (google-translate-backend-method . 'curl)
  (google-translate-output-destination . 'popup)
  (google-translate-text-face . 'nil)
  :advice (:override google-translate--search-tkk
                     google-translate--search-tkk-override-advice)
  :preface
  (defun google-translate--search-tkk-override-advice ()
    "Search TKK." (list 430675 2721866130)))

;; ---------------------------------
;; General config
;; ---------------------------------

;; line number
(global-display-line-numbers-mode t)
(custom-set-variables '(display-line-numbers-width-start t))

;; tool bar
(tool-bar-mode 0)
(menu-bar-mode 0)

;; clipboard
(defun copy-from-osx ()
(shell-command-to-string "pbpaste"))

(defun paste-to-osx (text &optional push)
(let ((process-connection-type nil))
(let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
(process-send-string proc text)
(process-send-eof proc))))

(setq interprogram-cut-function 'paste-to-osx)
(setq interprogram-paste-function 'copy-from-osx) 


