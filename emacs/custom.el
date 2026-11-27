;;; custom.el --- Customize output only; settings live in the topic files  -*- lexical-binding: t -*-

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(tango-dark))
 '(package-vc-selected-packages
   '((monet :url "https://github.com/stevemolitor/monet")
     (claude-code :url "https://github.com/stevemolitor/claude-code.el")))
 '(safe-local-eval-forms '((text-scale-mode t)))
 '(safe-local-variable-values
   '((text-scale-mode-amount . 2) (text-scale-mode-amount . 3))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :extend nil :stipple nil :background "#161819" :foreground "#eeeeec" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight regular :height 180 :width normal :foundry "nil" :family "Menlo")))))
