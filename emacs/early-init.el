;;; early-init.el --- Settings needed before packages and the first frame  -*- lexical-binding: t -*-

;; Emacs reads this from ~/.emacs.d/early-init.el, which is a symlink to
;; this file (see README.md).

(defvar dotfiles-early-init-loaded t
  "Non-nil once this file has run; core.el warns when it hasn't.")

;; Log native-compiler warnings instead of popping up *Warnings*
(setq native-comp-async-report-warnings-errors 'silent)

;; Set before the first frame is drawn, so it opens without a toolbar and
;; at its final size.  The default face in custom.el still sets the font.
;; `tool-bar-mode' may not be defined yet: use the frame parameter, and
;; preset the variable so the mode is off once tool-bar.el loads.
(push '(tool-bar-lines . 0) default-frame-alist)
(setq tool-bar-mode nil)
(push '(font . "Menlo-18") default-frame-alist)

(with-eval-after-load 'package
  (add-to-list 'package-archives
               '("melpa-stable" . "https://stable.melpa.org/packages/") t)
  (add-to-list 'package-archives
               '("melpa" . "https://melpa.org/packages/") t))
