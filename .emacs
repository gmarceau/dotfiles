(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)


(setenv "BODYLABS" "/Users/gmarceau/Documents/core")

(setenv "PYTHONPATH"
	(concat
	 (getenv "BODYLABS")
	 ":/Users/gmarceau/Documents"
	 ":/usr/local/lib/python2.7/site-packages"
	 ":/Library/Python/2.7/site-packages"
	 ":/Users/gmarceau/miniconda/lib/python2.7/site-packages"
	 ":"
	 (getenv "PYTHONPATH")))

(setenv "PATH"
	(concat
	 ":/Users/gmarceau/.local/bin"
	 ":/Users/gmarceau/miniconda/bin"
	 ":/usr/local/bin"
	 ":"
	 (getenv "PATH")))

(package-initialize)
(elpy-enable)
(define-key elpy-mode-map [(control down)] nil)
(define-key elpy-mode-map [(control up)] nil)
(define-key elpy-mode-map "\M-]" 'elpy-goto-definition)
(define-key elpy-mode-map "\M-[" 'pop-tag-mark)

(defun gcm-scroll-down ()
  (interactive)
  (scroll-up 3))
(defun gcm-scroll-up ()
  (interactive)
  (scroll-down 3))

(defun recompile ()
  (interactive)
  (compile compile-command))

(defun join-line-backward ()
  (interactive)
  (join-line -1)
  (indent-for-tab-command))

(global-set-key [(control down)] 'gcm-scroll-down)
(global-set-key [(control up)]   'gcm-scroll-up)

(global-set-key "\C-cc" 'calculator)
(global-set-key "\C-cp" 'compile)
(global-set-key "\C-cr" 'revert-buffer)
(global-set-key "\C-ca" 'magit-status)
(global-set-key "\C-co" 'occur)
(global-set-key "\C-ci" 'set-fill-column)
(global-set-key [f3] 'ido-switch-buffer)
(global-set-key [f9] 'recompile)
(global-set-key "\C-j" 'join-line-backward)
(global-set-key [M-S-up] 'move-text-up)
(global-set-key [M-S-down] 'move-text-down)

(defalias 'yes-or-no-p 'y-or-n-p)
(ido-mode)

(setq magit-last-seen-setup-instructions "1.4.0")
(delete-selection-mode)

(tool-bar-mode 0)
(blink-cursor-mode 0)
(setq mac-command-modifier 'control)
(setq inhibit-splash-screen t)
(setq visible-bell 1)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
(setq mouse-wheel-progressive-speed nil)

(require 'compile)
(add-to-list
 'compilation-error-regexp-alist
 '("^\\([^ \n]+\\)(\\([0-9]+\\)): \\(?:error\\|.\\|warnin\\(g\\)\\|remar\\(k\\)\\)"
   1 2 nil (3 . 4)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-revert-interval 0.5)
 '(autopair-blink t)
 '(autopair-blink-delay 0.05)
 '(autopair-global-mode t)
 '(compilation-ask-about-save nil)
 '(custom-enabled-themes (quote (tango-dark)))
 '(ispell-program-name "/usr/local/bin/ispell")
 '(traad-server-program (quote ("/Users/gmarceau/miniconda/bin/traad"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )



(add-hook 'python-mode-hook 'eldoc-mode)
(eval-after-load "company"
 '(progn
   (add-to-list 'company-backends 'company-anaconda)))

