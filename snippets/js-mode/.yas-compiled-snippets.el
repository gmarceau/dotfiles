;;; Compiled snippets and support files for `js-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'js-mode
                     '(("require" "const $1 = require('$0$1');\n" "require" nil nil nil "/home/gmarceau/dotfiles/snippets/js-mode/require" nil nil)
                       ("import" "import $1 from '$1';\n$0\n" "import" nil nil nil "/home/gmarceau/dotfiles/snippets/js-mode/import" nil nil)
                       ("dp" "console.log('--`(line-number-at-pos)`', $0);" "debug-print" nil nil nil "/home/gmarceau/dotfiles/snippets/js-mode/debug-print" "C-c 0" nil)))


;;; Do not edit! File generated at Tue Mar 13 13:43:20 2018
