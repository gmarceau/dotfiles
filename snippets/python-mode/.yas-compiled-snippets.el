;;; Compiled snippets and support files for `python-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'python-mode
                     '(("unittest" "import unittest\n\nclass ${1:`(file-name-nondirectory (file-name-sans-extension (buffer-file-name)))`}(unittest.TestCase):\n\n    def setUp(self):\n        pass\n\n    def tearDown(self):\n        pass\n\n    def test_$0(self):\n        pass\n\npass\n\nif __name__ == '__main__':\n    unittest.main()\n" "unittest" nil nil nil "/home/gmarceau/dotfiles/snippets/python-mode/unittest" nil nil)
                       ("disable" "# pylint: disable=" "pylint-disable" nil nil nil "/home/gmarceau/dotfiles/snippets/python-mode/pylint-disable" nil nil)
                       ("cli" "#!/usr/bin/env python\nfrom plumbum import cli, local\n\nclass ${1:`(file-name-nondirectory (file-name-sans-extension (buffer-file-name)))`}(cli.Application):\n\n    switch = cli.SwitchAttr('--switch', argtype=str,\n                               help='Change this to a real switch')\n\n    flag = cli.Flag('--the-flag', help='Change this to a real flag')\n\n    def main(self, *args):  # pylint: disable=arguments-differ\n        $0pass\n\npass\nif __name__ == '__main__':\n    $1.run()\n" "plumbum-cli" nil nil nil "/home/gmarceau/dotfiles/snippets/python-mode/plumbum-cli" nil nil)
                       ("pdb" "import pdb; pdb.set_trace()\n" "pdb-trace" nil nil nil "/home/gmarceau/dotfiles/snippets/python-mode/pdb-trace" nil nil)
                       ("main" "if __name__ == '__main__':\n   $0" "main" nil nil nil "/home/gmarceau/dotfiles/snippets/python-mode/main" nil nil)
                       ("''" "'''\n$0\n'''\n" "docstring" nil nil nil "/home/gmarceau/dotfiles/snippets/python-mode/docstring" nil nil)
                       ("dp" "print('--`(line-number-at-pos)`', $0)\n" "debug-print" nil nil nil "/home/gmarceau/dotfiles/snippets/python-mode/debug-print" "C-c 0" nil)))


;;; Do not edit! File generated at Tue Mar 13 13:43:20 2018
