;;; agent-tools-test.el --- Tests for agent-tools.el -*- lexical-binding: t -*-

;; Run: emacs -Q --batch -l ~/dotfiles/lisp/agent-tools-test.el -f ert-run-tests-batch-and-exit
;; `hc' and the git repository are fakes made in temporary directories.

;;; Code:

(require 'ert)
(require 'ert-x)
(package-initialize)
(load (expand-file-name "agent-tools" (file-name-directory (or load-file-name buffer-file-name))))

;;;; Helpers

(defmacro agent-tools-test-with-fake-hc (script &rest body)
  "Run BODY with `hc-program' bound to a temporary shell SCRIPT.
Delete the script afterwards."
  (declare (indent 1))
  `(let ((hc-program (make-temp-file "fake-hc-" nil nil (concat "#!/bin/sh\n" ,script))))
     (unwind-protect
         (progn (set-file-modes hc-program #o755) ,@body)
       (delete-file hc-program))))

(defmacro agent-tools-test-with-repo (files &rest body)
  "Run BODY with $JUMP_CODE_HOME set to a temporary git repository.
The repository holds FILES, a list of (NAME . CONTENT). Skip the test
unless `git' and `fzf' are found. Delete the repository afterwards."
  (declare (indent 1))
  `(progn
     (skip-unless (and (executable-find "git") (executable-find "fzf")))
     (let ((root (file-name-as-directory (make-temp-file "agent-tools-repo-" t))))
       (unwind-protect
           (progn
             (dolist (file ,files)
               (let ((name (expand-file-name (car file) root)))
                 (make-directory (file-name-directory name) t)
                 (write-region (cdr file) nil name nil 'silent)))
             (let ((default-directory root))
               (call-process "git" nil nil nil "init" "--quiet")
               (call-process "git" nil nil nil "add" "."))
             (with-environment-variables (("JUMP_CODE_HOME" root))
               ,@body))
         (delete-directory root t)))))

(defun agent-tools-test-wait (process)
  "Wait until PROCESS's sentinel has finished: its buffer is killed.
Fail the test after 10 seconds."
  (with-timeout (10 (ert-fail "hc did not finish"))
    (while (buffer-live-p (process-buffer process))
      (accept-process-output nil 0.05))))

;;;; jump-to-code-form-at-point

(defun agent-tools-test-form-at (text)
  "Return `jump-to-code-form-at-point' in emacs-lisp-mode TEXT at its \"|\"."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert text)
    (goto-char (point-min))
    (search-forward "|")
    (delete-char -1)
    (jump-to-code-form-at-point)))

(ert-deftest jump-to-code-form-at-point/on-open-paren ()
  (should (equal (agent-tools-test-form-at "x |(jump-to-code \"a.ts\" 3) y")
                 '("a.ts" 3))))

(ert-deftest jump-to-code-form-at-point/after-close-paren ()
  (should (equal (agent-tools-test-form-at "x (jump-to-code \"a.ts\" 3)| y")
                 '("a.ts" 3))))

(ert-deftest jump-to-code-form-at-point/in-string-argument ()
  (should (equal (agent-tools-test-form-at "(jump-to-code \"a.|ts\" \"anchor\")")
                 '("a.ts" "anchor"))))

(ert-deftest jump-to-code-form-at-point/other-form-is-nil ()
  (should-not (agent-tools-test-form-at "(message \"a.|ts\")"))
  (should-not (agent-tools-test-form-at "plain| text")))

(ert-deftest jump-to-code-form-at-point/non-string-argument-is-nil ()
  (should-not (agent-tools-test-form-at "(jump-to-code| name)")))

;;;; jump-to-code-file and jump-to-code

(ert-deftest jump-to-code-file/first-fzf-match ()
  (agent-tools-test-with-repo '(("src/ClientQueueClock.ts" . "") ("doc/notes.md" . ""))
    (let ((root (getenv "JUMP_CODE_HOME")))
      (should (equal (jump-to-code-file "ClientQueueClock" root)
                     (expand-file-name "src/ClientQueueClock.ts" root))))))

(ert-deftest jump-to-code-file/no-match-is-user-error ()
  (agent-tools-test-with-repo '(("a.ts" . ""))
    (should-error (jump-to-code-file "zzz" (getenv "JUMP_CODE_HOME"))
                  :type 'user-error)))

(ert-deftest jump-to-code-file/git-failure-is-user-error-not-a-file ()
  (skip-unless (and (executable-find "git") (executable-find "fzf")))
  (let ((root (file-name-as-directory (make-temp-file "agent-tools-no-repo-" t))))
    (unwind-protect
        (should-error (jump-to-code-file "a" root) :type 'user-error)
      (delete-directory root t))))

(defmacro agent-tools-test-jumping (&rest body)
  "Run BODY in a temporary buffer; kill the buffers it visits."
  `(let ((buffers (buffer-list)))
     (unwind-protect
         (with-temp-buffer
           (save-window-excursion ,@body))
       (dolist (buffer (buffer-list))
         (unless (memq buffer buffers) (kill-buffer buffer))))))

(defconst agent-tools-test-files
  '(("src/a.ts" . "one\ntwo\nthree\n")))

(ert-deftest jump-to-code/opens-file-at-line-anchor ()
  (agent-tools-test-with-repo agent-tools-test-files
    (agent-tools-test-jumping
     (jump-to-code "a.ts" 2)
     (should (equal (file-name-nondirectory buffer-file-name) "a.ts"))
     (should (= (line-number-at-pos) 2)))))

(ert-deftest jump-to-code/opens-file-at-search-anchor ()
  (agent-tools-test-with-repo agent-tools-test-files
    (agent-tools-test-jumping
     (jump-to-code "a.ts" "three")
     (should (= (line-number-at-pos) 3)))))

(ert-deftest jump-to-code/other-window ()
  (agent-tools-test-with-repo agent-tools-test-files
    (agent-tools-test-jumping
     (let ((window (selected-window)))
       (jump-to-code "a.ts" nil t)
       (should-not (eq (selected-window) window))
       (should (equal (file-name-nondirectory buffer-file-name) "a.ts"))))))

(ert-deftest jump-to-code/pushes-mark ()
  (agent-tools-test-with-repo agent-tools-test-files
    (agent-tools-test-jumping
     (let ((from (current-buffer)))
       (jump-to-code "a.ts")
       (should (eq (marker-buffer (car global-mark-ring)) from))))))

(ert-deftest jump-to-code/no-form-at-point-is-user-error ()
  (with-temp-buffer
    (insert "plain text")
    (should-error (advice-eval-interactive-spec (cadr (interactive-form 'jump-to-code)))
                  :type 'user-error)))

(ert-deftest jump-to-code/unset-home-is-user-error ()
  (with-environment-variables (("JUMP_CODE_HOME" nil))
    (should-error (jump-to-code "a.ts") :type 'user-error)))

;;;; herdr-claude-terminals and read-pseudocode-terminal

(defconst agent-tools-test-hc-ls
  "echo '{\"terminals\":[
 {\"address\":\"t2\",\"agent\":\"claude-code\",\"state\":\"live\",\"where\":\"eca / Author\"},
 {\"address\":\"t3\",\"agent\":\"zsh\",\"state\":\"live\",\"where\":\"eca / Shell\"},
 {\"address\":\"t4\",\"agent\":\"claude-code\",\"state\":\"finished\",\"where\":\"eca / Old\"},
 {\"address\":\"t5\",\"agent\":null,\"state\":\"live\",\"where\":null},
 {\"address\":\"t6\",\"agent\":\"claude-code\",\"state\":\"live\",\"where\":\"eca / Planner\"}]}'"
  "A fake `hc' that lists terminals.")

(defun agent-tools-test-read-terminal (input)
  "Call `read-pseudocode-terminal', answering INPUT.
Return the result and the arguments of `completing-read'."
  (let (args)
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest rest)
                 (setq args rest)
                 (if (equal input "") (or (nth 6 rest) "") input))))
      (list (read-pseudocode-terminal) args))))

(ert-deftest herdr-claude-terminals/live-claude-terminals-only ()
  (agent-tools-test-with-fake-hc agent-tools-test-hc-ls
    (should (equal (herdr-claude-terminals)
                   '(("t2" . "eca / Author") ("t6" . "eca / Planner"))))))

(ert-deftest herdr-claude-terminals/hc-failure-is-user-error ()
  (agent-tools-test-with-fake-hc "echo 'herdr is not running' >&2; exit 1"
    (should-error (herdr-claude-terminals) :type 'user-error)))

(ert-deftest read-pseudocode-terminal/default-is-a-candidate ()
  (agent-tools-test-with-fake-hc agent-tools-test-hc-ls
    (let* ((pseudocode-claude-terminal "t6")
           (args (cadr (agent-tools-test-read-terminal ""))))
      (should (equal (nth 6 args) "t6 eca / Planner"))
      (should (member (nth 6 args) (nth 1 args))))))

(ert-deftest read-pseudocode-terminal/tab-name-finds-terminal ()
  (agent-tools-test-with-fake-hc agent-tools-test-hc-ls
    (let ((candidates (nth 1 (cadr (agent-tools-test-read-terminal "t2")))))
      (should (equal (completion-substring-all-completions "Planner" candidates nil 7)
                     '("t6 eca / Planner" . 0))))))

(ert-deftest read-pseudocode-terminal/empty-input-without-default-is-user-error ()
  (agent-tools-test-with-fake-hc agent-tools-test-hc-ls
    (let ((pseudocode-claude-terminal nil))
      (should-error (agent-tools-test-read-terminal "") :type 'user-error))))

(ert-deftest read-pseudocode-terminal/returns-bare-address ()
  (agent-tools-test-with-fake-hc agent-tools-test-hc-ls
    (let ((pseudocode-claude-terminal "t6"))
      (should (equal (car (agent-tools-test-read-terminal "t2 eca / Author")) "t2"))
      (should (equal pseudocode-claude-terminal "t2")))))

;;;; replace-with-pseudocode

(defconst agent-tools-test-hc-send
  "out=$(printf '%s' \"$6\" | sed 's/.* to \\(.*\\)\\.$/\\1/')
printf '%s' \"$6\" > \"${REQUEST_FILE:-/dev/null}\"
printf 'pseudo\\n' > \"$out\""
  "A fake `hc send' that writes \"pseudo\" to the output file.
It saves the request text in $REQUEST_FILE when that is set.")

(defmacro agent-tools-test-with-request-file (&rest body)
  "Run BODY with $REQUEST_FILE set to a temporary file, bound to `request-file'."
  `(let ((request-file (make-temp-file "pseudocode-request-")))
     (unwind-protect
         (with-environment-variables (("REQUEST_FILE" request-file))
           ,@body)
       (delete-file request-file))))

(defun agent-tools-test-pseudocode (text beg end)
  "Run `replace-with-pseudocode' on lines BEG to END of a buffer with TEXT.
BEG and END are line numbers. Return the buffer's text when done."
  (with-temp-buffer
    (insert text)
    (let ((process (replace-with-pseudocode
                    (progn (goto-char (point-min)) (line-beginning-position beg))
                    (progn (goto-char (point-min)) (line-beginning-position end))
                    "t6")))
      (agent-tools-test-wait process)
      (buffer-string))))

(ert-deftest replace-with-pseudocode/replaces-full-lines ()
  (agent-tools-test-with-fake-hc agent-tools-test-hc-send
    (should (equal (agent-tools-test-pseudocode "a\nb\nc\nd\n" 2 4)
                   "a\npseudo\nd\n"))))

(ert-deftest pseudocode-make-request/expands-to-full-lines ()
  (with-temp-buffer
    (insert "one\ntwo\nthree\n")
    (let ((request (pseudocode-make-request 6 10 "t6")))
      (should (= (pseudocode-request-start request) 5))
      (should (= (pseudocode-request-stop request) 15))
      (should (equal (pseudocode-request-newline request) "\n")))))

(ert-deftest pseudocode-make-request/line-numbers-ignore-narrowing ()
  (with-temp-buffer
    (insert "1\n2\n3\n4\n5\n")
    (narrow-to-region 5 (point-max))
    (let ((request (pseudocode-make-request 7 9 "t6")))
      (should (= (pseudocode-request-first-line request) 4))
      (should (= (pseudocode-request-last-line request) 4)))))

(ert-deftest pseudocode-make-request/last-line-without-newline ()
  (with-temp-buffer
    (insert "a\nb")
    (should (equal (pseudocode-request-newline (pseudocode-make-request 3 4 "t6")) ""))))

(ert-deftest pseudocode-prompt/names-files-and-lines ()
  (with-temp-buffer
    (insert "a\nb\n")
    (setq buffer-file-name "/tmp/x.el")
    (let ((request (pseudocode-make-request 3 5 "t6")))
      (setf (pseudocode-request-input request) "/tmp/in.el"
            (pseudocode-request-output request) "/tmp/out.txt")
      (should (equal (pseudocode-prompt request)
                     "/pseudocode the code in /tmp/in.el, lines 2-2 of /tmp/x.el. Write only the pseudocode to /tmp/out.txt.")))
    (set-buffer-modified-p nil)
    (setq buffer-file-name nil)))

(ert-deftest pseudocode-prompt/no-file-name-for-non-file-buffer ()
  (with-temp-buffer
    (insert "a\n")
    (let ((request (pseudocode-make-request 1 2 "t6")))
      (setf (pseudocode-request-input request) "/tmp/in.txt"
            (pseudocode-request-output request) "/tmp/out.txt")
      (should (equal (pseudocode-prompt request)
                     "/pseudocode the code in /tmp/in.txt. Write only the pseudocode to /tmp/out.txt.")))))

(ert-deftest replace-with-pseudocode/read-only-while-waiting ()
  (agent-tools-test-with-fake-hc (concat "sleep 0.3\n" agent-tools-test-hc-send)
    (with-temp-buffer
      (insert "a\n")
      (let ((process (replace-with-pseudocode 1 2 "t6")))
        (should buffer-read-only)
        (agent-tools-test-wait process)
        (should-not buffer-read-only)))))

(ert-deftest replace-with-pseudocode/read-only-buffer-is-refused ()
  (with-temp-buffer
    (insert "a\n")
    (setq buffer-read-only t)
    (should-error (replace-with-pseudocode 1 2 "t6") :type 'buffer-read-only)))

(ert-deftest replace-with-pseudocode/hc-failure-keeps-lines-and-reports ()
  (agent-tools-test-with-fake-hc "echo 'unknown terminal t6'; exit 2"
    (ert-with-message-capture messages
      (should (equal (agent-tools-test-pseudocode "a\nb\n" 1 2) "a\nb\n"))
      (should (string-match-p "No pseudocode .* hc: unknown terminal t6" messages)))))

(ert-deftest replace-with-pseudocode/deletes-temp-files-and-process-buffer ()
  (agent-tools-test-with-fake-hc agent-tools-test-hc-send
    (agent-tools-test-with-request-file
     (agent-tools-test-pseudocode "a\n" 1 2)
     (let ((request (with-temp-buffer (insert-file-contents request-file) (buffer-string))))
       (should (string-match "code in \\(.*\\)\\. Write .* to \\(.*\\)\\.$" request))
       (should-not (file-exists-p (match-string 1 request)))
       (should-not (file-exists-p (match-string 2 request)))))))

;;; agent-tools-test.el ends here
