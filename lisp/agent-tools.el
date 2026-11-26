;;; agent-tools.el --- Jump to code and ask Claude for pseudocode -*- lexical-binding: t -*-

;;; Commentary:

;; `jump-to-code' opens a file in $JUMP_CODE_HOME by a short name.
;; `replace-with-pseudocode' asks a Claude session in herdr, through
;; `hc send --wait', to replace the region's lines with pseudocode.

;;; Code:

(require 'exec-path-from-shell)
(require 'ol)
(require 'cl-lib)
(require 'eieio)
(exec-path-from-shell-copy-env "JUMP_CODE_HOME")

;;;; Jump to code

(defun jump-to-code-form-at-point ()
  "Return the arguments of the `jump-to-code' form at point, or nil.
Point may be on the form's \"(\", after its \")\", or inside it, in a
string argument too. Return nil unless the first argument is a string."
  (save-excursion
    (when (eq (char-before) ?\)) (backward-sexp))
    (when (eq (char-after) ?\() (forward-char))
    (let ((form (ignore-errors
                  (backward-up-list 1 t t)
                  ;; From inside a string, the first step only leaves the string.
                  (when (eq (char-after) ?\") (backward-up-list 1 t t))
                  (and (looking-at-p "(jump-to-code\\_>")
                       (read (current-buffer))))))
      (and (stringp (nth 1 form)) (cdr form)))))

(defun jump-to-code-file (name root)
  "Return the absolute name of the first file in ROOT that NAME matches.
The match is the first line of `fzf --filter NAME' over `git ls-files'
in ROOT. Signal `user-error' when the command fails or nothing matches.
Keep the command's stderr out of the result."
  (let ((default-directory root))
    (with-temp-buffer
      (unless (eq 0 (call-process "git" nil '(t nil) nil "ls-files"))
        (user-error "`git ls-files' failed in %s" root))
      (unless (eq 0 (call-process-region nil nil "fzf" t '(t nil) nil "--filter" name))
        (user-error "No file in %s matches %s" root name))
      (goto-char (point-min))
      (expand-file-name (buffer-substring-no-properties (point) (line-end-position))
                        root))))

(defun jump-to-code (name &optional anchor other-window)
  "Open the file in $JUMP_CODE_HOME that NAME matches, at ANCHOR.
NAME is short, such as \"ClientQueueClock.ts\" or \"m/ClientQueue\".
The file is the first match of `fzf --filter NAME' over `git ls-files'.
ANCHOR is a line number or a search string, as after \"::\" in an Org
file link. Open the file in this window, or in the other window when
OTHER-WINDOW is non-nil. Push the mark first, so `pop-global-mark'
returns here. Pulse the line.

Interactively, use the `jump-to-code' form at point, or signal
`user-error' when there is none. A prefix argument opens the other
window."
  (interactive (let ((args (or (jump-to-code-form-at-point)
                               (user-error "No `jump-to-code' form at point"))))
                 (list (nth 0 args) (nth 1 args) current-prefix-arg)))
  (let* ((root (file-name-as-directory
                (or (getenv "JUMP_CODE_HOME") (user-error "JUMP_CODE_HOME is not set"))))
         (file (jump-to-code-file name root))
         (org-link-frame-setup
          (cons (cons 'file (if other-window #'find-file-other-window #'find-file))
                org-link-frame-setup)))
    (push-mark nil t)
    (org-link-open-from-string
     (org-link-make-string
      (concat "file+emacs:" file (if anchor (format "::%s" anchor) ""))))
    (recenter)
    (pulse-momentary-highlight-one-line (point))))

;;;; Claude terminals in herdr

(defvar hc-program "hc"
  "The herdr `hc' command.")

(defvar pseudocode-claude-terminal nil
  "The herdr terminal of the Claude session that writes pseudocode.
Nil until `read-pseudocode-terminal' saves a choice.")

(defvar pseudocode-timeout 1200
  "Seconds `hc send --wait' waits for the Claude session's turn to end.")

(defun herdr-claude-terminals ()
  "Return the live Claude terminals in herdr, as (ADDRESS . WHERE) pairs.
ADDRESS is such as \"t6\", WHERE such as \"project / Tab\"."
  (with-temp-buffer
    (unless (eq 0 (call-process hc-program nil '(t nil) nil "ls" "--json"))
      (user-error "`hc ls' failed"))
    (goto-char (point-min))
    (let ((terminals (alist-get 'terminals (json-parse-buffer :object-type 'alist
                                                              :array-type 'list
                                                              :null-object nil))))
      (seq-keep (lambda (terminal)
                  (let-alist terminal
                    (when (and (equal .state "live") (string-match-p "claude" (or .agent "")))
                      (cons .address .where))))
                terminals))))

(defun read-pseudocode-terminal ()
  "Ask for a Claude terminal, with `pseudocode-claude-terminal' as the default.
Each candidate is an address and its herdr tab, such as
\"t6 dotfiles / Planner\", so typing a tab name finds its terminal.
Save the address in `pseudocode-claude-terminal' and return it. Signal
`user-error' on empty input when there is no default."
  (let* ((candidates (mapcar (lambda (terminal) (format "%s %s" (car terminal) (cdr terminal)))
                             (herdr-claude-terminals)))
         (default (and pseudocode-claude-terminal
                       (or (seq-find (lambda (candidate)
                                       (string-prefix-p (concat pseudocode-claude-terminal " ")
                                                        candidate))
                                     candidates)
                           pseudocode-claude-terminal)))
         (choice (completing-read (format-prompt "Claude terminal" default)
                                  candidates nil nil nil nil default)))
    (when (string-blank-p choice) (user-error "No terminal"))
    (setq pseudocode-claude-terminal (car (split-string choice)))))

;;;; Replace with pseudocode

(cl-defstruct pseudocode-request
  "One run of `replace-with-pseudocode'."
  buffer          ; the buffer whose lines are replaced
  start stop      ; markers around the lines
  first-line last-line
  newline         ; "\n" when the lines end with a newline, else ""
  terminal        ; the herdr terminal, such as "t6"
  input output)   ; the temporary files

(defun pseudocode-make-request (beg end terminal)
  "Return a `pseudocode-request' for the lines from BEG to END, for TERMINAL.
Expand BEG and END to full lines and count lines from the buffer's
start, even when narrowed. Leave the temporary files unset."
  (let ((beg (save-excursion (goto-char beg) (line-beginning-position)))
        (end (save-excursion (goto-char end) (if (bolp) end (line-beginning-position 2)))))
    (make-pseudocode-request
     :buffer (current-buffer)
     :start (copy-marker beg)
     :stop (copy-marker end t)
     :first-line (line-number-at-pos beg t)
     :last-line (line-number-at-pos (max beg (1- end)) t)
     :newline (if (eq (char-before end) ?\n) "\n" "")
     :terminal terminal)))

(defun pseudocode-prompt (request)
  "Return the `/pseudocode' text that `hc send' types for REQUEST.
It names the input file, the buffer's file name and line range when
the buffer visits a file, and the output file."
  (with-slots (buffer first-line last-line input output)
      request
    (format "/pseudocode the code in %s%s. Write only the pseudocode to %s."
            input
            (if (buffer-file-name buffer)
                (format ", lines %d-%d of %s" first-line last-line
                        (abbreviate-file-name (buffer-file-name buffer)))
              "")
            output)))

(defun pseudocode-finish (request process)
  "Finish REQUEST after `hc send --wait' PROCESS exits.
Make REQUEST's buffer writable again. When PROCESS succeeded and the
output file has text, replace the lines with it, as one undo step, and
delete the output file; otherwise keep it and name it in the message,
with `hc''s output. Delete the input file, PROCESS's buffer and the
markers."
  (with-slots (buffer start stop first-line last-line newline terminal input output)
      request
    (when (buffer-live-p buffer)
      (with-current-buffer buffer (setq buffer-read-only nil)))
    (let ((text (with-temp-buffer
                  (insert-file-contents output)
                  (string-trim-right (buffer-string)))))
      (if (or (/= 0 (process-exit-status process))
              (string-empty-p text)
              (not (buffer-live-p buffer)))
          (message "No pseudocode from %s for lines %d-%d; it goes to %s. hc: %s"
                   terminal first-line last-line output
                   (with-current-buffer (process-buffer process)
                     (string-trim (buffer-string))))
        (with-current-buffer buffer
          (undo-boundary)
          (save-excursion
            (goto-char start)
            (delete-region start stop)
            (insert text newline)))
        (delete-file output)
        (message "Pseudocode from %s replaced lines %d-%d" terminal first-line last-line)))
    (kill-buffer (process-buffer process))
    (delete-file input)
    (set-marker start nil)
    (set-marker stop nil)))

(defun replace-with-pseudocode (beg end terminal)
  "Replace the lines from BEG to END with Claude's pseudocode for them.
Expand the region to full lines. Write the lines to a temporary file and
send the Claude session in TERMINAL a request with `hc send --wait': the
file, the buffer's file name and line range, and an output file. Make
the buffer read-only until the turn ends, then replace the lines with
the output file's text. This runs asynchronously. Undo or revert to get
the code back. Return the `hc' process.

Signal an error when the buffer is read-only, so a second run on the
same buffer waits for the first. Interactively, also signal
`user-error' when there is no active region."
  (interactive (progn
                 (barf-if-buffer-read-only)
                 (unless (use-region-p) (user-error "No region"))
                 (list (region-beginning) (region-end) (read-pseudocode-terminal))))
  (barf-if-buffer-read-only)
  (let ((request (pseudocode-make-request beg end terminal))
        (extension (and buffer-file-name (file-name-extension buffer-file-name))))
    (with-slots (start stop first-line last-line input output)
        request
      (setf input (make-temp-file "pseudocode-in-" nil
                                  (if extension (concat "." extension) ".txt")
                                  (buffer-substring-no-properties start stop))
            output (make-temp-file "pseudocode-out-" nil ".txt"))
      (prog1 (make-process
              :name "pseudocode"
              :buffer (generate-new-buffer " *pseudocode*")
              :command (list hc-program "send" "--wait"
                             "--timeout" (number-to-string pseudocode-timeout)
                             terminal (pseudocode-prompt request))
              :sentinel (lambda (process _event)
                          (unless (process-live-p process)
                            (pseudocode-finish request process))))
        (setq buffer-read-only t)
        (deactivate-mark)
        (message "Sent lines %d-%d to %s" first-line last-line terminal)))))

(provide 'agent-tools)
;;; agent-tools.el ends here


(require 'generic-x)
(define-generic-mode allium-mode
  '("--")
  '("entity" "external" "value" "variant" "rule" "when" "requires" "ensures" "let" "for"
    "in" "if" "else" "where" "with" "surface" "facing" "context" "exposes" "provides"
    "contracts" "demands" "fulfils" "related" "timeout" "contract" "config" "default"
    "invariant" "given" "use" "as" "deferred" "open" "question" "transitions" "terminal"
    "actor" "identified_by" "within" "not" "and" "or" "implies" "exists"
    "transitions_to" "becomes" "created" "this" "now")
  '(("@[a-z]+" . font-lock-preprocessor-face)
    ("\\_<[A-Z][A-Za-z0-9]*\\_>" . font-lock-type-face))
  '("\\.allium\\'")
  nil
  "Major mode for Allium specification files.")
