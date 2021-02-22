;;; flymake-rust.el -- Flymake integration for the Rust programming language  -*- lexical-binding: t; -*-

;; Copyright © 2021 Brian Cully <bjc@kublai.com>

;; Author: Brian Cully <bjc@kublai.com>
;; URL: https://github.com/bjc/flymake-rust
;; Keywords: tools, flymake, rust
;; Maintainer: Brian Cully <bjc@kublai.com>
;; Version: 0.1
;; Package-Requires: ((emacs "27.1"))

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;
;; Add support for the Rust programming language to Flymake.
;;
;; ## Usage
;;
;; Call ‘flymake-rust-setup’ in your Emacs initialization.
;;
;;     (add-hook ’rust-mode-hook ’flymake-rust-setup)
;;

;;; Code:

(eval-when-compile
  (require 'subr-x))

(defgroup flymake-rust nil
  "Flymake integration for the Rust programming language."
  :prefix "flymake-rust-"
  :group 'applications
  :link '(url-link :tag "Github" "https://github.com/bjc/flymake-rust")
  :link '(emacs-commentary-link :tag "Commentary" "flymake-rust"))

(defcustom flymake-rust-cargo-path "cargo"
  "Path to cargo executable."
  :package-version '(flymake-rust . "0.1")
  :type 'string
  :group 'flymake-rust)

(defcustom flymake-rust-rustc-path "rustc"
  "Path to rustc executable."
  :package-version '(flymake-rust . "0.1")
  :type 'string
  :group 'flymake-rust)

(defcustom flymake-rust-checker 'cargo
  "Which checker to use.

 * cargo (the default) can only check on save, but is suitable
for all projects.

 * rustc can check between saves, but isn’t project aware, and
will complain about things like missing main functions."
  :package-version '(flymake-rust . "0.1")
  :type '(choice (const cargo)
                 (const rustc))
  :options '(cargo rustc)
  :group 'flymake-rust)

(defvar-local flymake-rust--process nil
  "Flymake process associated with the current buffer.")

(defvar flymake-rust--cargo-flags '("check" "--quiet" "--message-format=json")
  "Additional flags to pass to cargo.")

(defvar flymake-rust--rustc-flags '("--error-format=json" "-o" "/dev/null" "-")
  "Additional flags to pass to rustc.")

(defun flymake-rust--buffer-name (suffix)
  "What to name the flymake buffer for ‘SUFFIX’."
  (format "*flymake-rust[%s] %s*" suffix buffer-file-name))

(defun flymake-rust--extract-error-location (hash)
  "Extract file location data from ‘HASH’.

Returns a sequence of data in the form of (FILE-NAME (BYTE-START
. BYTE-END)).  If any datum is not available, nil will be used in
its place."
  (let* ((spans (or (gethash "spans" hash) [])))
    (mapcar (lambda (span)
              (let ((file-name (or (gethash "file_name" span) nil))
                    (byte-start (or (1+ (gethash "byte_start" span)) nil))
                    (byte-end (or (1+ (gethash "byte_end" span)) nil)))
                `(,file-name (,byte-start . ,byte-end))))
            spans)))

(defun flymake-rust--level-to-action (level)
  "Convert ‘LEVEL’ into a Flymake action."
  (pcase level
    ("error"   :error)
    ("warning" :warning)
    (_         :note)))

(defun flymake-rust--extract-msghash (hash)
  "Return the message hash from ‘HASH’.

Cargo and rustc have slightly different formats for this, which
this function attempts to account for."
  (pcase flymake-rust-checker
    ('cargo (when (string= (gethash "reason" hash) "compiler-message")
              (gethash "message" hash)))
    ('rustc hash)))

(defun flymake-rust--normalize-path (hash file-name)
  "Return full path to ‘FILE-NAME’.

Cargo only puts the relative path in there, so we need to add the
path to the workspace from ‘HASH’ to get the full path."
  (pcase flymake-rust-checker
    ('cargo (thread-last hash
              (gethash "target")
              (gethash "src_path")))
    ('rustc file-name)))

;; See https://doc.rust-lang.org/rustc/json.html for a description of
;; the JSON format.
(defun flymake-rust--make-diagnostics (source-buffer hash)
  "Extract diagnostic messages for Flymake from ‘HASH’.

‘HASH’ is the hash-table representation of the JSON output by the
checker for ‘SOURCE-BUFFER’"
  (when-let ((msghash (flymake-rust--extract-msghash hash)))
    (let ((message (gethash "message" msghash))
          (level (gethash "level" msghash))
          (errlocs (flymake-rust--extract-error-location msghash)))
      (mapcar (lambda (errloc)
                (pcase-let ((`(,file-name (,start . ,end)) errloc))
                  ;; Filter out errors that don’t relate to
                  ;; ‘source-buffer’.
                  (when (or (string= file-name "<anon>")
                            (string= (flymake-rust--normalize-path hash file-name)
                                     (file-local-name (buffer-file-name source-buffer))))
                    (flymake-make-diagnostic source-buffer
                                             start end
                                             (flymake-rust--level-to-action level)
                                             message))))
              errlocs))))

(defun flymake-rust--parse-buffer (proc-buffer source-buffer report-fn)
  "Parse ‘PROC-BUFFER’ as JSON compiler output for ‘REPORT-FN’.

‘PROC-BUFFER’ is the process buffer associated with the checker
process for ‘SOURCE-BUFFER’, and is expected to contain JSON
output from the Rust compiler."
  (with-current-buffer proc-buffer
    (let ((continue t)
          (diags nil))
      (goto-char (point-min))
      (while continue
        (if-let ((diag (condition-case err
                           (json-parse-buffer)
                         (json-end-of-file nil)
                         (t (error err)))))
            (push (flymake-rust--make-diagnostics source-buffer diag) diags)
          (setq continue nil)))

      (let ((diags (flatten-list diags)))
        (if diags
            (funcall report-fn (flatten-list diags))
          (funcall report-fn nil :explanation "no errors"))))))

;; TODO: verify that idle timers don’t actually break anything, and
;; are, in fact, an improvement over just calling stuff in the
;; sentinel directly.
(defun flymake-rust--sentinel (source-buffer report-fn proc _event)
  "Handle events for ‘PROC‘.

The only event currently being handled is the process finishing,
at which point a short-delay idle timer is set up to handle the
processing of compiler output for ‘SOURCE-BUFFER’, which will be
reported to ‘REPORT-FN’."
  (when (eq 'exit (process-status proc))
    (process-put proc 'timer
                 (run-with-idle-timer 0.1 nil
                                      'flymake-rust--parse-buffer
                                      (process-buffer flymake-rust--process) source-buffer report-fn))))

(defun flymake-rust--make-sentinel (source-buffer report-fn)
  "Create a sentinel for ‘SOURCE-BUFFER’ reporting to ‘REPORT-FN’."
  (lambda (proc event)
    (flymake-rust--sentinel source-buffer report-fn proc event)))

(defun flymake-rust--cleanup ()
  "Clean up processes and buffers associated with the current buffer."
  (when-let ((timer (and flymake-rust--process
                         (process-get flymake-rust--process 'timer))))
    (cancel-timer timer))

  (when (process-live-p flymake-rust--process)
    (flymake-log :warning "Killing out-of-date checker process.")
    (delete-process flymake-rust--process))

  (dolist (suffix '("stdout" "stderr"))
    (when-let ((buf (get-buffer (flymake-rust--buffer-name suffix))))
      (kill-buffer buf))))

(defun flymake-rust--cargo-command ()
  "Return a command line for cargo check."
  (cons (executable-find flymake-rust-cargo-path t)
        flymake-rust--cargo-flags))

(defun flymake-rust--rustc-command ()
  "Return a command line for rustc reading from standard input."
  (cons (executable-find flymake-rust-rustc-path t) flymake-rust--rustc-flags))

(defun flymake-rust--ignore-stderr-p ()
  "Return t if the stderr output of ‘flymake-rust-checker’ should be ignored.

Cargo puts non-JSON data on stderr as it runs, so it has to be
shunted elsewhere or it breaks parsing."
  (eq flymake-rust-checker 'cargo))

(defun flymake-rust--checker-command ()
  "Return a command line to check ‘PATH’.

This uses the value of ‘flymake-rust-checker’ to determine the
specific command line."
  (pcase flymake-rust-checker
    ('cargo (flymake-rust--cargo-command))
    ('rustc (flymake-rust--rustc-command))))

(defun flymake-rust--call (source-buffer report-fn)
  "Check ‘SOURCE-BUFFER’ for errors and report them to ‘REPORT-FN’.

‘REPORT-FN’ is a function normally created by Flymake which
expects a list of diagnostics created by
‘flymake-make-diagnostic’.  For further information, see the Info
node ‘(flymake)Backend functions’."
  (with-current-buffer source-buffer
    (flymake-rust--cleanup)

    (let ((buf (get-buffer-create (flymake-rust--buffer-name "stdout"))))
      (with-current-buffer buf
        (js-mode))

      (setq flymake-rust--process
            (make-process :name "flymake-rust"
                          :buffer buf
                          :command (flymake-rust--checker-command)
                          :connection-type 'pipe
                          :noquery nil
                          :sentinel (flymake-rust--make-sentinel source-buffer report-fn)
                          :stderr (and (flymake-rust--ignore-stderr-p)
                                       (flymake-rust--buffer-name "stderr"))
                          :file-handler t)))

    (when (eq flymake-rust-checker 'rustc)
      (process-send-region flymake-rust--process
                           (point-min) (point-max))
      (process-send-eof flymake-rust--process))))

(defun flymake-rust--backend (report-fn &rest args)
  "Send Flymake diagnostics to ‘REPORT-FN’.

For the value of ‘ARGS’, see the documentation for
‘flymake-diagnostic-functions’."
  (let ((rc (plist-member args :recent-changes)))
    (when (or (not rc) (cadr rc))
      (flymake-rust--call (current-buffer) report-fn))))

;;;###autoload
(defun flymake-rust-setup ()
  "Provide Flymake support for the Rust programming language."
  (add-hook 'flymake-diagnostic-functions 'flymake-rust--backend nil t))

(provide 'flymake-rust)
;;; flymake-rust.el ends here
