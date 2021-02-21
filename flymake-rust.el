;;; flymake-rust -- Flymake integration for the Rust programming language  -*- lexical-binding: t; -*-

;; Copyright © 2021 Brian Cully <bjc@kublai.com>

;; Author: Brian Cully <bjc@kublai.com>
;; URL: https://github.com/bjc/nspawn-tramp
;; Keywords: flymake, rust
;; Maintainer: Brian Cully <bjc@kublai.com>
;; Version: 0.1
;; Package-Requires: ((emacs "26.1"))

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

(defgroup flymake-rust nil
  "Flymake integration for the Rust programming language."
  :prefix "flymake-rust-"
  :group 'applications
  :link '(url-link :tag "Github" "https://github.com/bjc/flymake-rust")
  :link '(emacs-commentary-link :tag "Commentary" "flymake-rust"))

(defcustom flymake-rust-cargo-path "cargo"
  "Path to cargo executable."
  :type 'string
  :group 'flymake-rust)

(defvar-local flymake-rust--process nil
  "Flymake process associated with the current buffer.")

(defvar-local flymake-rust--parse-marker nil
  "Last parse position in the current buffer.")

(defun flymake-rust--buffer-name (suffix)
  "What to name the flymake buffer for ‘SUFFIX’."
  (format "*flymake-rust-cargo-check %s %s*" suffix buffer-file-name))

(defmacro flymake-rust--with-proc-buf (&rest body)
  "Run ‘BODY’ in the buffer for the current buffer’s Flymake process."
  (declare (indent defun))
  (let ((buf (gensym)))
    `(when-let ((,buf (and flymake-rust--process
                           (process-buffer flymake-rust--process))))
       (with-current-buffer ,buf
         ,@body))))

(defun flymake-rust--extract-error-location (hash)
  "Extract file location data from ‘HASH’.

Returns a sequence of data in the form of (BYTE-START . BYTE-END)
If any data are not available, nil will be used in its place."
  (let* ((spans (or (gethash "spans" hash) [])))
    (mapcar (lambda (span)
              (let ((byte-start (or (gethash "byte_start" span) nil))
                    (byte-end (or (gethash "byte_end" span) nil)))
                `(,byte-start . ,byte-end)))
            spans)))

(defun flymake-rust--level-to-action (level)
  "Convert ‘LEVEL’ into a Flymake action."
  (cond ((string= level "error") :error)
        ((string= level "warning") :warning)
        (t :note)))

(defun flymake-rust--report-errors (source-buffer report-fn hash)
  "Pull interesting things out of ‘HASH’ for ‘SOURCE-BUFFER ’and forward them to ‘REPORT-FN’."
  (let ((reason (gethash "reason" hash))
        (msghash (gethash "message" hash)))
    (when (and (string= reason "compiler-message") msghash)
      (let ((message (gethash "message" msghash))
            (level (gethash "level" msghash))
            (errlocs (flymake-rust--extract-error-location msghash)))
        (funcall report-fn
                 (mapcar (lambda (errloc)
                           (flymake-make-diagnostic source-buffer
                                                    (car errloc) (cdr errloc)
                                                    (flymake-rust--level-to-action level)
                                                    message))
                         errlocs))))))

(defun flymake-rust--parse-json (source-buffer report-fn)
  "Grab new JSON from ‘SOURCE-BUFFER’ and send it to ‘REPORT-FN’.

This function attempts to parse JSON data beginning at
  ‘flymake-rust--parse-marker’ and if successful, will update
  the marker to the end of the parsed data."
  (goto-char flymake-rust--parse-marker)
  (while (condition-case err
             (let ((parsed (json-parse-buffer)))
               (set-marker flymake-rust--parse-marker (point))
               (flymake-rust--report-errors source-buffer report-fn parsed)
               t)
           (json-parse-error nil)
           (json-end-of-file nil)
           (t (error err)))))

;; TODO: this blocks ‘C-g’, so it may be better to just do this in an
;; idle process.
(defun flymake-rust--filter (source-buffer report-fn proc string)
  "Process ‘STRING’ as JSON from ‘PROC’ and send it to ‘REPORT-FN’ for ‘SOURCE-BUFFER’."
  (flymake-rust--with-proc-buf
    (let ((moving (= (point) (process-mark proc))))

      (save-excursion
        ;; Append the latest output to the end of the previous.
        (setq buffer-read-only nil)
        (goto-char (process-mark proc))
        (insert string)
        (set-marker (process-mark proc) (point))
        (setq buffer-read-only t)
        (flymake-rust--parse-json source-buffer report-fn))

      (when moving
        (goto-char (process-mark proc))))))

(defun flymake-rust--make-filter (source-buffer report-fn)
  "Create a process filter for ‘SOURCE-BUFFER’ reporting to ‘REPORT-FN’."
  (lambda (proc string)
    (flymake-rust--filter source-buffer report-fn proc string)))

(defun flymake-rust--cleanup ()
  "Clean up and leftover processes and buffers for the current buffer."
  (when (process-live-p flymake-rust--process)
    (flymake-log :warning "Killing out-of-date checker process.")
    (delete-process flymake-rust--process))
  (mapc (lambda (suffix)
          (when-let ((buf (get-buffer (flymake-rust--buffer-name suffix))))
            (kill-buffer buf)))
        '("stdout" "stderr")))

(defun flymake-rust--setup-proc-buf ()
  "Set up stdout process buffer for scanning."
  ;; Save the position where output will begin so we can know where to
  ;; start scanning from when output is done.
  (flymake-rust--with-proc-buf
    (setq buffer-read-only t)
    (setq flymake-rust--parse-marker (make-marker))
    (set-marker flymake-rust--parse-marker (point))))

(defun flymake-rust--call (source-buffer report-fn)
  "Call out to the syntax checker for ‘SOURCE-BUFFER’ and report to ‘REPORT-FN’."
  (with-current-buffer source-buffer
    (flymake-rust--cleanup)
    (setq flymake-rust--process
          (make-process :name "flymake-rust-cargo-check"
                        :buffer (flymake-rust--buffer-name "stdout")
                        :command `(,flymake-rust-cargo-path "check" "--quiet" "--message-format" "json")
                        :noquery nil
                        :filter (flymake-rust--make-filter source-buffer report-fn)
                        :stderr (flymake-rust--buffer-name "stderr")
                        :stderr nil
                        :file-handler t))
    (flymake-rust--setup-proc-buf)))

(defun flymake-rust--backend (report-fn &rest args)
  "Send Flymake diagnostics to ‘REPORT-FN’.

For the value of ‘ARGS’, see the documentation for
‘flymake-diagnostic-functions’."
  (let ((rc (plist-member args :recent-changes)))
    (when (or (not rc) (cadr rc))
      (flymake-rust--call (current-buffer) report-fn))))

(defun flymake-rust-setup ()
  "Provide Flymake support for the Rust programming language."
  (add-hook 'flymake-diagnostic-functions 'flymake-rust--backend nil t))

(provide 'flymake-rust)
;;; flymake-rust.el ends here
