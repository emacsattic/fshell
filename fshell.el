;;; fshell.el --- enhancements to shell.el

;; Copyright (C) 1988, 1993, 1994 Free Software Foundation, Inc.
;; Copyright (C) 1994, 95, 96, 06, 08, 2010 Noah S. Friedman

;; Author: Noah Friedman <friedman@splode.com>
;; Maintainer: friedman@splode.com
;; Keywords: extensions, processes
;; Created: 1994-06-21

;; $Id: fshell.el,v 1.18 2012/08/25 18:17:46 friedman Exp $

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;; Code:

(require 'shell)

;;;###autoload
(defvar fshell-default-make-new-shell nil
  "*If non-`nil', reverse the meaning of prefix arg to \\[fshell]")

;;;###autoload
(defvar fshell-make-shell-hook nil
  "*Forms to run in a new shell buffer just before the process is started.")

;;;###autoload
(defvar fshell-after-start-shell-hook nil
  "*Forms to run in a new shell buffer after the process is started.")

;;;###autoload
(defvar fshell-buffer-name "*shell*"
  "*Buffer name for shell process.")

;;;###autoload
(defvar fshell-reuse-visible-buffers nil
  "*When nil, only switch to existing shell buffers which are not already visible in another window.")

;;;###autoload
(defvar fshell-reuse-visible-buffer-frames nil
  "*Specifies frames to search for windows displaying a candidate shell buffer.
If `visible', search all visible frames.
If `0', search all visible and iconified frames.
If `t', search all frames.
If nil, search only the selected frame.
If set to a frame object, search only that frame.")

;; See comments near fshell-pop-to-buffer for an explanation.
;;;###autoload (add-hook 'same-window-regexps "^\\*shell\\*\\(\\|<[0-9]+>\\)")


;;;###autoload
(defun fshell (&optional prefix)
  "Run an inferior shell, with I/O through buffer *shell*.
The actual name of the buffer can be specified with the variable
`fshell-buffer-name', but for the sake of brevity the default
\(\"*shell*\"\) will be used in the examples below.

If buffer exists but shell process is not running, make new shell.

If buffer exists and shell process is running, just switch to buffer
 named \"*shell*\".

If an explicit numeric prefix argument is given (or this function is called
  from lisp with a numeric argument), switch to the buffer *shell*<prefix>,
  e.g. \"*shell*<2>\".  If there is no process in that buffer, start one.

If invoked with `\\[universal-argument] \\[fshell]', search for an existing shell buffer
  whose current directory is the same as that of the current buffer and
  switch to that one.  If there isn't one already, create a new buffer and
  start a shell process in it.

If invoked with `\\[universal-argument] \\[universal-argument] \\[fshell]', always create a new shell process
  in a new buffer.
This is the same as calling the function from lisp with an argument of `t'.

The previous paragraphs describe the behavior of this function whenever it
is called from lisp.  If it is called interactively and the variable
`fshell-default-make-new-shell' is non-`nil', then the meaning of
non-numeric prefix arguments is reversed, i.e. typing `\\[fshell]' without
a prefix argument creates a new shell, and `\\[universal-argument] \\[fshell]'
would switch to the buffer \"*shell*\".

The variable `fshell-reuse-visible-buffers' controls whether shell buffers
  that are already visible in some other window will be considered.
The variable `fshell-reuse-visible-buffer-frames' controls which frames
  will be considered when scanning to see if a buffer is already visible.

Program used comes from variable `explicit-shell-file-name',
 or (if that is nil) from the ESHELL environment variable,
 or else from SHELL if there is no ESHELL.
If a file `~/.emacs_SHELLNAME' exists, it is given as initial input
 (Note that this may lose due to a timing error if the shell
  discards input when it starts up.)
The buffer is put in Shell mode, giving commands for sending input
and controlling the subjobs of the shell.  See `shell-mode'.
See also the variable `shell-prompt-pattern'.

The shell file name (sans directories) is used to make a symbol name
such as `explicit-sh-args'.  If that symbol is a variable,
its value is used as a list of arguments when invoking the shell.
Otherwise, one argument `-i' is passed to the shell.

Type \\[describe-mode] in the shell buffer for a list of commands."
  (interactive "P")

  (and fshell-default-make-new-shell
       (interactive-p)
       (not (numberp prefix))
       (setq prefix (not prefix)))

  (let (shell-buffer)
    (cond ((null prefix)
           (setq shell-buffer (fshell-make-new-buffer-name)))
          ((numberp prefix)
           (setq shell-buffer (fshell-make-new-buffer-name nil prefix)))
          ((and (consp prefix)
                (= (car prefix) 4)
                (setq shell-buffer (fshell-process-matching-shell-directory))))
          (t
           (setq shell-buffer (fshell-make-new-buffer-name nil t))))

    (if (comint-check-proc shell-buffer)
        (fshell-pop-to-buffer shell-buffer)
      (fshell-pop-to-buffer
       (set-buffer (fshell-start-shell shell-buffer))))))

(defun fshell-buffer-name-p (name)
  (when (bufferp name)
    (setq name (buffer-name name)))
  (save-match-data
    (string-match (concat "^" (regexp-quote fshell-buffer-name)) name)))

(defun fshell-buffer-local-value-pre22 (variable buffer)
  "Return the value of VARIABLE in BUFFER.
If VARIABLE does not have a buffer-local binding in BUFFER, the value
is the default binding of the variable."
  (save-excursion
    (set-buffer buffer)
    (symbol-value variable)))

;; `buffer-local-value' was introduced as a primitive function in Emacs 22.
(defalias 'fshell-buffer-local-value
  (if (fboundp 'buffer-local-value)
      'buffer-local-value
    'fshell-buffer-local-value-pre22))

;; Find a shell buffer whose current directory matches that specified by
;; `dir' (or the same directory as the current buffer).  Only buffers
;; starting with names matching `fshell-buffer-name' are considered.
(defun fshell-process-matching-shell-directory (&optional dir)
  (or dir (setq dir default-directory))
  (let ((re (concat "^" (regexp-quote fshell-buffer-name)))
        (buflist nil)
        (found nil))
    (save-match-data
      ;; Build a list of candidate buffers based on name
      (mapc (lambda (buf)
              (when (string-match re (buffer-name buf))
                (setq buflist (cons buf buflist))))
        ;; scan list in reverse so we build up our matching list in
        ;; buffer-list order; that gives us the most recently used match
        ;; first below, if there is more than one.
        (nreverse (buffer-list))))
    (while (and buflist (not found))
      (when (and (string= dir (fshell-buffer-local-value
                               'default-directory (car buflist)))
                 (or fshell-reuse-visible-buffers
                     (not (get-buffer-window
                           (car buflist)
                           fshell-reuse-visible-buffer-frames))))
        (setq found (car buflist)))
      (setq buflist (cdr buflist)))
    found))


;; Todo: add prefix argument that will kill current process and start a new one.
(defun fshell-restart-shell ()
  "Restart shell process in the current buffer if it is exited."
  (interactive)
  (and (null (get-buffer-process (current-buffer)))
       (memq major-mode '(shell-mode fshell-mode))
       (fshell-start-shell)))

(defun fshell-no-tty (&optional prefix)
  "Like `fshell' \(\\[fshell]\), but never allocate a pseudo-tty for the shell."
  (interactive "P")
  (let ((process-connection-type nil))
    (if (interactive-p)
        (call-interactively 'fshell)
      (fshell prefix))))

;; This can be customized with defadvice or redefined.
(defun fshell-make-new-buffer-name (&optional name number)
  (unless name
    (setq name fshell-buffer-name))
  (cond ((numberp number)
         (format "%s<%d>" name number))
        (number
         (generate-new-buffer-name name))
        (t name)))

(defun fshell-start-shell (&optional buffer)
  (save-excursion
    (let* ((prog (or explicit-shell-file-name
                     (getenv "ESHELL")
                     (getenv "SHELL")
                     "/bin/sh"))
           (name (file-name-nondirectory prog))
           (startfile (concat "~/.emacs_" name))
           (xargs-name (intern-soft (concat "explicit-" name "-args"))))
      (set-buffer (apply 'fshell-make-shell
                         (or buffer (buffer-name))
                         prog
                         (if (file-exists-p startfile) startfile)
                         (if (and xargs-name (boundp xargs-name))
                             (symbol-value xargs-name)
                           '("-i"))))
      (shell-mode)
      (run-hooks 'fshell-after-start-shell-hook)
      (current-buffer))))

;; This is just like comint.el:make-comint, except that it doesn't
;; implicitly put asterisks around the buffer name; it is assumed that's
;; already been done if it's desired.
(defun fshell-make-shell (name program &optional startfile &rest switches)
  "Make a comint process NAME in a buffer, running PROGRAM.
If there is already a running process in that buffer, it is not restarted.
Optional third arg STARTFILE is the name of a file to send the contents of to
the process.  Any more args are arguments to PROGRAM."
  (let ((buffer (get-buffer-create name)))
    ;; If no process, or nuked process, crank up a new one and put buffer in
    ;; comint mode.  Otherwise, leave buffer and existing process alone.
    (cond ((not (comint-check-proc buffer))
	   (save-excursion
	     (set-buffer buffer)
	     (comint-mode)  ; Install local vars, mode, keymap, ...
             (run-hooks 'fshell-make-shell-hook)
             (comint-exec buffer name program startfile switches))))
    buffer))

;; Starting in Emacs 19.29, the variable same-window-regexps modifies how
;; pop-to-buffer works; in particular, if the name of the buffer being
;; switched to matches one of the regexps in same-window-regexps (which may
;; be buffer-local to the current buffer), then pop-to-buffer acts like
;; switch-to-buffer.  This gives users more control.
;; This also explains the related autoload cookie near the top of the file.
(defun fshell-pop-to-buffer (buffer)
  (if (fboundp 'same-window-regexps)
      (pop-to-buffer buffer)
    (switch-to-buffer buffer)))

;; Potentially useful function to put on fshell-make-shell-hook.
;; If you are in a shell buffer and you create a new shell, the new shell
;; inherits the current working directory of the previous buffer, but it
;; may resolve symbolic links to determine where it really is, thus making
;; the names of emacs' default-directory and the shell's internal current
;; working different.  Some shells, e.g. Bash, will not do this if they
;; inhereit a PWD environment variable.
;;
;; Note that this modification of process-environment is very short-lived.
;; Because shell-mode (called later) calls comint-mode, and the latter
;; destroys all local variables, this local process-environment is gone by
;; the time fshell returns.  That's ok, because we only needed it long
;; enough for `start-process' to see it.
(defun fshell-inherit-cwd ()
  (let ((dir (file-name-as-directory default-directory)))
    (cond ((file-accessible-directory-p dir)
           (make-local-variable 'process-environment)
           (setq process-environment
                 (copy-sequence (default-value 'process-environment)))
           ;; Bash can't handle a cwd of `~/foo', only `~'.  But since it
           ;; will abbreviate the home directory portion of an absolute path
           ;; to `~' internally anyway, expand it here.
           (and (> (length dir) 1)
                (string= "~/" (substring dir 0 2))
                (setq dir (expand-file-name dir)))
           (setenv "PWD"
                   (substring dir 0 (1- (length dir))))))))

(defun fshell-input-at-current-prompt-p ()
  "Returns `t' if the prompt for the next command is visible.
While process output is occuring and the shell has not displayed a prompt
for more input, this function should return nil."
  (save-excursion
    (goto-char (process-mark (get-buffer-process (current-buffer))))
    (let ((inhibit-field-text-motion t))
      (beginning-of-line)
      (looking-at comint-prompt-regexp))))

(defun fshell-send-commands (proc &rest commands)
  "Send a sequence of commands to the inferior process as if they were typed interactively.
PROC specifies the shell process or buffer.

The remaining arguments are commands to be sent, one at a time, to the inferior process.
If there is only one remaining argument and it is a list, use that list.

Before subsequent commands are sent, the current command must complete \(as
evidenced by the display of a command prompt in the buffer\).  When the
final command complete, this function returns."
  (let ((orig-buffer (current-buffer)))
    (unwind-protect
        (let ((cmdlist (if (consp (car commands))
                           (car commands)
                         commands))
              cmd pmark)

          (if (bufferp proc)
              (setq proc (get-buffer-process proc)))
          (set-buffer (process-buffer proc))

          (setq pmark (process-mark proc)
                cmd   cmdlist)

          (while (not (fshell-input-at-current-prompt-p))
            (accept-process-output proc .1))

          (while cmd
            (goto-char pmark)
            (insert (car cmd))
            (if (string-lessp "22" emacs-version)
                (comint-send-input nil t)
              (comint-send-input))
            (while (not (fshell-input-at-current-prompt-p))
              (accept-process-output proc .1)
              ;; redisplay
              (sit-for 0))
            (setq cmd (cdr cmd)))

          (goto-char pmark))
      (set-buffer orig-buffer))))

;; Indent like `while'
(put 'fshell-send-commands 'lisp-indent-function 1)

(provide 'fshell)

;;; fshell.el ends here
