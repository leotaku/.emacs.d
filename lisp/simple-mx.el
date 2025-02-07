;; -*- lexical-binding: t; -*-

(require 'savehist)
(add-to-list 'savehist-additional-variables 'simple-M-x--known-commands)

(defvar simple-M-x--command-set (make-hash-table))
(defvar simple-M-x--known-commands nil)
(defvar simple-M-x--obarray-size 0)

(defun simple-M-x--rehash ()
  (mapatoms
   (lambda (it)
     (when (and (commandp it t)
                (not (gethash it simple-M-x--command-set))
                (not (seq-contains-p simple-M-x--known-commands it)))
       (puthash it (symbol-name it) simple-M-x--command-set))))
  (setq simple-M-x--known-commands (seq-filter 'commandp simple-M-x--known-commands))
  (setq simple-M-x--obarray-size (simple-M-x--obarray-size obarray)))

(defun simple-M-x--obarray-size (obarray)
  (string-to-number (substring (prin1-to-string obarray) 12 -1)))

(defun simple-M-x-action (command-name)
  (let ((command (intern command-name)))
    (setq simple-M-x--known-commands (cons command (delete command simple-M-x--known-commands)))
    (remhash command simple-M-x--command-set)
    (setq prefix-arg current-prefix-arg)
    (setq this-command command)
    (setq real-this-command command)
    (command-execute command t)))

(defun simple-M-x ()
  (interactive)
  (setq this-command last-command)
  (setq real-this-command real-last-command)
  (when (/= simple-M-x--obarray-size (simple-M-x--obarray-size obarray))
    (simple-M-x--rehash))
  (ivy-read
   "M-x " (append simple-M-x--known-commands (sort (hash-table-values simple-M-x--command-set) :key #'length))
   :action #'simple-M-x-action))

(bind-key [remap execute-extended-command] #'simple-M-x)
