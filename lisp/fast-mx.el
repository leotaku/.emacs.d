;; -*- lexical-binding: t; -*-

(require 'savehist)
(add-to-list 'savehist-additional-variables 'naive--known-commands)

(defvar naive--command-set (make-hash-table))
(defvar naive--known-commands nil)
(defvar naive--obarray-size 0)

(defun naive--rehash ()
  (mapatoms
   (lambda (it)
     (when (and (commandp it t)
                (not (gethash it naive--command-set))
                (not (seq-contains-p naive--known-commands it)))
       (puthash it (symbol-name it) naive--command-set))))
  (setq naive--known-commands (seq-filter 'commandp naive--known-commands))
  (setq naive--obarray-size (naive--obarray-size obarray)))

(defun naive--obarray-size (obarray)
  (string-to-number (substring (prin1-to-string obarray) 12 -1)))

(defun naive-M-x-action (command-name)
  (let ((command (intern command-name)))
    (setq naive--known-commands (cons command (delete command naive--known-commands)))
    (remhash command naive--command-set)
    (setq prefix-arg current-prefix-arg)
    (setq this-command command)
    (setq real-this-command command)
    (command-execute command t)))

(defun naive-M-x ()
  (interactive)
  (setq this-command last-command)
  (setq real-this-command real-last-command)
  (when (/= naive--obarray-size (naive--obarray-size obarray))
    (naive--rehash))
  (ivy-read
   "M-x " (append naive--known-commands (sort (hash-table-values naive--command-set) :key #'length))
   :action #'naive-M-x-action))

(bind-key "M-x" #'naive-M-x)
