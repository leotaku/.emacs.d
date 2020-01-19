;;; keytheme.el --- custom keybinding theme -*- lexical-binding: t; -*-

;;; Code:

(bk-block modal-editing
  :requires .theist-mode .viper-cmd .modalka expand-region .multiple-cursors
  :custom
  (modalka-cursor-type . 'box)
  (cursor-type . 'bar)
  :config
  (leaf-key "<f7>" 'modalka-mode)
  (leaf-key "<escape>" 'modalka-mode)
  (advice-add 'modalka-mode :around 'fi-call-silent)
  (add-to-list
   'emulation-mode-map-alists
   `((modalka-mode . ,modalka-mode-map)))
  :config
  (modalka-reserve (identity capitalize)
    "q" "w" "e" "r" "t" "z" "u" "i" "o" "p" "ü"
    "a" "s" "d" "f" "g" "h" "j" "k" "l" "ö" "ä"
    "y" "x" "c" "v" "b" "n" "m")
  (modalka-keys
   ("-" . negative-argument))
  (modalka-multiplex 'digit-argument (identity)
    "0" "1" "2" "3" "4" "5" "6" "7" "8" "9")
  (modalka-keys
   ("i" . modalka-mode)
   ("a" . ((when (/= (point) (point-at-eol))
             (forward-char))
           (modalka-mode -1)))
   ("I" . ((back-to-indentation) (modalka-mode -1)))
   ("A" . ((end-of-line) (modalka-mode -1)))
   ("h" . backward-char)
   ("j" . next-line)
   ("k" . previous-line)
   ("l" . forward-char)
   ("w" . viper-forward-word)
   ("W" . viper-forward-Word)
   ("b" . viper-backward-word)
   ("B" . viper-backward-Word)
   ("e" . viper-end-of-word)
   ("E" . viper-end-of-Word))
  (modalka-keys
   ("c" . ((kill-region-or-line arg) (modalka-mode -1)))
   ("d" . kill-region-or-line)
   ("y" . copy-region-or-line))
  (modalka-keys
   ("<f7>" . modalka-mode)
   ("SPC" . mc/mark-down-or-more)
   ("<backspace>" . ((if (region-active-p)
                         (delete-region
                          (region-beginning)
                          (region-end))
                       (delete-char 1))))
   ("g" . goto-or-quit)
   ("x" . theist-C-x)
   ("z" . theist-C-c)
   ("m" . er/expand-region)
   ("n" . er/contract-region)
   ("s" . avy-goto-word-or-subword-1))
  (modalka-keys
   ("v" . switch-mark-command)
   ("r" . viper-replace-char)
   ("o" . exchange-point-and-mark)
   ("U" . fi-undo-only-global)
   ("u" . fi-undo-global)
   ("p" . viper-put-back)
   ("P" . viper-Put-back)
   (";" . comment-or-uncomment-region)
   ("f" . ((let ((beg (mark)))
             (call-interactively 'viper-find-char-forward)
             (when (region-active-p)
               (set-mark beg)))))
   ("t" . ((let ((beg (mark)))
             (call-interactively 'viper-goto-char-forward)
             (when (region-active-p)
               (set-mark beg)))))
   ("," . ((let ((beg (mark))
                 (active (region-active-p)))
             (call-interactively 'viper-repeat-find)
             (when active
               (set-mark beg)
               (transient-mark-mode)))))))

(bk-block multiple-cursors
  :requires .multiple-cursors
  :bind ((:mc/keymap
          :package multiple-cursors
          ("<return>" . nil))))

(bk-block expand-region
  :requires .mode-local .expand-region
  :bind (("M-m" . er/expand-region)
         ("M-n" . er/contract-region))
  :config
  (setq-mode-local
   org-mode
   er/try-expand-list
   '(er/mark-line
     er/mark-inside-pairs
     er/mark-inside-quotes
     er/mark-block
     er/mark-sentence
     er/mark-word))
  (defun er/mark-outside-quotes ()
    "Mark the inside of the current string, not including the quotation marks."
    (interactive)
    (when (er--point-inside-string-p)
      (er--move-point-backward-out-of-string)
      (set-mark (point))
      (forward-char)
      (er--move-point-forward-out-of-string)
      (exchange-point-and-mark))))

(provide 'keytheme)

;;; keytheme.el ends here
