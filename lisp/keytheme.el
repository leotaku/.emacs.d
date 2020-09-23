;;; keytheme.el --- custom keybinding theme -*- lexical-binding: t; -*-

;;; Code:

(bk-block modal-editing
  :requires .theist-mode .viper-cmd .modalka expand-region .multiple-cursors
  :custom
  (modalka-cursor-type . 'box)
  (cursor-type . 'bar)
  :bind (("<f7>" . modalka-mode)
         ("<escape>" . modalka-mode))
  :bind* (("C-x C-x" . theist-C-c)
          ("<C-return>" . open-line))
  :config
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
   ("e" . ((viper-end-of-word arg)
           (forward-char 1)))
   ("E" . ((viper-end-of-Word arg)
           (forward-char 1))))
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
   ("m" . eri/expand-region)
   ("n" . eri/contract-region)
   ("s" . ((if (region-active-p)
               (call-interactively #'vr/mc-mark)
             (avy-goto-word-or-subword-1)))))
  (modalka-keys
   ("v" . switch-mark-command)
   ("r" . ((if (region-active-p)
               (call-interactively #'vr/replace)
             (call-interactively #'viper-replace-char))))
   ("o" . exchange-point-and-mark)
   ("U" . fi-undo-only-global)
   ("u" . fi-undo-global)
   ("p" . viper-put-back)
   ("P" . viper-Put-back)
   (";" . comment-or-uncomment-region)
   ("%" . eri/maximize-region)
   ("f" . ((let ((beg (mark)))
             (call-interactively #'viper-find-char-forward)
             (when (region-active-p)
               (set-mark beg)))))
   ("t" . ((let ((beg (mark)))
             (call-interactively #'viper-goto-char-forward)
             (when (region-active-p)
               (set-mark beg)))))
   ("," . ((let ((beg (mark))
                 (active (region-active-p)))
             (call-interactively #'viper-repeat-find)
             (when active
               (set-mark beg)
               (transient-mark-mode)))))))

(bk-block multiple-cursors
  :requires .multiple-cursors
  :bind ((:mc/keymap
          :package multiple-cursors
          ("<return>" . nil))))

(bk-block expand-region
  :requires .expand-region-improved
  :bind (("M-m" . eri/expand-region)
         ("M-n" . eri/contract-region))
  :config
  (eri/define-pair org-table-cell "|" 'org-at-table-p)
  (eri/add-mode-expansions 'org-mode
    '((eri/mark-inside-org-table-cell
       eri/mark-outside-org-table-cell))))

;;; keytheme.el ends here
