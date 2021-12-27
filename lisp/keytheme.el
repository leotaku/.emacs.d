;;; keytheme.el --- custom keybinding theme -*- lexical-binding: t; -*-

;;; Code:

(bk-block modal-editing
  :requires .avy .theist-mode .modalka expand-region multiple-cursors
  :custom
  (modalka-cursor-type . 'box)
  (cursor-type . 'bar)
  (motion-mode-function . #'modalka-mode)
  :bind (("<escape>" . modalka-mode))
  :bind* (("C-x C-x" . theist-C-c)
          ("<C-return>" . open-line))
  :config
  (advice-add 'modalka-mode :around #'fi-call-silent)
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
   ("i" . motion-insert)
   ("a" . motion-append)
   ("I" . motion-Insert)
   ("A" . motion-Append)
   ("(" . backward-char)
   ("j" . next-line)
   ("k" . previous-line)
   ("l" . forward-char)
   ("w" . motion-forward-word)
   ("W" . motion-forward-Word)
   ("b" . motion-backward-word)
   ("B" . motion-backward-Word)
   ("e" . motion-forward-end)
   ("E" . motion-forward-End))
  (modalka-keys
   ("c" . motion-change)
   ("d" . kill-region-or-line)
   ("y" . copy-region-or-line))
  (modalka-keys
   ("SPC" . mc/mark-next-like-this)
   ("<backspace>" . delete-char-or-region)
   ("g" . goto-or-quit)
   ("x" . theist-C-x)
   ("z" . kill-ring-cycle)
   ("Z" . kill-ring-uncycle)
   ("m" . eri/expand-region)
   ("n" . eri/contract-region)
   ("s" . avy-goto-word-or-subword-1))
  (modalka-keys
   ("v" . switch-mark-command)
   ("r" . replace-char-or-region)
   ("o" . exchange-point-and-mark)
   ("u" . undo)
   ("U" . undo-redo)
   ("p" . yank-put-after)
   ("P" . yank-put-before)
   (";" . comment-or-uncomment-region)
   ("%" . eri/maximize-region)
   ("f" . motion-goto-char)
   ("t" . motion-till-char)
   ("," . motion-repeat-char)))

(defun replace-char-or-region ()
  (interactive)
  (if (region-active-p)
      (call-interactively #'vr/replace)
    (call-interactively #'replace-char)))

(defun delete-char-or-region (arg)
  (interactive "p")
  (if (region-active-p)
      (delete-region (region-beginning) (region-end))
    (delete-char arg)))

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
