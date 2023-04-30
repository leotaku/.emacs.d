;;; keytheme.el --- custom keybinding theme -*- lexical-binding: t; -*-

;;; Code:

(bk-block modal-editing
  :requires .avy .theist-mode .modalka expand-region multiple-cursors
  :custom
  (modalka-cursor-type . 'box)
  (cursor-type . 'bar)
  (motion-mode-function . #'modalka-mode)
  :bind (("<escape>" . modalka-mode)
         ("<C-return>" . open-line))
  :config
  (advice-add 'modalka-mode :around #'fi-advice-silent)
  (add-to-list
   'emulation-mode-map-alists
   `((modalka-mode . ,modalka-mode-map)))
  :config
  (define-key modalka-mode-map [remap self-insert-command] #'ignore)
  (define-key modalka-mode-map (kbd "x") (theist-menu (kbd "C-x")))
  (define-key ctl-x-map (kbd "x") (theist-menu (kbd "C-c")))
  (modalka-multiplex 'digit-argument (identity)
    "0" "1" "2" "3" "4" "5" "6" "7" "8" "9")
  (modalka-keys
   ("-" . negative-argument)
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
   ("d" . motion-kill-region-or-line)
   ("y" . motion-copy-region-or-line))
  (modalka-keys
   ("SPC" . mc/mark-next-like-this)
   ("<backspace>" . motion-delete)
   ("g" . motion-goto-or-quit)
   ("z" . motion-kill-cycle)
   ("Z" . motion-kill-uncycle)
   ("m" . eri/expand-region)
   ("n" . eri/contract-region)
   ("s" . avy-goto-word-or-subword-1))
  (modalka-keys
   ("v" . motion-mark-cycle)
   ("r" . replace-char-or-region)
   ("o" . exchange-point-and-mark)
   ("u" . undo)
   ("U" . undo-redo)
   ("p" . motion-put-after)
   ("P" . motion-put-before)
   (";" . comment-or-uncomment-region)
   ("%" . eri/maximize-region)
   ("f" . motion-goto-char)
   ("t" . motion-till-char)
   ("," . motion-repeat-char)))

(defun replace-char-or-region ()
  (interactive)
  (if (use-region-p)
      (call-interactively #'vr/replace)
    (call-interactively #'motion-replace-char)))

(bk-block multiple-cursors
  :requires .multiple-cursors
  :bind ((:mc/keymap
          :package multiple-cursors
          ("<return>" . nil)
          ("C-0" . mc/insert-numbers))))

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
