;;; keytheme.el --- custom keybinding theme -*- lexical-binding: t; -*-

;;; Code:

(bk-block modal-editing
  :requires .theist-mode .viper-cmd .modalka expand-region multiple-cursors
  :custom
  (modalka-cursor-type . 'box)
  (cursor-type . 'bar)
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
   ("i" . modalka-mode)
   ("h" . backward-char)
   ("a" . modalka-append)
   ("I" . modalka-Insert)
   ("A" . modalka-Append)
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
   ("c" . modalka-change)
   ("d" . kill-region-or-line)
   ("y" . copy-region-or-line))
  (modalka-keys
   ("SPC" . mc/mark-next-like-this)
   ("<backspace>" . delete-char-or-region)
   ("g" . goto-or-quit)
   ("x" . theist-C-x)
   ("z" . theist-C-c)
   ("m" . eri/expand-region)
   ("n" . eri/contract-region)
   ("s" . avy-goto-word-or-subword-1))
  (modalka-keys
   ("v" . switch-mark-command)
   ("r" . replace-char-or-region)
   ("o" . exchange-point-and-mark)
   ("U" . fi-undo-only-global)
   ("u" . fi-undo-global)
   ("p" . yank-put-after)
   ("P" . yank-put-before)
   (";" . comment-or-uncomment-region)
   ("%" . eri/maximize-region)
   ("f" . jump-to-char)
   ("t" . jump-till-char)
   ("," . jump-repeat)))

(defun replace-char-or-region ()
  (interactive)
  (if (region-active-p)
      (call-interactively #'vr/replace)
    (call-interactively #'replace-char)))

(defun modalka-append ()
  (interactive)
  (when (/= (point) (point-at-eol))
    (forward-char))
  (modalka-mode -1))

(defun modalka-Insert ()
  (interactive)
  (back-to-indentation)
  (modalka-mode -1))

(defun modalka-Append ()
  (interactive)
  (end-of-line)
  (modalka-mode -1))

(defun modalka-change (arg)
  (interactive "p")
  (kill-region-or-line arg)
  (modalka-mode -1))

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
