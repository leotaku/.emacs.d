;;; keytheme.el --- custom keybinding themes

;;; Code:

(leaf ryo-modal
  :straight t
  :leaf-defer t
  :bind* (("<f7>" . ryo-modal-mode-back))
  :require viper expand-region avy
  :custom
  (ryo-modal-cursor-type . 'box)
  :config
  (ryo-modal-keys
   ;; back to insert
   ("<f7>" forward-char-on-line :exit t)
   ("i" ryo-modal-mode)
   ("I" back-to-indentation :exit t)
   ("a" forward-char-on-line :exit t)
   ("A" move-end-of-line :exit t)
   ;; hjkl
   ("h" backward-char)
   ("k" previous-line)
   ("j" next-line)
   ("l" forward-char)
   ;; movement
   ("H" beginning-of-visual-line)
   ("L" end-of-visual-line)
   ("w" viper-forward-word)
   ("W" viper-forward-Word)
   ("b" viper-backward-word)
   ("B" viper-backward-Word)
   ("e" viper-end-of-word)
   ("E" viper-end-of-Word)
   ;; marking
   ("v" toggle-region)
   ("m" er/expand-region)
   ("n" er/contract-region)
   ;; actions
   ("d" kill-region-or-line)
   ("y" copy-region-or-line)
   ("c" kill-region-or-line :exit t)
   ;; default
   ("p" viper-put-back)
   ("P" viper-Put-back)
   ;; theist
   ("x" theist-C-x)
   ("z" theist-C-c)
   ;; misc
   ("g" goto-or-quit)
   ("o" exchange-point-and-mark)
   ("u" fi-undo-only-global)
   ("U" fi-undo-global)
   ("s" avy-goto-word-or-subword-1)
   ("f" viper-find-char-forward)
   ("t" viper-goto-char-forward)
   ("SPC" mc/mark-down-or-more)
   ;; digits
   ("1" digit-argument)
   ("2" digit-argument)
   ("3" digit-argument)
   ("4" digit-argument)
   ("5" digit-argument)
   ("6" digit-argument)
   ("7" digit-argument)
   ("8" digit-argument)
   ("9" digit-argument)
   ("0" digit-argument)
   ("-" negative-argument))
  '|config
  (defun advice-ryo-modal (func args)
    (let ((inhibit-message t))
      (funcall func args)))
  (advice-add 'ryo-modal-mode :around 'advice-ryo-modal)
  (fset 'ryo-modal--cursor-color-update (lambda (&rest _) nil))
  (add-to-list
   'emulation-mode-map-alists
   `((ryo-modal-mode . ,ryo-modal-mode-map))))

(leaf expand-region
  :straight t
  :leaf-defer t
  :bind (("M-m" . er/expand-region)
         ("M-n" . er/contract-region))
  :config
  (setq  er/try-expand-list (fi-insert-after
                             er/try-expand-list
                             'er/mark-defun
                             'er/mark-text-paragraph))
  (defun er/mark-line ()
    (interactive)
    (setf (point) (point-at-eol))
    (forward-char)
    (set-mark (point))
    (backward-char)
    (setf (point) (point-at-bol))))

(leaf multiple-cursors
  :straight t
  :leaf-defer t
  :bind* (("M-j" . mc/mark-next-lines)))

;; custom commands

(defun ryo-modal-mode-back (arg)
  (interactive "p")
  (unless (= (point) (point-at-bol))
    (ignore-errors
      (backward-char arg)))
  (ryo-modal-mode 1))

(defun toggle-region ()
  (interactive)
  (if (region-active-p)
      (deactivate-mark)
    (set-mark (1+ (point)))))

(defun kill-region-or-line (arg)
  (interactive "p")
  (if (region-active-p)
      (kill-region (region-beginning) (region-end))
    (kill-whole-line arg)))

(defun copy-region-or-line (arg)
  (interactive "p")
  (if (region-active-p)
      (copy-region-as-kill (region-beginning) (region-end))
    (viper-yank-line arg)))

(defun goto-or-quit (arg)
  (interactive "P")
  (if (numberp arg)
      (if (> arg 0)
          (goto-line arg)
        (goto-line (+ arg (line-number-at-pos (point-max)))))
    (fi-universal-quit)))

(defun mc/mark-down-or-more (arg)
  (interactive "p")
  (if (region-active-p)
      (dotimes (_ arg) (mc/mark-next-like-this 1))
    (mc/mark-next-lines arg)))

(defun do-deactivate-mark ()
  (interactive)
  (deactivate-mark))

(defun forward-char-on-line (arg)
  (interactive "p")
  (unless (= (point) (point-at-eol))
    (ignore-errors
      (forward-char arg))))

(provide 'keytheme)
;;; keytheme.el ends here
