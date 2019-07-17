(defun vr-test ()
  (interactive)
  (save-excursion
    (cl-destructuring-bind
        (regexp replace _ __)
        (vr--interactive-get-args
         'vr--mode-regexp-replace
         nil)
      (replace-regexp (pcre-to-elisp regexp) replace nil (point-min) (point-max)))))
