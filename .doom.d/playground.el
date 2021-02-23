;;; ../dotfiles/.doom.d/playground.el -*- lexical-binding: t; -*-

(switch-to-buffer (other-buffer))


(defun delete-all ()
  (interactive)
  (let ((start (point-min))
        (end (point-max)))
    (save-excursion
      (upcase-region start end))
    ))
