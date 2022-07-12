;;; ../dotfiles/.doom.d/lisp/triggers.el -*- lexical-binding: t; -*-

(require 'comint)

(define-derived-mode triggers-test-mode
  comint-mode "triggers-test"
  (compilation-setup))

(defun triggers-test--get-buffer ()
  (if (eq major-mode 'triggers-test-mode)
      (current-buffer)
    (get-buffer-create "*triggers-tests*")))

;;;###autoload
(defun triggers-test-run ()
  (interactive)
  (triggers-test--run-tests))

;(setq triggers-test--test-command "simon-docker run test ./manage.py run_trigger_integration_tests")
(setq triggers-test--test-command "cdw && simon-boot && djtest ./manage.py run_trigger_integration_tests")

(defun triggers-test--run-tests ()
  (let* ((buffer (triggers-test--get-buffer))
         (process (get-buffer-process buffer)))
    (with-current-buffer buffer
      (when process
        (delete-process process))
      (let ((inhibit-read-only t))
        (erase-buffer))
      (unless (eq major-mode 'triggers-test-mode)
        (triggers-test-mode))
      (compilation-forget-errors)
      (make-comint-in-buffer "trigger-tests" buffer "zsh" nil "-ic" triggers-test--test-command)
      (display-buffer buffer))))
