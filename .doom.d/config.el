;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;(when (memq window-system '(mac ns x))
;  (exec-path-from-shell-initialize))

(setq shell-command-switch "-ic")

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

(load! "lisp/triggers")

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Adrian Aleixandre"
      user-mail-address "aaleixandre@bushelpowered.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
(setq doom-font (font-spec :family "Fira Code" :size 12))
(setq doom-big-font (font-spec :family "Fira Code" :size 18))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
;;(setq doom-theme 'doom-dracula)
;;(setq doom-theme 'doom-challenger-deep)
(setq doom-theme 'alabaster)

(custom-set-faces!
  '(ivy-minibuffer-match-face-1 :foreground "#aaaaaa"))

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type nil)

(setq tab-width 2)

;; Raise the default
(setq gc-cons-threshold 20000000)

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c g k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c g d') to jump to their definition and see how
;; they are implemented.

(setq doom-localleader-key ",")

;; Movement
;;(map! "C-h" #'evil-window-left)
;;(map! "C-j" #'evil-window-down)
;;(map! "C-k" #'evil-window-up)
;;(map! "C-l" #'evil-window-right)
(map! "<home>" #'beginning-of-line)
(map! "<end>" #'end-of-line)

;; Splitting
(map! :leader
      :desc "New vertical split" "w /" #'evil-window-vsplit)
(map! :leader
      :desc "New horizontal split" "w -" #'evil-window-split)

;; Frame/window management
(map! :leader "w o" #'other-frame)

;; Python
; disable formatting on save for python mode
(add-to-list '+format-on-save-enabled-modes 'python-mode t)


;(setq python-pytest-executable "run_tests()\n{\nzsh -ic \"simon-docker unit-tests $*\"\n}\nrun_tests")
;(setq python-pytest-executable "zsh -ic 'simon-docker unit-tests \"$0\" \"$@\"'")
(setq python-pytest-executable "zsh -ic 'simon-boot && DJANGO_SETTINGS_MODULE=settings.test poetry run pytest -rw \"$@\" --nomigrations'")
(setq python-pytest-unsaved-buffers-behavior t)

(map! :mode python-mode :localleader "t r" #'python-pytest-repeat)
; Run trigger integration tests
(map! :mode python-mode :localleader "t i" #'triggers-test-run)

;(defun python-setup-flycheck ()
;  (flycheck-add-next-checker 'lsp 'python-flake8))
;(add-hook 'python-mode-local-vars-hook #'python-setup-flycheck)


;; Associate .leex (Elixir LiveView) with web-mode
(add-to-list 'auto-mode-alist '("\\.leex\\'" . web-mode))

;; Associate .tsx with typescript-mode
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-mode))

(setq-default flycheck-disabled-checkers '(typescript-tslint))

;; Magit Forge
(setq auth-sources '("~/.authinfo"))
(setq forge-owned-accounts '(("Vynlar")))
(map! :leader
      :desc "List assigned pull requests"
      "g l a" #'forge-list-assigned-pullreqs)

(map! :leader
      "g e" #'forge-edit-topic-assignees)

(defun forge-assign-topic-to-me (n)
  "Edit the assignees of the current topic.
If there is no current topic or with a prefix argument read a
topic N and modify that instead."
  (interactive (list (forge-read-topic "Edit assignees of")))
  (let* ((topic (forge-get-topic n))
         (repo  (forge-get-repository topic)))
    (forge--set-topic-assignees repo topic '("Vynlar"))))

(map! :map forge-topic-mode-map
      "C-c C-a" #'forge-assign-topic-to-me)
(map! :leader
      "g a" #'forge-assign-topic-to-me)

;(add-hook 'after-init-hook #'global-prettier-mode)
;(add-hook! typescript-mode #'prettier-mode)
;(add-hook! javascript-mode #'prettier-mode)
;(add-hook! js2-mode #'prettier-mode)
;(add-hook! web-mode #'prettier-mode)

;(add-hook 'typescript-mode-hook #'format-all-mode)
;(setq-hook! 'typescript-mode-hook +format-with 'prettier)
(setq +format-with-lsp nil)


;; Scala
(map!
 :mode scala-modebas
 :localleader
 :prefix ("l" . "LSP Lens")
 "s" #'lsp-lens-show
 "h" #'lsp-lens-hide
 "l" #'lsp-avy-lens)
(map!
 :mode scala-mode
 :localleader
 "T" #'lsp-metals-treeview)
(map!
 :mode scala-mode
 :localleader
 :prefix ("t" . "Test")
 "a" #'espr-run-all-tests)
(map!
 :mode scala-mode
 :localleader
 :prefix ("e" . "Error")
 "e" #'lsp-treemacs-errors-list)
(map!
 :mode scala-mode
 :localleader
 :prefix ("r" . "Run")
 "r" #'dap-debug
 "d" #'dap-delete-session
 "D" #'dap-delete-all-sessions
 "l" #'dap-debug-last)

(setq treemacs-text-scale 1)

(defun triggers--scala-debug-provider (conf)
  (if (and (plist-get conf :debugServer)
           (plist-get conf :name))
      conf
    (-let (((&DebugSession :name :uri)
            (lsp-send-execute-command
             "debug-adapter-start"
             conf)))
      (list :debugServer (-> uri
                             (split-string ":")
                             cl-third
                             string-to-number)
            :type "scala"
            :name name
            :host "localhost"
            :request "launch"
            :noDebug t))))

(add-hook! dap-mode (dap-register-debug-provider "scala" #'triggers--scala-debug-provider))
(defun espr-run-all-tests ()
    (interactive)
    (dap-debug (list :name "All Tests"
                     :type "scala"
                     :runType "testTarget"
                     :path (lsp--path-to-uri (buffer-file-name)))))

(defun espr-run-file-tests ()
    (interactive)
    (dap-debug (list :name "Tests File"
                     :type "scala"
                     :runType "testFile"
                     :path (lsp--path-to-uri (buffer-file-name)))))

(setq stimmung-themes-dark-highlight-color "#222277")
