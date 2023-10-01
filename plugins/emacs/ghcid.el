;;; ghcid.el --- Really basic ghcid mode support with compilation-mode

;; Author: Matthew Wraith <wraithm@gmail.com>
;;         Yorick Sijsling
;; Maintainer: Matthew Wraith <wraithm@gmail.com>
;;             Yorick Sijsling
;;             Vasiliy Yorkin <vasiliy.yorkin@gmail.com>
;;             Neil Mitchell <ndmitchell@gmail.com>
;; URL: https://github.com/ndmitchell/ghcid
;; Version: 1.0
;; Created: 26 Sep 2014
;; Keywords: tools, files, Haskell
;; Package-Requires: ((emacs "28.1"))
;;; Commentary:

;; Use M-x ghcid to launch

;;; Code:

(require 'haskell-mode)
(require 'compile)
(require 'term)
(require 'dash)
(require 'subr-x)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Configuration

(defgroup ghcid nil
  "Ghcid development mode for Haskell."
  :group 'haskell)

(defcustom ghcid-repl-command-line nil
  "Command line to start GHCi, as a list: the executable and its arguments.
When nil, ghcid will guess the value depending on
`ghcid-project-root' contents.  This should usually be customized
as a file or directory variable.  Each element of the list is a
sexp which is evaluated to a string before being passed to the
shell."
  :group 'ghcid
  :type '(repeat sexp))

(put 'ghcid-repl-command-line 'safe-local-variable #'listp)

(defcustom ghcid-project-root nil
  "The project root, as a string or nil.
When nil, ghcid will guess the value by looking for a cabal file.
Customize as a file or directory variable."
  :group 'ghcid
  :type '(choice (const nil) string))

(put 'ghcid-project-root 'safe-local-variable #'stringp)

(defcustom ghcid-target nil
  "The target to demand from cabal repl, as a string or nil.
Customize as a file or directory variable.  Different targets
will be in different GHCi sessions."
  :group 'ghcid :safe t
  :type '(choice (const nil) string))

(put 'ghcid-target 'safe-local-variable #'stringp)

(defcustom ghcid-test-command-line nil
  "The demand to use as the test command parameter for ghcid.
Customize as a file or directory variable.  Different targets
will be in different GHCi sessions."
  :group 'ghcid :safe t
  :type '(choice (const nil) string))

(put 'ghcid-test-command-line 'safe-local-variable #'stringp)

(defcustom ghcid-setup-command-line nil
  "The demand to use as the setup command parameter for ghcid.
Customize as a file or directory variable.  Different targets
will be in different GHCi sessions."
  :group 'ghcid :safe t
  :type '(choice (const nil) string))

(put 'ghcid-setup-command-line 'safe-local-variable #'stringp)

(defcustom ghcid-lint-command-line nil
  "The demand to use as the lint command parameter for ghcid.
Customize as a file or directory variable.  Different targets
will be in different GHCi sessions."
  :group 'ghcid :safe t
  :type '(choice (const nil) string))

(put 'ghcid-lint-command-line 'safe-local-variable #'stringp)

(defun ghcid-cabal-new-nix (d)
  "Non-nil if D contain a nix file and a cabal file."
  (and (directory-files d t "shell.nix\\|default.nix\\|flake.nix")
       (directory-files d t "cabal.project.local")))

(defun ghcid-cabal-nix (d)
  "Non-nil if D contain a nix file and a cabal file."
  (and (directory-files d t "shell.nix\\|default.nix\\|flake.nix")
       (directory-files d t ".cabal$")))

(defcustom ghcid-methods-alist
  `(;(new-flake-impure "flake.nix" ("nix" "develop" "--impure" "-c" "cabal" "v2-repl" (or ghcid-target (ghcid-package-name) #1="") "--builddir=dist/ghcid"))
    ;(new-flake "flake.nix" ("nix" "develop" "-c" "cabal" "v2-repl" (or ghcid-target (ghcid-package-name) #1="") "--builddir=dist/ghcid"))
    ;(flake-impure "flake.nix" ("nix" "develop" "--impure" "-c" "cabal" "v1-repl" (or ghcid-target (ghcid-package-name) #1="") "--builddir=dist/ghcid"))
    ;(flake "flake.nix" ("nix" "develop" "-c" "cabal" "v1-repl" (or ghcid-target (ghcid-package-name) #1="") "--builddir=dist/ghcid"))
    ;(styx "styx.yaml" ("styx" "repl" ghcid-target))
    ; (snack ,(lambda (d) (directory-files d t "package\\.\\(yaml\\|nix\\)")) ("snack" "ghci" ghcid-target)) ; too easy to trigger, confuses too many people.
    ;(new-impure-nix ghcid-cabal-new-nix ("nix-shell" "--run" (concat "cabal v2-repl " (or ghcid-target (ghcid-package-name) "") " --builddir=dist/ghcid")))
    ;(new-nix ghcid-cabal-new-nix ("nix-shell" "--pure" "--run" (concat "cabal v2-repl " (or ghcid-target (ghcid-package-name) "") " --builddir=dist/ghcid")))
    ;(nix ghcid-cabal-nix ("nix-shell" "--pure" "--run" (concat "cabal v1-repl " (or ghcid-target "") " --builddir=dist/ghcid")))
    ;(impure-nix ghcid-cabal-nix ("nix-shell" "--run" (concat "cabal v1-repl " (or ghcid-target "") " --builddir=dist/ghcid")))
    (predefined-ghcid ".ghcid" (""))
    (new-build "cabal.project.local" ("cabal" "new-repl" (or ghcid-target (format "%s:lib:%s" (ghcid-package-name) (ghcid-package-name)) nil)))
    ;(nix-ghci ,(lambda (d) (directory-files d t "shell.nix\\|default.nix")) ("nix-shell" "--pure" "--run" "ghci"))
    (stack "stack.yaml" ("stack" "repl" ghcid-target))
    ;(mafia "mafia" ("mafia" "repl" ghcid-target))
    (bare-cabal ,(lambda (d) (directory-files d t "..cabal$")) ("cabal" "repl" ghcid-target))
    (bare-ghci ,(lambda (_) t) ("ghci")))
"How to automatically locate project roots and launch GHCi.
This is an alist from method name to a pair of
a `locate-dominating-file' argument and a command line."
  :type '(alist :key-type symbol :value-type (list (choice (string :tag "File to locate") (function :tag "Predicate to use")) (repeat sexp))))

(defcustom ghcid-methods (-map 'car ghcid-methods-alist)
  "Keys in `ghcid-methods-alist' to try, in order.
Consider setting this variable as a directory variable."
   :group 'ghcid :safe t :type '(repeat symbol))

(put 'ghcid-methods 'safe-local-variable #'listp)

(defun ghcid-initialize-method ()
  "Initialize `ghcid-project-root' and `ghcid-repl-command-line'.
Do it according to `ghcid-methods' and previous values of the above variables."
  (or (--first (let ((root (locate-dominating-file default-directory (nth 0 it))))
                 (when root
                   (setq-local ghcid-project-root (or ghcid-project-root root))
                   (setq-local ghcid-repl-command-line (or ghcid-repl-command-line (nth 1 it)))))
               (-non-nil (--map (alist-get it ghcid-methods-alist)
                                ghcid-methods)))
      (error "No GHCi loading method applies.  Customize
      `ghcid-methods' or
      (`ghcid-repl-command-line' and `ghcid-project-root')")))

(defun ghcid-repl-command-line ()
  "Return the command line for running GHCi.
If the variable `ghcid-repl-command-line' is non-nil, it will be
returned.  Otherwise, use `ghcid-initialize-method'."
  (or ghcid-repl-command-line
      (progn (ghcid-initialize-method) ghcid-repl-command-line)))

(defun ghcid-project-root ()
  "Get the root directory for the project.
If the variable `ghcid-project-root' is non-nil, return that,
otherwise search for project root using
`ghcid-initialize-method'."
  (or ghcid-project-root
      (progn (ghcid-initialize-method) ghcid-project-root)))

(defun ghcid-test-command-line ()
  "Get the test command for ghcid.
If the variable `ghcid-test-command-line' is non-nil, return that,
otherwise return \\='return ()'."
  (if ghcid-test-command-line (format "--test=\"%s\"" ghcid-test-command-line) ""))

(defun ghcid-setup-command-line ()
  "Get the setup command for ghcid.
If the variable `ghcid-setup-command-line' is non-nil, return that,
otherwise return \\=':set myide ghcid'."
  (if ghcid-setup-command-line (format "--setup=\"%s\"" ghcid-setup-command-line) ""))

(defun ghcid-lint-command-line ()
  "Get the lint command for ghcid.
If the variable `ghcid-lint-command-line' is non-nil, return that,
otherwise return \\='true'."
  (if ghcid-lint-command-line (format "--lint=\"%s\"" ghcid-lint-command-line) ""))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Session-local variables. These are set *IN THE GHCi INTERACTION BUFFER*

(defvar-local ghcid-command-line nil "Command line used to start GHCi.")
(defvar-local ghcid-package-name nil "The package name associated with the current buffer.")
(defvar-local ghcid-state nil
  "The state could has following setting:
- nil: initial state
- deleting: The process of the buffer is being deleted.
- dead: GHCi died on its own. Do not try restarting
automatically. The user will have to manually run `ghcid-restart'
to destroy the buffer and create a fresh one without this variable enabled.
- other value: informative value for the user about what GHCi is doing.")

(defun ghcid-get-var (symbol)
  "Return the value of SYMBOL in the GHCi process buffer."
  (let ((bp (ghcid-buffer-p))) (when bp (buffer-local-value symbol bp))))

(defun ghcid-package-name (&optional cabal-file)
  "Get the current package name from a nearby .cabal file.
If there is none, return an empty string.  If specified, use
CABAL-FILE rather than trying to locate one."
  (or ghcid-package-name
      (setq ghcid-package-name
            (let ((cabal-file (or cabal-file
                                  (ghcid-cabal-find-file))))
              (if cabal-file
                  (replace-regexp-in-string
                   ".cabal$" ""
                   (file-name-nondirectory cabal-file))
                "")))))

(defun ghcid-cabal-find-file (&optional file)
  "Search for directory of cabal file.
upwards from FILE (or `default-directory' if nil)."
  (let ((dir (locate-dominating-file (or file default-directory)
                                     (lambda (d) (directory-files d t ".\\.cabal\\'")))))
    (when dir (car (directory-files dir t ".\\.cabal\\'")))))


;;; Original ghcid emacs plugin from the ghcid repostory.
;;;###autoload
(define-minor-mode ghcid-mode
  "A minor mode for ghcid terminals.

Use `ghcid' to start a ghcid session in a new buffer. The process
will start in the directory of your project, i.e., the directory
where your .cabal or stack.yaml file located.

It is based on `compilation-mode'. That means the errors and
warnings can be clicked and the `next-error'(\\[next-error]) and
`previous-error'(\\[previous-error]) commands will work as usual.

To configure where the new buffer should appear, customize your
`display-buffer-alist'. For instance like so:

    (add-to-list
     \\='display-buffer-alist
     \\='(\"*ghcid*\"
       (display-buffer-reuse-window   ;; First try to reuse an existing window
        display-buffer-at-bottom      ;; Then try a new window at the bottom
        display-buffer-pop-up-window) ;; Otherwise show a pop-up
       (window-height . 18)      ;; New window will be 18 lines
       ))

If the window that shows ghcid changes size, the process will not
recognize the new height until you manually restart it by calling
`ghcid' again."
  :lighter " Ghcid"
  :keymap
  (list (cons (kbd "C-c g h c i d s") #'ghcid)
        (cons (kbd "C-c g h c i d k") #'ghcid-stop))
  (if ghcid-mode
       (add-hook 'hack-local-variables-hook #'my-local-variables-hook)
       (remove-hook 'hack-local-variables-hook #'my-local-variables-hook)))

;;; local-variables hook hack
(defun my-local-variables-hook ()
    (when (derived-mode-p 'haskell-mode) (ghcid)))

(defun ghcid-activate-mode ()
  "Really activate the ghcid mode."
  (when (fboundp 'nlinum-mode) (nlinum-mode -1))
  (when (fboundp 'linum-mode) (linum-mode -1))
  (when (fboundp 'display-line-numbers-mode) (display-line-numbers-mode -1))
  (read-only-mode 1)
  (compilation-minor-mode))

(defun ghcid-buffer-name ()
  "Construct a ghcid buffer name."
  (concat "*" "ghcid" "*"))

(defun ghcid-buffer-p ()
  "Return the GHCi buffer if it exists, nil otherwise."
  (get-buffer (ghcid-buffer-name)))


;; TODO Pass in compilation command like compilation-mode
(defun ghcid-command (cmd testcmd setupcmd lintcmd h)
  "Construct a ghcid command.
with the specified CMD, TESTCMD, SETUPCMD, LINTCMD and H."
  (format "ghcid %s %s %s %s --height=%s\n"
          cmd testcmd setupcmd lintcmd h))

(defun ghcid-get-buffer ()
  "Create or reuse a ghcid buffer with the configured name."
  (get-buffer-create (ghcid-buffer-name)))

(defun ghcid-get-buffer-display ()
  "Create or reuse a ghcid buffer with the configured name and display it.
Return the window that show the buffer.

User configuration will influence where the buffer gets shown
exactly. See `ghcid-mode'."
  (display-buffer (ghcid-get-buffer) '((display-buffer-reuse-window                ;; first, try to reuse existing window
                                        display-buffer-in-side-window)             ;; otherwise, create in side window
                                       (side . right)                              ;; at right side
                                       (slot . 0)                                  ;; slot 0
                                       (window-width . fit-window-to-buffer)       ;; fit window width to buffer
                                       )))

(defun ghcid-start (dir cmd testcmd setupcmd lintcmd)
  "Start ghcid.
in the specified directory DIR and CMD, TESTCMD, SETUPCMD and LINTCMD."

  (with-selected-window (ghcid-get-buffer-display)

    ;; Do we really want this? The HLS can provide a list of errors.
    ;; So far, I just comment out this.
    ;; (setq next-error-last-buffer (current-buffer))
    (setq-local default-directory dir)

    ;; Only now we can figure out the height to pass along to the ghcid process
    (let ((height (- (window-body-size) 1)))

      (term-mode)
      (term-line-mode)  ;; Allows easy navigation through the buffer
      (ghcid-activate-mode) ;; This will active the compilation-mode

      ;; Compilation mode does some caching for markers in files, but it gets confused
      ;; because ghcid reloads the files in the same process. Here we parse the
      ;; 'Reloading...' message from ghcid and flush the cache for the mentioned
      ;; files. This approach is very similar to the 'omake' hacks included in
      ;; compilation mode.
      (add-to-list
       'compilation-error-regexp-alist-alist
       '(ghcid-reloading
         "Reloading\\.\\.\\.\\(\\(\n  .+\\)*\\)" 1 nil nil nil nil
         (0 (progn
              (let* ((filenames (cdr (split-string (match-string 1) "\n  "))))
                (dolist (filename filenames)
                  (compilation--flush-file-structure filename)))
              nil))))
      (add-to-list 'compilation-error-regexp-alist 'ghcid-reloading)

      (setq-local term-buffer-maximum-size height)
      (setq-local scroll-up-aggressively 1)
      (setq-local show-trailing-whitespace nil)

      (term-exec (ghcid-buffer-name)
           "ghcid"
           "/bin/bash"
           nil
           (list "-c" (ghcid-command cmd testcmd setupcmd lintcmd height))))))

(defun ghcid-kill ()
  "Kill the ghcid buffer and process."
  (let* ((ghcid-buf (get-buffer (ghcid-buffer-name)))
         (ghcid-proc (get-buffer-process ghcid-buf)))
    (when (processp ghcid-proc)
      (progn
        (set-process-query-on-exit-flag ghcid-proc nil)
        (kill-process ghcid-proc)))))

;; TODO Close stuff if it fails
;;;###autoload
(defun ghcid ()
  "Start a ghcid process in a new window. Kill any existing sessions.

The process will be started in the directory of your .cabal or stack.yaml
project root."
  (interactive)
  (let* ((root (ghcid-project-root))
         (replcmd (-non-nil (-map #'eval (ghcid-repl-command-line))))
         (combinedReplcmd (combine-and-quote-strings replcmd))
         (ghcidcmd (if (string-blank-p combinedReplcmd) "" (format "--command=%s" combinedReplcmd)))
         (testcmd (ghcid-test-command-line))
         (setupcmd (ghcid-setup-command-line))
         (lintcmd (ghcid-lint-command-line)))
    (ghcid-start root ghcidcmd testcmd setupcmd lintcmd)))

;; Assumes that only one window is open
;;;###autoload
(defun ghcid-stop ()
  "Stop ghcid."
  (interactive)
  (ghcid-kill)
  (kill-buffer (ghcid-buffer-name)))


(provide 'ghcid-mode)

;;; ghcid.el ends here
