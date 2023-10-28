;;; rtx.el --- Emacs library for interacting with rtx -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Vitaly Slobodin
;; Author: Vitaly Slobodin <vitaliy.slobodin@gmail.com>
;; Keywords: version manager, rtx
;;

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; 'rtx.el' is an Emacs integration for "rtx" version manager.
;; This file is pretty-much code&paste of https://github.com/tabfugnic/asdf.el
;; Usage:
;; (add-hook 'prog-mode-hook 'rtx-setup-env)

;;; Code:

(require 'subr-x)
(require 'cl-seq)
(require 'compile)

;; Customizable variables
(defgroup rtx nil
  "Settings to interact with rtx."
  :version "0.1.0"
  :group 'applications)

(defcustom rtx-path "$HOME/.local/share/rtx"
  "Path to rtx directory."
  :type 'string
  :group 'rtx)

(defcustom rtx-binary "rtx"
  "Path to rtx binary."
  :type 'string
  :group 'rtx)

(define-compilation-mode
  rtx-compilation-mode
  "rtx compilation"
  "Compilation output for rtx.")

;;;###autoload
(defun rtx-install (&optional name version)
  "Install tools.
Run this command with no arguments and it will install all tools
based on the .tools-version if available.  Optionally pass NAME
to specify the tool being installed.  Optionally specify
VERSION."
  (interactive
   (let ((name (completing-read "Tool: " (cons " " (rtx--plugin-list-list)))))
     (if (not (string-blank-p name))
         (list name (completing-read "Version: " (cons " " (rtx--list-all-list name))))
       (list name))))
  (compile
   (substitute-env-vars
    (string-join
     (cl-remove-if 'null `(,rtx-binary "install" ,name ,version)) " "))
   'rtx-compilation-mode))

;;;###autoload
(defun rtx-current ()
  "Get current versions being used in path."
  (interactive)
  (shell-command (rtx--command "current")))

;;;###autoload
(defun rtx-plugin-list()
  "Get currently installed plugin list."
  (interactive)
  (shell-command (rtx--command "plugin" "ls")))

;;;###autoload
(defun rtx-plugin-add(name &optional git-url)
  "Add a new plugin by NAME.
Optionally supply a GIT-URL for git repository to a plugin."
  (interactive
   (let ((input (split-string
                 (completing-read "Plugin: " (cons " " (rtx--plugin-list-all-list)))
                 " " t " ")))
     (if (not (string-blank-p (car input)))
         (list (car input) (read-string "Git URL: " (cadr input)))
       (list (car input)))))
  (compile
   (substitute-env-vars
    (string-join
     (cl-remove-if 'null `(,rtx-binary "plugin" ,"add" ,name ,git-url)) " "))
   'rtx-compilation-mode))

(defun rtx--plugin-list-list()
  "Get currently installed plugin list as usable strings."
  (rtx--format-output-to-list
   (shell-command-to-string (rtx--command "plugin" "ls"))))

(defun rtx--plugin-list-all-list()
  "Get currently installed plugin list as usable strings."
  (rtx--format-output-to-list
   (shell-command-to-string (rtx--command "plugin" "ls" "--all"))))

(defun rtx--list-all-list(tool)
  "Get list all versions for specific TOOL."
  (rtx--format-output-to-list
   (shell-command-to-string (rtx--command "ls" "--all" tool))))

(defun rtx--format-output-to-list (shell-output)
  "Take SHELL-OUTPUT and format it into a usable list to select from."
  (split-string
   (replace-regexp-in-string
    (rx (* (any " \t\n")) eos)
    ""
    shell-output) "\n"))

(defun rtx--command(&rest args)
  "Construct command using ARGS and binary for execution."
  (substitute-env-vars
   (string-join (cons rtx-binary args) " ")))

(defun rtx--bin-paths()
  "Get paths to installed plugins bins."
  (rtx--format-output-to-list
   (shell-command-to-string (rtx--command "bin-paths"))))

;;;###autoload
(defun rtx-bin-path(binary)
  "Get a bin path for specific BINARY."
  (string-trim-right (shell-command-to-string (rtx--command "which" binary))))

;;;###autoload
(defun rtx-setup-env()
  "Invoke rtx to configure environment: \"exec-path\" and PATH variable."
  (interactive)
  (setenv "PATH" (concat (string-join (rtx--bin-paths) ":") ":" (getenv "PATH")))
  (setq exec-path (nconc (rtx--bin-paths) exec-path)))

(provide 'rtx)

;;; rtx.el ends here
