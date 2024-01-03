;;; mise.el --- Emacs library for interacting with mise -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Vitaly Slobodin
;; Author: Vitaly Slobodin <vitaliy.slobodin@gmail.com>
;; Keywords: version manager, mise
;;

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; 'mise.el' is an Emacs integration for "mise" version manager.
;; This file is pretty-much code&paste of https://github.com/tabfugnic/asdf.el
;; Usage:
;; (add-hook 'prog-mode-hook 'mise-setup-env)

;;; Code:

(require 'subr-x)
(require 'cl-seq)
(require 'compile)

;; Customizable variables
(defgroup mise nil
  "Settings to interact with mise."
  :version "0.1.0"
  :group 'applications)

(defcustom mise-path "$HOME/.local/share/mise"
  "Path to mise directory."
  :type 'string
  :group 'mise)

(defcustom mise-binary "mise"
  "Path to mise binary."
  :type 'string
  :group 'mise)

(define-compilation-mode
  mise-compilation-mode
  "mise compilation"
  "Compilation output for mise.")

;;;###autoload
(defun mise-install (&optional name version)
  "Install tools.
Run this command with no arguments and it will install all tools
based on the .tools-version if available.  Optionally pass NAME
to specify the tool being installed.  Optionally specify
VERSION."
  (interactive
   (let ((name (completing-read "Tool: " (cons " " (mise--plugin-list-list)))))
     (if (not (string-blank-p name))
         (list name (completing-read "Version: " (cons " " (mise--list-all-list name))))
       (list name))))
  (compile
   (substitute-env-vars
    (string-join
     (cl-remove-if 'null `(,mise-binary "install" ,name ,version)) " "))
   'mise-compilation-mode))

;;;###autoload
(defun mise-current ()
  "Get current versions being used in path."
  (interactive)
  (shell-command (mise--command "current")))

;;;###autoload
(defun mise-plugin-list()
  "Get currently installed plugin list."
  (interactive)
  (shell-command (mise--command "plugin" "ls")))

;;;###autoload
(defun mise-plugin-add(name &optional git-url)
  "Add a new plugin by NAME.
Optionally supply a GIT-URL for git repository to a plugin."
  (interactive
   (let ((input (split-string
                 (completing-read "Plugin: " (cons " " (mise--plugin-list-all-list)))
                 " " t " ")))
     (if (not (string-blank-p (car input)))
         (list (car input) (read-string "Git URL: " (cadr input)))
       (list (car input)))))
  (compile
   (substitute-env-vars
    (string-join
     (cl-remove-if 'null `(,mise-binary "plugin" ,"add" ,name ,git-url)) " "))
   'mise-compilation-mode))

(defun mise--plugin-list-list()
  "Get currently installed plugin list as usable strings."
  (mise--format-output-to-list
   (shell-command-to-string (mise--command "plugin" "ls"))))

(defun mise--plugin-list-all-list()
  "Get currently installed plugin list as usable strings."
  (mise--format-output-to-list
   (shell-command-to-string (mise--command "plugin" "ls" "--all"))))

(defun mise--list-all-list(tool)
  "Get list all versions for specific TOOL."
  (mise--format-output-to-list
   (shell-command-to-string (mise--command "ls" "--all" tool))))

(defun mise--format-output-to-list (shell-output)
  "Take SHELL-OUTPUT and format it into a usable list to select from."
  (split-string
   (replace-regexp-in-string
    (rx (* (any " \t\n")) eos)
    ""
    shell-output) "\n"))

(defun mise--command(&rest args)
  "Construct command using ARGS and binary for execution."
  (substitute-env-vars
   (string-join (cons mise-binary args) " ")))

(defun mise--bin-paths()
  "Get paths to installed plugins bins."
  (mise--format-output-to-list
   (shell-command-to-string (mise--command "bin-paths"))))

;;;###autoload
(defun mise-bin-path(binary)
  "Get a bin path for specific BINARY."
  (string-trim-right (shell-command-to-string (mise--command "which" binary))))

;;;###autoload
(defun mise-setup-env()
  "Invoke mise to configure environment: \"exec-path\" and PATH variable."
  (interactive)
  (setenv "PATH" (concat (string-join (mise--bin-paths) ":") ":" (getenv "PATH")))
  (setq exec-path (nconc (mise--bin-paths) exec-path)))

(provide 'mise)

;;; mise.el ends here
