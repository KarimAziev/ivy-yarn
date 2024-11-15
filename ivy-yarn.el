;;; ivy-yarn.el --- Run yarn commands in vterm with ivy completions -*- lexical-binding: t -*-

;; Copyright (C) 2022 Karim Aziiev <karim.aziiev@gmail.com>

;; Author: Karim Aziiev <karim.aziiev@gmail.com>
;; URL: https://github.com/KarimAziev/ivy-yarn
;; Keywords: tools
;; Version: 0.1.1
;; Package-Requires: ((emacs "28.1"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Run yarn commands with ivy completions.

;; Usage

;; (require 'ivy-yarn)

;; And then run in your project:
;;   M-x `ivy-yarn'

;; If project directory contains .nvmrc file,
;; a variable `ivy-yarn-use-nvm' is non-nil and nvm installed,
;; execute nvm-use before running command.

;; If vterm is installed and the value of variable
;; `ivy-yarn-use-vterm' is non-nil, run command in `vterm',
;; otherwise with `async-shell-command'.

;;
;; Customization

;; `ivy-yarn-global-config-directory'
;;    The path to the node and yarn directory with links.

;; `ivy-yarn-use-nvm'
;;     Whether to execute nvm-use if .nvmrc exists and nvm installed.

;; `ivy-yarn-use-vterm'
;;     Whether to run commands in `vterm', if installed.

;;; Code:

(require 'json)
(require 'ivy)
(require 'counsel)

(declare-function vterm "ext:vterm")
(declare-function vterm--invalidate "ext:vterm")
(declare-function vterm-send-string "ext:vterm")

(defgroup ivy-yarn nil
  "Run yarn in Emacs with `ivy-read'."
  :group 'tools)

(defcustom ivy-yarn-global-config-directory "~/.config/yarn"
  "The path to the yarn config."
  :type 'directory
  :group 'ivy-yarn)

(defcustom ivy-yarn-use-nvm nil
  "Whether to prepend nvm-use to command if .nvmrc exists."
  :type 'boolean
  :group 'ivy-yarn)

(defcustom ivy-yarn-use-vterm t
  "Whether to execute commands in vterm, if installed."
  :type 'boolean
  :group 'ivy-yarn)

(defvar ivy-yarn-json-hash (make-hash-table :test 'equal))

(defun ivy-yarn-read-json (file &optional json-type)
  "Read the JSON object in FILE, return object converted to JSON-TYPE.
JSON-TYPE must be one of `alist', `plist', or `hash-table'."
  (condition-case nil
      (let* ((json-object-type (or json-type 'plist))
             (cache (gethash (format "%s:%s" file json-object-type)
                             ivy-yarn-json-hash))
             (cache-tick (and cache (plist-get cache :tick)))
             (tick (file-attribute-modification-time (file-attributes
                                                      file
                                                      'string)))
             (content-json))
        (when (or (null cache)
                  (not (equal tick cache-tick)))
          (setq content-json
                (with-temp-buffer
                  (insert-file-contents file)
                  (json-read-from-string
                   (buffer-substring-no-properties
                    (point-min)
                    (point-max)))))
          (setq cache (list :tick tick
                            :json content-json))
          (puthash file cache ivy-yarn-json-hash))
        (plist-get cache :json))
    (error (message "Could't read %s as json."
                    file))))

;;;###autoload
(defun ivy-yarn-read-multy (prompt collection)
  "Read COLLECTION with PROMPT and return list with selected candidates."
  (interactive)
  (let ((marked)
        (item))
    (setq item (ivy-read prompt
                         collection
                         :caller 'ivy-yarn-read-multy
                         :action (lambda (it) it)
                         :multi-action (lambda (cands)
                                         (setq marked cands))))
    (or marked (list item))))

(defvar ivy-yarn-history-dependencies nil)

(defun ivy-yarn-search-npm-repo (str)
  "Search STR with npm search."
  (or
   (ivy-more-chars)
   (progn
     (counsel--async-command (concat "npm search --parseable " str))
     '("" "working..."))))

;;;###autoload
(defun ivy-yarn-read-new-dependency (&optional initial-input)
  "Call the \"npm search\" shell command.
INITIAL-INPUT can be given as the initial minibuffer input."
  (interactive)
  (let ((result)
        (dependencies))
    (setq result
          (ivy-read
           "Repo:\s" #'ivy-yarn-search-npm-repo
           :initial-input initial-input
           :dynamic-collection t
           :history 'ivy-yarn-history-dependencies
           :multi-action (lambda (marked)
                           (setq dependencies
                                 (append marked dependencies)))
           :action (lambda (d) d)
           :unwind #'counsel-delete-process
           :caller 'ivy-yarn-read-new-dependency))
    (if dependencies
        (string-join (mapcar (lambda (d) (car (split-string d)))
                             dependencies)
                     "\s")
      (car (split-string result nil t)))))

(defun ivy-yarn-get-package-versions (package)
  "Exec \"yarn info\" for PACKAGE and return list of available versions."
  (let ((str (string-trim
              (shell-command-to-string (string-join
                                        `("yarn info" ,package)
                                        "\s"))))
        (versions))
    (with-temp-buffer
      (erase-buffer)
      (insert str)
      (goto-char (point-min))
      (re-search-forward "\\_<\\(versions\\)\\_>" nil t 1)
      (skip-chars-forward ":\s\t\f\n")
      (when (looking-at "\\[")
        (let ((beg (point))
              (end))
          (forward-sexp 1)
          (setq end (point))
          (setq versions
                (mapcar
                 (lambda (it) (string-join (split-string it "[,'\"]" t) ""))
                 (split-string
                  (buffer-substring-no-properties
                   (1+ beg)
                   (1- end))
                  nil t))))))
    (reverse versions)))

(defun ivy-yarn-list-symlinked-dirs-recursively (directory)
  "Return list of absolute symlinked directories in DIRECTORY."
  (setq directory (file-name-as-directory directory))
  (let ((result nil)
        (tramp-mode (and tramp-mode (file-remote-p
                                     (expand-file-name directory)))))
    (dolist (file (delete ".." (delete "." (directory-files directory))))
      (when-let* ((full-file (unless (member file '(".git" ".github"))
                              (concat directory file))))
        (when (file-directory-p full-file)
          (if (file-symlink-p full-file)
              (push full-file result)
            (when-let* ((dirs (ivy-yarn-list-symlinked-dirs-recursively
                              full-file)))
              (setq result (nconc result dirs)))))))
    result))

(defun ivy-yarn-links-in-dir (directory)
  "Return non-absolute symlinked packages in DIRECTORY."
  (let ((len (1+ (length directory))))
    (mapcar (lambda (it)
              (substring it len))
            (ivy-yarn-list-symlinked-dirs-recursively directory))))

(defun ivy-yarn-project-linked-dependencies ()
  "Return list of linked packages in the current project.
Only those packages includes that listed in package.json."
  (let ((node-modules (ivy-yarn-get-node-modules-path))
        (dependencies (ivy-yarn-get-current-dependencies))
        (links))
    (dolist (package-name dependencies)
      (when-let* ((dir (ivy-yarn-expand-when-exists package-name node-modules)))
        (when (file-symlink-p dir)
          (push package-name links))))
    links))

(defun ivy-yarn-find-global-links ()
  "Return linked packages in `ivy-yarn-global-config-directory' directory."
  (when-let* ((links-dir
              (when ivy-yarn-global-config-directory
                (ivy-yarn-expand-when-exists
                 "link"
                 ivy-yarn-global-config-directory))))
    (ivy-yarn-links-in-dir links-dir)))

(defun ivy-yarn-get-node-modules-path ()
  "Look up directory hierarchy for directory containing node_modules.
Return absolute path to node_modules or nil."
  (when-let* ((project-root (locate-dominating-file
                            default-directory
                            "node_modules")))
    (expand-file-name "node_modules" project-root)))

(defun ivy-yarn-get-project-root ()
  "Look up directory hierarchy for directory containing package.json.
Return full path of containing directory or nil."
  (locate-dominating-file
   default-directory
   "package.json"))

(defun ivy-yarn-unlink ()
  "Unlink and reinstall linked package in current project."
  (let ((current-project-name (ivy-yarn-read-from-package-json 'name))
        (global-links (ivy-yarn-find-global-links))
        (linked-packages (ivy-yarn-project-linked-dependencies))
        (choice))
    (setq choice (completing-read "Unlink:\s"
                                  (if (member current-project-name global-links)
                                      (nconc (list "self") linked-packages)
                                    linked-packages)))
    (pcase choice
      ("self" "")
      ((pred stringp) (concat choice " && yarn install --force"))
      (_ " && yarn install --force"))))

(defun ivy-yarn-get-package-json-path ()
  "Look up directory hierarchy for directory containing package.json.
Return absolute path to package.json or nil."
  (when-let* ((project-root (ivy-yarn-get-project-root)))
    (expand-file-name "package.json"
                      project-root)))

(defun ivy-yarn-read-from-package-json (key)
  "Return value of KEY from package.json."
  (let* ((package-json-path (ivy-yarn-get-package-json-path))
         (package-json (ivy-yarn-read-json package-json-path 'alist)))
    (alist-get key package-json)))

(defun ivy-yarn-jest-installed-p ()
  "Return t if jest can be run in the current project."
  (when-let* ((node-modules (ivy-yarn-get-node-modules-path))
              (package-json-path (ivy-yarn-get-package-json-path))
              (package-json (ivy-yarn-read-json package-json-path 'alist))
              (scripts (alist-get 'scripts package-json)))
    (and (alist-get 'test scripts)
         (not (null (member "jest" (directory-files node-modules)))))))

(defun ivy-yarn-jest-current-file-cmd ()
  "Return string with jest command for testing current file."
  (when-let* ((file (or buffer-file-name default-directory))
             (project-root (ivy-yarn-get-project-root)))
    (let ((path-pattern (shell-quote-argument
                         (replace-regexp-in-string
                          (regexp-quote project-root)
                          ""
                          (abbreviate-file-name file)))))
      (ivy-yarn-add-props
       (string-join (list
                     "test"
                     "--testPathPattern"
                     path-pattern)
                    "\s")
       :description "Run jest on current file"))))

(defun ivy-yarn-get-current-scripts ()
  "Get current project scripts from package.json.
Return list of strings, propertized with :description."
  (let* ((package-json-path (ivy-yarn-get-package-json-path))
         (package-json (ivy-yarn-read-json package-json-path 'alist))
         (scripts (alist-get 'scripts package-json)))
    (mapcar (lambda (it)
              (ivy-yarn-add-props
               (format "%s" (car it))
               :description (format "%s:\s%s" (car it) (cdr it))))
            scripts)))

(defun ivy-yarn-stringify (x)
  "Convert X to string effeciently.
X can be any object."
  (cond
   ((stringp x)
    x)
   ((symbolp x)
    (symbol-name x))
   ((integerp x)
    (number-to-string x))
   ((floatp x)
    (number-to-string x))
   (t (format "%s" x))))

(defun ivy-yarn-add-props (string &rest properties)
  "Propertize STRING with PROPERTIES."
  (setq string (ivy-yarn-stringify string))
  (let* ((result (list 'head))
         (last result))
    (while properties
      (let* ((key (pop properties))
             (val (pop properties))
             (new (and val (list key val))))
        (when new
          (setcdr last new)
          (setq last (cdr new)))))
    (apply #'propertize string (cdr result))))

(defun ivy-yarn-get-current-dependencies ()
  "Get current project dependencies from package.json.
Return list of strings. Each of string propertized with props
:type (dependencies, devDependencies, optionalDependencies
 or peerDependencies), :version - package version,and
:description (type and version)."
  (let* ((package-json-path (ivy-yarn-get-package-json-path))
         (package-json (ivy-yarn-read-json package-json-path 'alist))
         (all-items))
    (dolist (type (list 'dependencies
                        'devDependencies
                        'peerDependencies
                        'optionalDependencies))
      (let ((depths (alist-get type package-json)))
        (setq depths
              (mapcar (lambda (it)
                        (ivy-yarn-add-props
                         (ivy-yarn-stringify (car it))
                         :type type
                         :description (format "@%s - %s" (cdr it) type)
                         :version (cdr it)))
                      depths))
        (setq all-items (append all-items depths))))
    all-items))

(defun ivy-yarn-confirm-package-version (package &optional prompt)
  "Confirm PACKAGE version with PROMPT."
  (let ((version))
    (unless (string-match-p "[a-z-]+@" package)
      (when-let* ((versions (seq-reduce
                            (lambda (acc it)
                              (let ((prefixes '("^" ">="))
                                    (l `(,it)))
                                (dolist (prefix prefixes)
                                  (push (format "%s%s" prefix it) l))
                                (setq acc (append acc l))))
                            (ivy-yarn-get-package-versions package)
                            '())))
        (setq version
              (completing-read
               (or prompt (format "%s " package))
               (push "latest" versions)))))
    (if version
        (format "%s@%s" package version)
      package)))

(defun ivy-yarn-get-prop (item property)
  "Get PROPERTY at ITEM.
ITEM can be propertized string or plist."
  (if (stringp item)
      (get-text-property 0 property item)
    (when (listp item)
      (plist-get item property))))

;;;###autoload
(defun ivy-yarn-read-installed-package ()
  "Read installed package."
  (interactive)
  (let* ((marked)
         (item (ivy-read "search: " (ivy-yarn-get-current-dependencies)
                         :caller 'ivy-yarn-read-installed-package
                         :multi-action (lambda (children)
                                         (setq marked children))
                         :action (lambda (it) it)))
         (results))
    (dolist (package (or marked (list item)))
      (push (ivy-yarn-confirm-package-version
             package
             (format "%s@%s" package
                     (ivy-yarn-get-prop package :version)))
            results))
    (mapconcat #'string-trim results "\s")))

(defun ivy-yarn-parse-command-args (command)
  "Parse shell COMMAND args."
  (setq command (if (> (length (split-string command nil t)) 1)
                    command
                  (concat command " --help")))
  (when-let* ((help-string command))
    (with-temp-buffer
      (erase-buffer)
      (insert help-string)
      (let ((items))
        (while (re-search-backward "--[a-z-]+" nil t 1)
          (let ((flag (match-string-no-properties 0))
                (end (match-end 0))
                (description)
                (line-end))
            (setq line-end (save-excursion (end-of-line) (point)))
            (setq description
                  (string-trim
                   (buffer-substring-no-properties end line-end)))
            (setq flag (and flag (ivy-yarn-add-props
                                  flag :description description)))
            (push flag items)))
        (delq nil (reverse items))))))

;;;###autoload
(defun ivy-yarn-add-read-dependency (&optional initial-input)
  "Read dependency to install.
INITIAL-INPUT can be given as the initial minibuffer input."
  (interactive)
  (let ((result (if initial-input
                    (read-string "yarn add\s" (or initial-input ""))
                  (ivy-yarn-read-new-dependency)))
        (parts))
    (setq result
          (string-join (seq-drop-while
                        (lambda (it) (member it '("yarn" "add")))
                        (split-string result "[\s\f\t\n\r\v\"]+" t))
                       "\s"))
    (setq result (split-string result nil t))
    (if (yes-or-no-p "Check versions?")
        (dolist (it result)
          (unless (string-match-p "[a-z-]+@" it)
            (when-let* ((versions (ivy-yarn-get-package-versions it))
                        (version (completing-read
                                  (format "%s@^" it) versions)))
              (setq it (if (and version
                                (string-match-p "[0-9]" version))
                           (format "%s@^%s" it version)
                         it)))
            (push it parts)))
      (setq parts result))
    (string-join (reverse parts)  "\s")))

;;;###autoload
(defun ivy-yarn-add ()
  "Add new dependency."
  (interactive)
  (let* ((dependency (ivy-yarn-add-read-dependency))
         (flags (string-join
                 (ivy-yarn-read-multy
                  "Options:\s"
                  (seq-uniq
                   (append
                    (list "none" "--dev" "--peer" "file:")
                    (ivy-yarn-parse-command-args
                     (string-join
                      (list "yarn" "help" "add") "\s")))))
                 "\s")))
    (if (and flags (string= flags "none"))
        (concat dependency)
      (string-trim (concat dependency "\s" (or flags ""))))))

(defvar ivy-yarn-all-commands
  '("add"
    "audit"
    "autoclean"
    "bin"
    "cache"
    "check"
    "config"
    "create"
    "dedupe"
    "generate-lock-entry"
    "global"
    "help"
    "import"
    "info"
    "init"
    "install"
    "licenses"
    "link"
    "list"
    "lockfile"
    "login"
    "logout"
    "outdated"
    "owner"
    "pack"
    "policies"
    "prune"
    "publish"
    "remove"
    "run"
    "self-update"
    "tag"
    "team"
    "test"
    "unlink"
    "upgrade"
    "upgrade-interactive"
    "version"
    "versions"
    "why"
    "workspace"
    "workspaces"))

(defvar ivy-yarn-completions-commands
  '(("add" . ivy-yarn-add)
    ("global" . (("add" . ivy-yarn-add)
                 ("remove" . ivy-yarn-get-current-dependencies)
                 ("upgrade" . ivy-yarn-read-installed-package)))
    ("run" . ivy-yarn-get-current-scripts)
    ("link" . ivy-yarn-find-global-links)
    ("unlink" . ivy-yarn-unlink)
    ("remove" . ivy-yarn-get-current-dependencies)
    ("upgrade" . ivy-yarn-read-installed-package)
    ("list" . (("--depth" . ("1" "2" "3" "4" " "))
               ("--pattern" . ivy-yarn-get-current-dependencies)
               ("" . ("1" "2" "3" "4" " "))))
    ("outdated" . ivy-yarn-get-current-dependencies)
    ("config" . ("list"))
    ("cache" . (("list" . '("pattern")) "dir" "clean"))
    ("info" . ivy-yarn-get-current-dependencies)
    ("audit" . (("--level" . ("info" "low" "moderate" "high" "critical"))
                ("--groups")))
    ("config" . (("current" "list" "get" "set" "delete")))
    ("version")
    ("versions")
    ("why" . ivy-yarn-get-current-dependencies)))

(defun ivy-yarn-get-choices ()
  "Return alist of commands, and scripts with handlers."
  (let ((def-commands
         (mapcar
          (lambda (it)
            `(,it .
                        (lambda ()
                          (let ((marked-items)
                                (val)
                                (choices
                                 (ivy-yarn-parse-command-args
                                  (string-join
                                   (list
                                    "yarn" ,it "--help")
                                   "\s"))))
                            (setq val
                                  (ivy-yarn-read-multy
                                   (format "yarn %s" ,it)
                                   choices))
                            (if (listp val)
                                (string-join val "\s")
                              val)))))
          (seq-difference
           ivy-yarn-all-commands
           (mapcar #'car ivy-yarn-completions-commands))))
        (scripts (ivy-yarn-get-current-scripts)))
    (if (ivy-yarn-jest-installed-p)
        (append scripts (list (ivy-yarn-jest-current-file-cmd))
                ivy-yarn-completions-commands def-commands)
      (append scripts ivy-yarn-completions-commands def-commands))))

(defun ivy-yarn-plist-pick (keywords pl)
  "Pick KEYWORDS from PL."
  (let ((result)
        (keyword))
    (while (setq keyword (pop keywords))
      (when-let* ((value (plist-get pl keyword)))
        (unless (null value)
          (setq result (append result (list keyword value))))))
    result))

;;;###autoload
(defun ivy-yarn-complete-alist (&optional alist &rest plist)
  "Complete with ALIST.
PLIST is additional props passed to `ivy-read'."
  (interactive)
  (let ((current)
        (result)
        (count 0))
    (if (functionp alist)
        (funcall alist)
      (while (and alist
                  (not (keywordp alist))
                  (or (listp alist)
                      (vectorp alist))
                  (not (functionp alist)))
        (when (vectorp alist)
          (setq alist
                (seq-map-indexed (lambda (it i)
                                   (cons (format "%s" i)
                                         it))
                                 (append alist nil))))
        (setq this-command 'ivy-yarn-complete-alist)
        (setq count (1+ count))
        (let ((prompt (format "%s %s\s "
                              (if (= 1 (% count 2))
                                  (string-join
                                   (reverse result) "\s")
                                (string-join
                                 (or result "") "\s"))
                              (or (and plist (plist-get
                                              plist :prompt))
                                  ""))))
          (let* ((marked)
                 (args (append
                        (list prompt alist
                              :action (lambda (str)
                                        str)
                              :multi-action (lambda (children)
                                              (setq marked children)))
                        (ivy-yarn-plist-pick
                         '(:predicate :require-match :initial-input
                                      :history :preselect
                                      :def :keymap :update-fn :sort
                                      :unwind :re-builder :matcher
                                      :dynamic-collection
                                      :extra-props
                                      :caller)
                         plist))))
            (setq current (apply #'ivy-read args))
            (if marked
                (progn (dolist (it marked)
                         (let ((cell (assoc it alist)))
                           (if-let* ((rest (cdr cell)))
                               (progn (setq rest
                                            (cond
                                             ((functionp rest)
                                              (funcall rest))
                                             ((listp rest)
                                              (apply
                                               #'ivy-yarn-complete-alist
                                               (list rest (append
                                                           plist
                                                           `(:prompt ,it)))))
                                             ((stringp rest)
                                              rest)))
                                      (push (cons it rest)
                                            result))
                             (push it result))))
                       (setq current nil)
                       (setq alist nil))
              (push current result)
              (setq alist (cdr-safe (assoc current alist)))))
          (when (functionp alist)
            (let ((value (funcall alist)))
              (if (and (listp value)
                       (not (listp (car value))))
                  (setq alist value)
                (push value result)
                (setq alist nil))))
          alist))
      (setq result
            (reverse result)))))

(defun ivy-yarn-nvm-node-path (&optional project)
  "If PROJECT can use nvm, prepend to COMMAND nvm use."
  (when-let* ((source (and (ivy-yarn-expand-when-exists
                           ".nvmrc"
                           (or project
                               (ivy-yarn-get-project-root)))
                          (ivy-yarn-nvm-path))))
    (let ((cmd (string-join (list "source" source
                                  "&&"
                                  "nvm use" "&> /dev/null"
                                  "&&"
                                  "nvm which")
                            "\s")))
      (car (reverse (split-string
                     (shell-command-to-string cmd)
                     "[\n]"
                     t))))))

(defun ivy-yarn-get-nvm-node-version (&optional project)
  "Return string with version in PROJECT .nvmrc file."
  (when-let* ((nvmrc (ivy-yarn-expand-when-exists
                     ".nvmrc" (or project
                                  (ivy-yarn-get-project-root)))))
    (with-temp-buffer (insert-file-contents nvmrc)
                      (string-trim
                       (buffer-substring-no-properties (point-min)
                                                       (point-max))))))

(defun ivy-yarn-exec-with-args (command &rest args)
  "Run a shell COMMAND with ARGS."
  (let ((cmdline (mapconcat (lambda (it)
                              (if (string-match-p "\s\t\n" it)
                                  (shell-quote-argument it)
                                it))
                            (append (list command)
                                    (flatten-list args))
                            "\s")))
    (with-temp-buffer
      (shell-command cmdline (current-buffer))
      (let ((output (string-trim (buffer-substring-no-properties (point-min)
                                                                 (point-max)))))
        (unless (string-empty-p output)
          output)))))

(defun ivy-yarn-nvm-command (command &rest args)
  "Run nvm COMMAND with ARGS if nvm directory exists."
  (when-let* ((nvm-path (ivy-yarn-nvm-path)))
    (ivy-yarn-exec-with-args "source" nvm-path
                             "&&" "nvm" command args)))

(defun ivy-yarn-nvm-installed-node-versions ()
  "Return list of installed with nvm node versions."
  (let ((versions (split-string (or (ivy-yarn-nvm-command "ls") "")
                                "[\r\f\n]" t)))
    versions))

(defun ivy-yarn-ensure-nvm-node-installed (&optional project force)
  "Install node version specified in nvmrc file of PROJECT.
If FORCE is non nil, install it even if it is installed."
  (when-let* ((nvm-node-version (ivy-yarn-get-nvm-node-version project)))
    (let ((regex (regexp-quote nvm-node-version))
          (versions (ivy-yarn-nvm-installed-node-versions)))
      (when (and
             (or force
                 (not (seq-find (apply-partially #'string-match-p regex)
                                versions)))
             (yes-or-no-p
              (format
               "This project requires node %s, which is not installed. Install?"
               nvm-node-version)))
        (ivy-yarn-nvm-command "install"
                              nvm-node-version
                              "--reinstall-packages-from=current")))))

(defun ivy-yarn-ensure-nvm-use (command &optional project)
  "If PROJECT can use nvm, prepend to COMMAND nvm use."
  (if-let* ((nvm-path (and
                      ivy-yarn-use-nvm
                      (ivy-yarn-expand-when-exists
                       ".nvmrc" (or project
                                    (ivy-yarn-get-project-root)))
                      (ivy-yarn-nvm-path))))
      (progn (ivy-yarn-ensure-nvm-node-installed project)
             (concat "source " nvm-path " && nvm use && " command))
    command))

(defun ivy-yarn-normalize-result (strings)
  "Normalize STRINGS by trimming and prepending prefix to them."
  (unless (listp strings)
    (setq strings (split-string strings t)))
  (let* ((command (string-trim
                   (string-join (seq-remove #'string-blank-p
                                            (mapcar
                                             #'string-trim (delq nil strings)))
                                "\s"))))
    (string-trim (ivy-yarn-ensure-nvm-use
                  (if (member
                       (car (split-string command))
                       ivy-yarn-all-commands)
                      (concat "yarn " command)
                    (concat "yarn && yarn " command))))))

(defun ivy-yarn-expand-when-exists (filename &optional directory)
  "Expand FILENAME to DIRECTORY and return result if exists."
  (when-let* ((file (expand-file-name filename directory)))
    (when (file-exists-p file)
      file)))

(defun ivy-yarn-nvm-path ()
  "Return path to NVM_DIR if exists."
  (when-let* ((nvm-dir (or (getenv "NVM_DIR")
                           (when (file-exists-p "~/.nvm/")
                             "~/.nvm/")))
              (file (expand-file-name "nvm.sh" nvm-dir)))
    (when (file-exists-p file)
      file)))

(defun ivy-yarn-nvm-installed-p ()
  "Return t if NVM_DIR exists."
  (not (null (ivy-yarn-nvm-path))))

(defun ivy-yarn-run-in-vterm (project command)
  "Run COMMAND in PROJECT in vterm."
  (let ((buffer (format "*%s*" (concat "vterm-ivy-yarn-"
                                       (replace-regexp-in-string
                                        "~/"
                                        ""
                                        (abbreviate-file-name project))))))
    (when (buffer-live-p (get-buffer buffer))
      (switch-to-buffer (get-buffer buffer))
      (vterm--invalidate)
      (kill-buffer (get-buffer buffer)))
    (let ((default-directory project))
      (vterm buffer)
      (run-at-time
       0.5 nil 'vterm-send-string command)
      (run-at-time 1 nil 'vterm-send-return))))

(defun ivy-yarn-run-async-shell-command (project command)
  "Run COMMAND in PROJECT with `async-shell-command'."
  (let ((buffer (format "*%s*" (concat "ivy-yarn-"
                                       (abbreviate-file-name project))))
        (default-directory project))
    (when (buffer-live-p (get-buffer buffer))
      (kill-buffer buffer))
    (async-shell-command command buffer buffer)))

;;;###autoload
(defun ivy-yarn ()
  "Read and execute yarn command or project script.

If project directory contains .nvmrc file,
`ivy-yarn-use-nvm' is non-nil and nvm installed,
execute nvm-use before running command.

If vterm is installed and `ivy-yarn-use-vterm' is non-nil,
run command in `vterm', otherwise with `async-shell-command'."
  (interactive)
  (when-let* ((package-json (ivy-yarn-get-package-json-path))
              (project (file-name-directory package-json))
              (parts (ivy-yarn-complete-alist
                      (ivy-yarn-get-choices)))
              (command (read-string "Run\s" (ivy-yarn-normalize-result parts))))
    (if (and ivy-yarn-use-vterm
             (require 'vterm nil t))
        (ivy-yarn-run-in-vterm project command)
      (ivy-yarn-run-async-shell-command project command))))

(defun ivy-yarn-complete-display-fn (item)
  "Transform ITEM for displaying while `ivy-read'."
  (if-let* ((version (ivy-yarn-get-prop item :version)))
      (concat item "@"
              (propertize
               (or (string-join
                    (delete nil (list version (format "%s"
                                                      (ivy-yarn-get-prop
                                                       item :type))))
                    "\s")
                   "")
               'face
               'font-lock-builtin-face))
    (and item
         (or
          (and (ivy-yarn-get-prop item :description)
               (concat item "\s"
                       (propertize
                        (ivy-yarn-get-prop item :description)
                        'face
                        'font-lock-builtin-face)))
          item))))

(ivy-configure 'ivy-yarn-complete-alist
  :display-transformer-fn
  'ivy-yarn-complete-display-fn)

(ivy-configure 'ivy-yarn-read-multy
  :display-transformer-fn
  'ivy-yarn-complete-display-fn)

(ivy-configure 'ivy-yarn-read-installed-package
  :display-transformer-fn
  'ivy-yarn-complete-display-fn)

(provide 'ivy-yarn)
;;; ivy-yarn.el ends here