;;; iit-lib.el --- Run Icinga 2 clusters in podman containers for ERT tests -*- lexical-binding: t -*-

;; SPDX-FileCopyrightText: 2026 Icinga GmbH <https://icinga.com>
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Infrastructure for integration tests that run against real Icinga 2 clusters.
;;
;; The image is built from an Icinga 2 source tree (`iit-icinga2-source') once per run.  Each
;; `iit-with-cluster' form then sets up a complete cluster from a declarative spec: it generates
;; a CA and node certificates, the configuration for every node, a podman network and one container
;; per node.  Everything is removed again when the form exits.
;;
;; Only rootless podman is supported.  Containers run as root with podman's default user namespace,
;; where container root is the user running the tests, and Icinga 2 is told to stay root instead of
;; switching to the icinga user.  So every file the containers write into bind mounts belongs to the
;; user running the tests and can be removed without `podman unshare'.
;;
;; Mapping the icinga user with `--userns=keep-id:uid=...' would be the more obvious choice, but any
;; non-default mapping makes podman create a chowned copy of the image.  On overlay over ZFS that
;; copy can hang in the kernel, in an unkillable process that blocks podman until the next reboot.
;;
;; Per node, the following host directories (below `iit-node-dir') are bind-mounted:
;;
;;   etc/    -> /etc/icinga2              (generated config, see `iit--node-config-files')
;;   certs/  -> /var/lib/icinga2/certs
;;   log/    -> /var/log/icinga2          (read directly by `iit-expect-log')
;;
;; The rest of /data, including /var/lib/icinga2/api, lives in the container's anonymous volume.
;; It can be accessed with TRAMP through `iit-node-file', or mounted to the host with the :mounts
;; node option.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'json)
(require 'subr-x)
(require 'tramp)
(require 'url-util)

(defconst iit-directory (file-name-directory (or load-file-name buffer-file-name))
  "Directory containing the test files.")

;;;; Settings

(defvar iit-podman-program "podman"
  "The podman executable.")

(defvar iit-icinga2-source (or (getenv "ICINGA2_SOURCE")
                                 (expand-file-name "../icinga2" iit-directory))
  "Icinga 2 source tree to build the image from.")

(defvar iit-image "localhost/iit/icinga2:latest"
  "Name of the Icinga 2 image to build and test.")

(defvar iit-build-image t
  "Whether to build `iit-image' before running the first cluster test.
When nil, the image has to exist already.")

(defvar iit-build-args '("ICINGA2_BUILD_TESTING=OFF")
  "Build arguments passed to `podman build'.")

(defvar iit-keep-on-failure nil
  "When non-nil, don't remove the containers of a failed cluster test.
They are still removed at the start of the next cluster test, so this is mostly
useful when running a single test.")

(defvar iit-prefix "iit-"
  "Prefix for the names of containers and networks.")

(defvar iit-label "iit"
  "Label attached to all containers and networks, used to clean up leftovers.")

(defvar iit-log-timeout 60
  "Default number of seconds `iit-expect-log' waits for a pattern.")

(defvar iit-stop-timeout 30
  "Seconds podman waits for Icinga 2 to shut down before killing it.")

(defvar iit-ready-regexp (regexp-quote "/ApiListener: 'api' started.")
  "Log pattern indicating that a node has finished starting up.")

(defvar iit-api-user "root")
(defvar iit-api-password "icinga")

(defvar iit-default-features '(api mainlog checker)
  "Features enabled on a node unless it specifies :features.")

(defconst iit-feature-configs
  `((api (object ApiListener "api" (accept_config t) (accept_commands t)))
    (mainlog (object FileLogger "main-log"
                     (severity "information")
                     (path (raw "LogDir + \"/icinga2.log\""))))
    (debuglog (object FileLogger "debug-file"
                      (severity "debug")
                      (path (raw "LogDir + \"/debug.log\""))))
    (checker (object CheckerComponent "checker"))
    (notification (object NotificationComponent "notification")))
  "Config for the features that can be listed in a node's :features.")

;;;; Rendering Icinga 2 config from s-expressions

;; Statements:
;;
;;   (object TYPE NAME BODY...)          object TYPE "NAME" { ... }
;;   (template TYPE NAME BODY...)        template TYPE "NAME" { ... }
;;   (apply TYPE NAME [:to TYPE] BODY...) apply TYPE "NAME" [to TYPE] { ... }
;;   (const NAME VALUE)                  const NAME = VALUE
;;   (include "file") / (include <itl>)  include "file" / include <itl>
;;   (include-recursive "dir")           include_recursive "dir"
;;   (raw "text") or "text"              inserted verbatim
;;
;; Body items:
;;
;;   (ATTR VALUE)                        ATTR = VALUE
;;   (ATTR += VALUE)                     also -=
;;   (import "template")                 import "template"
;;   (assign "EXPR") / (ignore "EXPR")   assign where EXPR / ignore where EXPR
;;   (raw "text")                        inserted verbatim
;;
;; Values:
;;
;;   "string"                            quoted and escaped
;;   42, 1.5                             numbers
;;   t / nil or :false / :null           true / false / null
;;   symbol                              verbatim: identifiers, durations like 5m, LogDir
;;   [a b c]                             [ a, b, c ]
;;   (dict (KEY VALUE)...)               { KEY = VALUE ... }, KEY is a symbol or a string
;;   (raw "expr")                        verbatim expression

(defun iit-config-string (string)
  "Return STRING as a quoted Icinga 2 string literal."
  (concat "\""
          (replace-regexp-in-string
           "[\"\\\n\t\r]"
           (lambda (c)
             (pcase c
               ("\"" "\\\"") ("\\" "\\\\") ("\n" "\\n") ("\t" "\\t") ("\r" "\\r")))
           string t t)
          "\""))

(defun iit-config--indent (level)
  (make-string (* 2 level) ?\s))

(defun iit-config--value (value level)
  "Render VALUE as an Icinga 2 expression nested at indentation LEVEL."
  (pcase value
    ('t "true")
    ((or 'nil :false) "false")
    (:null "null")
    ((pred stringp) (iit-config-string value))
    ((pred numberp) (number-to-string value))
    ((pred keywordp) (error "Unknown config keyword %S" value))
    ((pred symbolp) (symbol-name value))
    ((pred vectorp)
     (if (zerop (length value))
         "[]"
       (concat "[ "
               (mapconcat (lambda (v) (iit-config--value v level)) value ", ")
               " ]")))
    (`(raw ,(and expr (pred stringp))) expr)
    (`(dict . ,entries)
     (if (null entries)
         "{}"
       (concat "{\n"
               (mapconcat
                (lambda (entry)
                  (pcase entry
                    (`(,key ,v)
                     (concat (iit-config--indent (1+ level))
                             (if (stringp key) (iit-config-string key) (symbol-name key))
                             " = "
                             (iit-config--value v (1+ level))))
                    (_ (error "Invalid dict entry %S" entry))))
                entries "\n")
               "\n" (iit-config--indent level) "}")))
    (_ (error "Can't render %S as a config value" value))))

(defun iit-config--body-item (item level)
  "Render ITEM of an object body at indentation LEVEL."
  (concat
   (iit-config--indent level)
   (pcase item
     (`(import ,name) (concat "import " (iit-config-string name)))
     (`(assign ,(and expr (pred stringp))) (concat "assign where " expr))
     (`(ignore ,(and expr (pred stringp))) (concat "ignore where " expr))
     (`(raw ,(and text (pred stringp))) text)
     (`(,(and attr (pred symbolp)) ,(and op (or '+= '-=)) ,value)
      (format "%s %s %s" attr op (iit-config--value value level)))
     (`(,(and attr (pred symbolp)) ,value)
      (format "%s = %s" attr (iit-config--value value level)))
     (_ (error "Invalid object body item %S" item)))))

(defun iit-config--statement (form)
  "Render the top-level config statement FORM."
  (pcase form
    ((pred stringp) form)
    (`(,(and kind (or 'object 'template 'apply)) ,type ,name . ,body)
     (let ((to nil))
       (when (eq (car body) :to)
         (setq to (cadr body)
               body (cddr body)))
       (format "%s %s %s%s {\n%s}"
               kind type (iit-config-string (format "%s" name))
               (if to (format " to %s" to) "")
               (mapconcat (lambda (item) (concat (iit-config--body-item item 1) "\n"))
                          body ""))))
    (`(const ,name ,value) (format "const %s = %s" name (iit-config--value value 0)))
    (`(include ,(and file (pred stringp))) (concat "include " (iit-config-string file)))
    (`(include ,(and file (pred symbolp))) (format "include %s" file))
    (`(include-recursive ,dir) (concat "include_recursive " (iit-config-string dir)))
    (`(raw ,(and text (pred stringp))) text)
    (_ (error "Unknown config statement %S" form))))

(defun iit-config-render (&rest forms)
  "Render the config statements FORMS to a string."
  (mapconcat (lambda (form) (concat (iit-config--statement form) "\n")) forms "\n"))

;;;; Podman

(defun iit--podman-args (args)
  (mapcar (lambda (a) (if (stringp a) a (format "%s" a))) (flatten-tree args)))

(defvar iit-verbose nil
  "When non-nil, log every podman command before running it.")

(defun iit--read-file (file)
  (with-temp-buffer
    (insert-file-contents file)
    (string-trim (buffer-string))))

(defun iit--podman-run (args)
  "Run podman with ARGS and return (STATUS STDOUT STDERR).
Output goes to temporary files rather than a pipe: with a pipe, `call-process'
waits until every process holding it has exited, which never happens when
podman leaves helper processes such as conmon or pasta running in the
background."
  (let ((args (iit--podman-args args))
        (stdout-file (make-temp-file "iit-stdout"))
        (stderr-file (make-temp-file "iit-stderr")))
    (when iit-verbose
      (message "+ podman %s" (string-join args " ")))
    (unwind-protect
        (let ((status (apply #'call-process iit-podman-program nil
                             (list (list :file stdout-file) stderr-file) nil args)))
          (list status (iit--read-file stdout-file) (iit--read-file stderr-file)))
      (delete-file stdout-file)
      (delete-file stderr-file))))

(defun iit-podman (&rest args)
  "Run podman with ARGS and return its trimmed output; signal an error on failure.
ARGS are flattened and nil elements are dropped, so optional arguments can
be passed as lists."
  (pcase-let ((`(,status ,out ,err) (iit--podman-run args)))
    (unless (eql status 0)
      (error "`podman %s' failed (%s): %s"
             (string-join (iit--podman-args args) " ") status err))
    out))

(defun iit-podman-ok-p (&rest args)
  "Run podman with ARGS and return non-nil if it exits successfully."
  (eql 0 (car (iit--podman-run args))))

(defun iit--podman-stream (&rest args)
  "Run podman with ARGS, passing its output through to stderr."
  (let* ((args (iit--podman-args args))
         (proc (make-process
                :name "podman"
                :command (cons iit-podman-program args)
                :connection-type 'pipe
                :noquery t
                :filter (lambda (_ output) (princ output #'external-debugging-output))
                :sentinel #'ignore)))
    (while (process-live-p proc)
      (accept-process-output proc 1))
    (accept-process-output proc 0.1)
    (unless (zerop (process-exit-status proc))
      (error "`podman %s' failed (%s)" (string-join args " ") (process-exit-status proc)))))

(defun iit--user-args ()
  "Arguments that run a container, and Icinga 2 in it, as container root.
See the commentary at the top of this file for why."
  '("--user" "0:0" "--env" "ICINGA2_USER=root" "--env" "ICINGA2_GROUP=root"))

(defvar iit--image-ready nil
  "Non-nil once `iit-image' has been built or found in this run.")

(defun iit-ensure-image ()
  "Build `iit-image' once per run, or check that it exists."
  (unless iit--image-ready
    (if iit-build-image
        (let* ((source (file-name-as-directory (expand-file-name iit-icinga2-source)))
               (containerfile (expand-file-name "Containerfile" source)))
          (unless (file-exists-p containerfile)
            (error "No Containerfile in %s; set ICINGA2_SOURCE or pass --source" source))
          (message "Building %s from %s" iit-image source)
          (iit--podman-stream "build" "-t" iit-image "-f" containerfile
                                (mapcar (lambda (arg) (list "--build-arg" arg)) iit-build-args)
                                source))
      (unless (iit-podman-ok-p "image" "exists" iit-image)
        (error "Image %s doesn't exist; run once without --skip-build" iit-image)))
    (setq iit--image-ready t)))

(defun iit-sweep ()
  "Remove all containers and networks left over from earlier runs."
  (let ((label (concat "label=" iit-label)))
    (when-let* ((containers (split-string (iit-podman "ps" "-aq" "--filter" label) nil t)))
      (iit-podman "rm" "-f" "-v" "-t" "0" containers))
    (when-let* ((networks (split-string (iit-podman "network" "ls" "-q" "--filter" label) nil t)))
      (iit-podman "network" "rm" "-f" networks))))

;;;; Cluster specs

(cl-defstruct (iit-cluster (:constructor iit--make-cluster) (:copier nil))
  nodes global-zones (start t) dir network)

(cl-defstruct (iit-node (:constructor iit--make-node) (:copier nil))
  name zone parent port features files mounts log-throttle (log-mark 0) cached-address
  throttle-process)

(defvar iit-current-cluster nil
  "The cluster of the innermost `iit-with-cluster'.")

(defconst iit--node-keys '(:zone :parent :port :features :files :mounts :log-throttle))
(defconst iit--cluster-keys '(:global-zones :start))

(defun iit--parse-node (spec)
  "Parse the node SPEC, either NAME or (NAME . PLIST)."
  (let* ((spec (if (consp spec) spec (list spec)))
         (name (car spec))
         (plist (cdr spec)))
    (unless (and name (or (symbolp name) (stringp name)))
      (error "Invalid node name in %S" spec))
    (unless (zerop (% (length plist) 2))
      (error "Odd number of options for node %s" name))
    (cl-loop for key in plist by #'cddr
             unless (memq key iit--node-keys)
             do (error "Unknown option %S for node %s" key name))
    (let ((name (format "%s" name))
          (log-throttle (plist-get plist :log-throttle)))
      (unless (or (null log-throttle)
                  (pcase log-throttle
                    (`(,(pred stringp) ,(pred numberp)) t)))
        (error ":log-throttle of node %s must be (REGEXP SECONDS)" name))
      (iit--make-node
       :name name
       :zone (format "%s" (or (plist-get plist :zone) name))
       :parent (when-let* ((parent (plist-get plist :parent))) (format "%s" parent))
       :port (plist-get plist :port)
       :features (if (plist-member plist :features)
                     (plist-get plist :features)
                   iit-default-features)
       :files (plist-get plist :files)
       :mounts (plist-get plist :mounts)
       :log-throttle log-throttle))))

(defun iit-parse-cluster-spec (spec)
  "Parse the cluster SPEC of `iit-with-cluster' into a `iit-cluster'."
  (let (nodes options)
    (while spec
      (let ((item (pop spec)))
        (cond
         ((keywordp item)
          (unless (memq item iit--cluster-keys)
            (error "Unknown cluster option %S" item))
          (unless spec
            (error "Missing value for cluster option %S" item))
          (setq options (plist-put options item (pop spec))))
         ((or (consp item) (symbolp item) (stringp item))
          (push (iit--parse-node item) nodes))
         (t (error "Invalid cluster spec element %S" item)))))
    (unless nodes
      (error "A cluster needs at least one node"))
    (setq nodes (nreverse nodes))
    (let ((names (mapcar #'iit-node-name nodes)))
      (unless (equal names (delete-dups (copy-sequence names)))
        (error "Duplicate node names in %S" names)))
    (let ((cluster (iit--make-cluster
                    :nodes nodes
                    :global-zones (mapcar (lambda (z) (format "%s" z))
                                          (plist-get options :global-zones))
                    :start (if (plist-member options :start) (plist-get options :start) t))))
      (iit--cluster-zones cluster)    ; validates zones
      cluster)))

(defun iit--cluster-zones (cluster)
  "Return the zones of CLUSTER as a list of (NAME PARENT ENDPOINT...)."
  (let (zones)
    (dolist (node (iit-cluster-nodes cluster))
      (let* ((zone (iit-node-zone node))
             (parent (iit-node-parent node))
             (entry (assoc zone zones)))
        (if (not entry)
            (push (list zone parent (iit-node-name node)) zones)
          (when (and parent (cadr entry) (not (equal parent (cadr entry))))
            (error "Conflicting parents for zone %s: %s and %s" zone (cadr entry) parent))
          (when parent
            (setf (cadr entry) parent))
          (setf (cddr entry) (append (cddr entry) (list (iit-node-name node)))))))
    (setq zones (nreverse zones))
    (dolist (zone zones)
      (when-let* ((parent (cadr zone)))
        (unless (assoc parent zones)
          (error "Parent zone %s of zone %s has no nodes" parent (car zone)))))
    zones))

;;;; Log throttling

;; A node with :log-throttle (REGEXP SECONDS) gets an additional FileLogger that writes into a
;; FIFO in its log directory.  A reader on the host reads the FIFO line by line and sleeps for
;; SECONDS after each line that matches REGEXP.  Once the FIFO's buffer is full, writing to the
;; logger blocks, so Icinga 2 can't log matching lines faster than that.  Code that logs every
;; step, like copying a file, can only proceed at that pace then.
;;
;; The FIFO's buffer (usually 64 KiB) and the logger's stream buffer allow several hundred lines
;; to be logged without waiting first.  Other threads that log at the same time wait for the
;; logger as well.

(defconst iit--log-throttle-fifo "log/log-throttle.fifo"
  "The FIFO of a node with :log-throttle, relative to `iit-node-dir'.")

(defconst iit--log-throttle-script "\
exec 3<>\"$1\"
while IFS= read -r line <&3; do
  if [[ $line =~ $2 ]]; then
    sleep \"$3\"
  fi
done"
  "Bash script that reads the FIFO $1 and sleeps $3 seconds per line matching $2.
The FIFO is opened for writing as well, so that the script doesn't get an end of
file when Icinga 2 restarts.  Bash's read reads a FIFO byte by byte, so nothing
is read ahead.")

(defun iit--start-log-throttle (node)
  "Create the FIFO of NODE's :log-throttle and start reading it."
  (pcase-let ((`(,regexp ,seconds) (iit-node-log-throttle node))
              (fifo (iit-node-dir node iit--log-throttle-fifo)))
    (unless (eql 0 (call-process "mkfifo" nil nil nil fifo))
      (error "Could not create %s" fifo))
    (setf (iit-node-throttle-process node)
          (make-process :name (concat "iit-log-throttle-" (iit-node-name node))
                        :command (list "bash" "-c" iit--log-throttle-script "iit-log-throttle"
                                       fifo regexp (number-to-string seconds))
                        :connection-type 'pipe
                        :noquery t
                        :sentinel #'ignore))))

(defun iit--stop-log-throttle (node)
  "Stop reading the FIFO of NODE's :log-throttle."
  (when-let* ((process (iit-node-throttle-process node)))
    (delete-process process)
    (setf (iit-node-throttle-process node) nil)))

;;;; Generated config

(defun iit--zones-conf (cluster)
  "Return the zones.conf statements for CLUSTER.
Every node gets the full topology; Icinga 2 only connects to the endpoints
it is supposed to."
  (append
   (mapcar (lambda (node)
             (let ((name (iit-node-name node)))
               `(object Endpoint ,name (host ,name) (port "5665"))))
           (iit-cluster-nodes cluster))
   (mapcar (pcase-lambda (`(,zone ,parent . ,endpoints))
             `(object Zone ,zone
                      (endpoints ,(vconcat endpoints))
                      ,@(when parent `((parent ,parent)))))
           (iit--cluster-zones cluster))
   (mapcar (lambda (zone) `(object Zone ,zone (global t)))
           (iit-cluster-global-zones cluster))))

(defun iit--node-config-files (cluster node)
  "Return the files of NODE's /etc/icinga2 as an alist (PATH . CONTENT).
Files from the node's :files come last and take precedence."
  `(("icinga2.conf"
     . ,(iit-config-render
         '(include "constants.conf")
         '(include "zones.conf")
         '(include <itl>)
         '(include <plugins>)
         '(include "features-enabled/*.conf")
         '(include-recursive "conf.d")))
    ("constants.conf"
     . ,(iit-config-render
         '(const PluginDir "/usr/lib/nagios/plugins")
         '(const ManubulonPluginDir "/usr/lib/nagios/plugins")
         '(const PluginContribDir "/usr/lib/nagios/plugins")
         `(const NodeName ,(iit-node-name node))
         `(const ZoneName ,(iit-node-zone node))
         '(const TicketSalt "")))
    ("zones.conf" . ,(apply #'iit-config-render (iit--zones-conf cluster)))
    ("conf.d/api-users.conf"
     . ,(iit-config-render
         `(object ApiUser ,iit-api-user
                  (password ,iit-api-password)
                  (permissions ["*"]))))
    ,@(mapcar (lambda (feature)
                (cons (format "features-enabled/%s.conf" feature)
                      (apply #'iit-config-render
                             (or (alist-get feature iit-feature-configs)
                                 (error "Unknown feature %S for node %s"
                                        feature (iit-node-name node))))))
              (iit-node-features node))
    ,@(when (iit-node-log-throttle node)
        `(("features-enabled/log-throttle.conf"
           . ,(iit-config-render
               `(object FileLogger "log-throttle"
                        (severity "information")
                        (path (raw ,(format "LogDir + \"/%s\""
                                            (file-name-nondirectory iit--log-throttle-fifo)))))))))
    ,@(mapcar (pcase-lambda (`(,path . ,forms))
                (cons path (apply #'iit-config-render forms)))
              (iit-node-files node))))

;;;; Nodes

(defun iit--node (node)
  "Return the `iit-node' designated by NODE: a struct, symbol or string."
  (if (iit-node-p node)
      node
    (or (and iit-current-cluster
             (cl-find (format "%s" node) (iit-cluster-nodes iit-current-cluster)
                      :key #'iit-node-name :test #'equal))
        (error "No node %s in the current cluster" node))))

(defun iit--nodes (nodes)
  "Return NODES as structs, or all nodes of the current cluster if NODES is nil."
  (if nodes
      (mapcar #'iit--node nodes)
    (iit-cluster-nodes iit-current-cluster)))

(defun iit-default-node ()
  "Return the first node of the current cluster."
  (car (iit-cluster-nodes iit-current-cluster)))

(defun iit-node-container (node)
  "Return the container name of NODE."
  (concat iit-prefix (iit-node-name (iit--node node))))

(defun iit-node-dir (node &optional path)
  "Return the host directory of NODE, or PATH relative to it."
  (let ((dir (file-name-as-directory
              (expand-file-name (iit-node-name (iit--node node))
                                (iit-cluster-dir iit-current-cluster)))))
    (if path (expand-file-name path dir) dir)))

(defun iit-node-file (node path)
  "Return a TRAMP file name for PATH inside the container of NODE."
  (concat "/podman:" (iit-node-container node) ":" path))

(defun iit-node-running-p (node)
  "Return non-nil if the container of NODE is running."
  (equal (car (split-string
               (cadr (iit--podman-run
                      (list "inspect" "-f" "{{.State.Running}}" (iit-node-container node))))))
         "true"))

(defun iit-node-address (node)
  "Return HOST:PORT under which the API of NODE is reachable from the host."
  (let ((node (iit--node node)))
    (or (iit-node-cached-address node)
        (setf (iit-node-cached-address node)
              (car (split-string (iit-podman "port" (iit-node-container node) "5665/tcp")
                                 "\n" t))))))

(defun iit--forget-connections (node)
  "Drop cached connection state for NODE after its container changed state."
  (setf (iit-node-cached-address node) nil)
  (ignore-errors
    (tramp-cleanup-connection (tramp-dissect-file-name (iit-node-file node "/")) t t)))

(defun iit--log-action (action nodes)
  (message "%s %s" action (string-join (mapcar #'iit-node-name nodes) ", ")))

(defun iit-node-start (&rest nodes)
  "Start NODES and wait until their API listeners have started.
Sets the log mark of each node to before the start."
  (let ((nodes (iit--nodes nodes)))
    (apply #'iit-log-mark nodes)
    (iit--log-action "Starting" nodes)
    (iit-podman "start" (mapcar #'iit-node-container nodes))
    (dolist (node nodes)
      (iit--forget-connections node)
      (iit-expect-log node iit-ready-regexp))))

(defun iit-node-stop (&rest nodes)
  "Stop NODES."
  (let ((nodes (iit--nodes nodes)))
    (iit--log-action "Stopping" nodes)
    (iit-podman "stop" "-t" iit-stop-timeout (mapcar #'iit-node-container nodes))
    (mapc #'iit--forget-connections nodes)))

(defun iit-node-restart (&rest nodes)
  "Restart NODES and wait until their API listeners have started.
Sets the log mark of each node to before the restart."
  (let ((nodes (iit--nodes nodes)))
    (apply #'iit-log-mark nodes)
    (iit--log-action "Restarting" nodes)
    (iit-podman "restart" "-t" iit-stop-timeout (mapcar #'iit-node-container nodes))
    (dolist (node nodes)
      (iit--forget-connections node)
      (iit-expect-log node iit-ready-regexp))))

(defun iit-node-reload (&rest nodes)
  "Send SIGHUP to NODES to make Icinga 2 reload its config, without waiting.
Sets the log mark of each node to before the reload.  The old worker keeps
running until the new one has loaded its config; once the new worker has taken
over, it logs `iit-ready-regexp' again."
  (let ((nodes (iit--nodes nodes)))
    (apply #'iit-log-mark nodes)
    (iit--log-action "Reloading" nodes)
    (iit-podman "kill" "--signal" "HUP" (mapcar #'iit-node-container nodes))))

(defun iit-node-reloaded-p (node)
  "Return non-nil if the new worker of NODE has taken over since `iit-node-reload'."
  (string-match-p iit-ready-regexp (iit-node-log node)))

;;;; Logs

(defun iit-node-log-file (node)
  "Return the host path of the main log of NODE."
  (iit-node-dir node "log/icinga2.log"))

(defun iit--file-size (file)
  (or (file-attribute-size (file-attributes file)) 0))

(defun iit-log-mark (&rest nodes)
  "Only consider log lines written from now on for NODES (default: all).
`iit-node-start' and `iit-node-restart' also set the mark."
  (dolist (node (iit--nodes nodes))
    (setf (iit-node-log-mark node) (iit--file-size (iit-node-log-file node)))))

(defun iit-node-log (node &optional from-start)
  "Return the log of NODE written since its mark, or all of it if FROM-START."
  (let* ((node (iit--node node))
         (file (iit-node-log-file node))
         (size (iit--file-size file))
         (mark (if from-start 0 (iit-node-log-mark node))))
    (when (< size mark)                 ; truncated
      (setq mark 0))
    (with-temp-buffer
      (when (> size mark)
        (insert-file-contents-literally file nil mark size))
      (decode-coding-region (point-min) (point-max) 'utf-8)
      (buffer-string))))

(defun iit--last-lines (string n)
  (string-join (last (split-string string "\n" t) n) "\n"))

(defun iit--node-diagnostics (node)
  "Return recent log output of NODE for failure messages."
  (pcase-let ((`(,_ ,out ,err)
               (iit--podman-run (list "logs" "--tail" "30" (iit-node-container node)))))
    (format "Last lines of icinga2.log:\n%s\n\nLast lines of podman logs:\n%s"
            (iit--last-lines (iit-node-log node t) 30)
            (string-trim (concat out "\n" err)))))

(defun iit-expect-log (node regexp &optional count timeout)
  "Wait until REGEXP appears COUNT times in the log of NODE after its mark.
Fails the test after TIMEOUT seconds (default `iit-log-timeout') or when the
node stops running."
  (let* ((node (iit--node node))
         (timeout (or timeout iit-log-timeout))
         (deadline (+ (float-time) timeout))
         (next-check (+ (float-time) 2))
         (stopped nil))
    (cl-flet ((found-p ()
                (with-temp-buffer
                  (insert (iit-node-log node))
                  (goto-char (point-min))
                  (re-search-forward regexp nil t count))))
      (while (not (found-p))
        (when stopped
          (ert-fail (format "%s stopped running while waiting for %S in its log\n\n%s"
                            (iit-node-name node) regexp (iit--node-diagnostics node))))
        (when (> (float-time) deadline)
          (ert-fail (format "Timed out after %ss waiting for %S%s in the log of %s\n\n%s"
                            timeout regexp (if count (format " (%d times)" count) "")
                            (iit-node-name node) (iit--node-diagnostics node))))
        (when (> (float-time) next-check)
          (setq next-check (+ (float-time) 2))
          ;; Check once more after noticing, the pattern may have been the last thing logged.
          (setq stopped (not (iit-node-running-p node))))
        (unless stopped
          (sleep-for 0.2)))
      t)))

;;;; HTTP API

(defvar iit-curl-program "curl"
  "The curl executable used for HTTP requests.")

(defun iit-http-request (method url &optional body username password)
  "Send an HTTP request using METHOD to URL and return the parsed JSON response.
The request can have an optional BODY and use basic authentication with
USERNAME and PASSWORD.  TLS certificates are not verified.

This uses curl rather than url.el, whose connection reuse breaks when the
server goes away between requests, as it does when nodes are restarted."
  (let ((stdout-file (make-temp-file "iit-http"))
        (stderr-file (make-temp-file "iit-stderr")))
    (unwind-protect
        (let ((status (apply #'call-process iit-curl-program nil
                             (list (list :file stdout-file) stderr-file) nil
                             (flatten-tree
                              (list "--silent" "--show-error" "--insecure"
                                    "--max-time" "30"
                                    "--request" (symbol-name method)
                                    "--header" "Accept: application/json"
                                    "--header" "Content-Type: application/json"
                                    (when (and username password)
                                      (list "--user" (concat username ":" password)))
                                    (when body
                                      (list "--data-binary" (json-encode body)))
                                    url)))))
          (unless (eql status 0)
            (error "%s %s failed (curl exit %s): %s"
                   method url status (iit--read-file stderr-file)))
          (with-temp-buffer
            (let ((coding-system-for-read 'utf-8))
              (insert-file-contents stdout-file))
            (condition-case nil
                (json-parse-buffer :object-type 'alist :array-type 'list)
              (json-error
               (error "%s %s returned invalid JSON: %s" method url (buffer-string))))))
      (delete-file stdout-file)
      (delete-file stderr-file))))

(defun iit-api-request (method node path &optional body)
  "Send a request using METHOD to the API of NODE (default: first node) at PATH."
  (iit-http-request method
                      (format "https://%s/%s"
                              (iit-node-address (or node (iit-default-node)))
                              (string-remove-prefix "/" path))
                      body iit-api-user iit-api-password))

;;;; Cluster lifecycle

(defun iit--write-node-config (cluster node)
  "Write the generated /etc/icinga2 of NODE to its host directory."
  (let ((etc (file-name-as-directory (iit-node-dir node "etc"))))
    (make-directory etc t)
    (make-directory (iit-node-dir node "log/crash") t)
    (pcase-dolist (`(,path . ,content) (iit--node-config-files cluster node))
      (let ((file (expand-file-name path etc)))
        (unless (string-prefix-p etc file)
          (error "File %s of node %s is outside of /etc/icinga2" path (iit-node-name node)))
        (make-directory (file-name-directory file) t)
        (let ((coding-system-for-write 'utf-8-unix))
          (write-region content nil file nil 'silent))))))

(defun iit--generate-certificates (cluster)
  "Create a CA and a certificate for each node of CLUSTER."
  (let ((pki (expand-file-name "pki" (iit-cluster-dir cluster)))
        (names (mapcar #'iit-node-name (iit-cluster-nodes cluster))))
    (message "Generating certificates")
    (make-directory (expand-file-name "ca" pki) t)
    (iit-podman "run" "--rm" "--label" iit-label (iit--user-args)
                  "-v" (concat pki ":/pki:z")
                  "-v" (concat pki "/ca:/data/var/lib/icinga2/ca:z")
                  "--entrypoint" "/bin/sh"
                  iit-image
                  "-c" "set -e
icinga2 pki new-ca
for n in \"$@\"; do
  icinga2 pki new-cert --cn \"$n\" --key \"/pki/$n.key\" --csr \"/pki/$n.csr\"
  icinga2 pki sign-csr --csr \"/pki/$n.csr\" --cert \"/pki/$n.crt\"
done"
                  "sh" names)
    (dolist (node (iit-cluster-nodes cluster))
      (let ((name (iit-node-name node))
            (certs (file-name-as-directory (iit-node-dir node "certs"))))
        (make-directory certs t)
        (copy-file (expand-file-name (concat name ".crt") pki) certs t)
        (copy-file (expand-file-name (concat name ".key") pki) certs t)
        (copy-file (expand-file-name "ca/ca.crt" pki) certs t)))))

(defun iit--mount-args (node mount)
  "Return the podman arguments for MOUNT of NODE: (HOST CONTAINER [OPTIONS]).
A relative HOST path is relative to the node's directory and created if needed."
  (pcase-let ((`(,host ,container ,options) mount))
    (let ((host (expand-file-name host (iit-node-dir node))))
      (unless (file-name-absolute-p container)
        (error "Mount target %s of node %s must be absolute" container (iit-node-name node)))
      (make-directory host t)
      (list "-v" (concat host ":" container ":z" (if options (concat "," options) ""))))))

(defun iit--create-container (cluster node)
  "Create (but don't start) the container for NODE."
  (let ((name (iit-node-name node)))
    (iit-podman "create"
                  "--name" (iit-node-container node)
                  "--hostname" name
                  "--network" (iit-cluster-network cluster)
                  "--network-alias" name
                  "--label" iit-label
                  (iit--user-args)
                  "--stop-timeout" iit-stop-timeout
                  "-p" (format "127.0.0.1:%s:5665" (or (iit-node-port node) ""))
                  "-v" (concat (iit-node-dir node "etc") ":/data/etc/icinga2:z")
                  "-v" (concat (iit-node-dir node "certs") ":/data/var/lib/icinga2/certs:z")
                  "-v" (concat (iit-node-dir node "log") ":/data/var/log/icinga2:z")
                  (mapcar (lambda (mount) (iit--mount-args node mount)) (iit-node-mounts node))
                  iit-image)))

(defun iit--setup-cluster (cluster)
  (setf (iit-cluster-dir cluster)
        (file-name-as-directory (make-temp-file "iit-" t)))
  (message "Setting up cluster in %s" (iit-cluster-dir cluster))
  (dolist (node (iit-cluster-nodes cluster))
    (iit--write-node-config cluster node)
    (when (iit-node-log-throttle node)
      (iit--start-log-throttle node)))
  (iit--generate-certificates cluster)
  (message "Creating network and containers")
  (iit-podman "network" "create" "--label" iit-label (iit-cluster-network cluster))
  (dolist (node (iit-cluster-nodes cluster))
    (iit--create-container cluster node)))

(defun iit--teardown-cluster (cluster ok)
  "Remove everything belonging to CLUSTER, keeping some of it if not OK."
  (let ((nodes (iit-cluster-nodes cluster))
        (dir (iit-cluster-dir cluster)))
    ;; Kept containers with a throttled log stop once the FIFO is full.
    (mapc #'iit--stop-log-throttle nodes)
    (if (and (not ok) iit-keep-on-failure)
        (message "Keeping containers %s"
                 (string-join (mapcar #'iit-node-container nodes) ", "))
      (with-demoted-errors "Cluster cleanup failed: %S"
        (message "Removing cluster")
        (mapc #'iit--forget-connections nodes)
        (iit--podman-run (list "rm" "-f" "-v" "--ignore" "-t" "0"
                                 (mapcar #'iit-node-container nodes)))
        (iit--podman-run (list "network" "rm" "-f" (iit-cluster-network cluster)))))
    (when dir
      (if ok
          (with-demoted-errors "Removing test directory failed: %S"
            (delete-directory dir t))
        (message "Keeping test directory %s" dir)))))

(defun iit-call-with-cluster (spec function)
  "Set up the cluster described by SPEC, call FUNCTION and tear it down again.
See `iit-with-cluster'."
  (let* ((cluster (iit-parse-cluster-spec spec))
         (iit-current-cluster cluster)
         (ok nil))
    (setf (iit-cluster-network cluster) (concat iit-prefix "net"))
    (iit-ensure-image)
    (iit-sweep)
    (unwind-protect
        (progn
          (iit--setup-cluster cluster)
          (when (iit-cluster-start cluster)
            (apply #'iit-node-start (iit-cluster-nodes cluster)))
          (funcall function)
          (setq ok t))
      (iit--teardown-cluster cluster ok))))

(defmacro iit-with-cluster (spec &rest body)
  "Run BODY with an Icinga 2 cluster described by SPEC.

SPEC is a list of node specs, optionally followed by cluster options.  It is
implicitly backquoted, so parts of it can be computed with `,' and `,@'.

A node spec is NAME or (NAME . OPTIONS) with these options:

  :zone NAME       zone of the node, defaults to the node name
  :parent NAME     parent zone of the node's zone
  :port PORT       host port for the API, defaults to a random free port
  :features LIST   features to enable, see `iit-feature-configs'
                   (default `iit-default-features')
  :files FILES     extra files in /etc/icinga2, as (PATH STATEMENT...) where
                   the statements are rendered with `iit-config-render'.
                   They replace generated files with the same PATH.
  :mounts MOUNTS   extra bind mounts as (HOST CONTAINER [OPTIONS]); a relative
                   HOST is below `iit-node-dir'
  :log-throttle (REGEXP SECONDS)
                   make Icinga 2 wait SECONDS after logging a line matching
                   REGEXP, a bash (POSIX extended) regexp; see the
                   \"Log throttling\" section of iit-lib.el

Cluster options:

  :global-zones LIST  names of global zones
  :start BOOL         start all nodes before BODY (default t)

All nodes share a network, where they can reach each other by node name.
Inside BODY, functions like `iit-node-start' or `iit-expect-log' take
node names as symbols."
  (declare (indent 1) (debug (sexp body)))
  `(iit-call-with-cluster ,(list '\` spec) (lambda () ,@body)))

;;;; Self-tests

;; Unit tests of this file, which need neither podman nor Icinga 2.  They are tagged :unit.

(ert-deftest iit-config-render-object ()
  :tags '(:unit)
  (should (equal (iit-config-render
                  '(object Host "h\"1"
                           (import "generic-host")
                           (check_command "dummy")
                           (check_interval 5m)
                           (enable_active_checks nil)
                           (max_check_attempts 3)
                           (groups ["a" "b"])
                           (vars (dict (os "Linux") ("a b" (dict (x t)))))
                           (vars.empty (dict))
                           (groups += [])
                           (address (raw "NodeName + \".local\""))))
                 "object Host \"h\\\"1\" {
  import \"generic-host\"
  check_command = \"dummy\"
  check_interval = 5m
  enable_active_checks = false
  max_check_attempts = 3
  groups = [ \"a\", \"b\" ]
  vars = {
    os = \"Linux\"
    \"a b\" = {
      x = true
    }
  }
  vars.empty = {}
  groups += []
  address = NodeName + \".local\"
}
")))

(ert-deftest iit-config-render-statements ()
  :tags '(:unit)
  (should (equal (iit-config-render
                  '(include <itl>)
                  '(include "zones.conf")
                  '(include-recursive "conf.d")
                  '(const NodeName "master-1\n")
                  '(apply Service "ping" (check_command "ping4") (assign "host.address"))
                  '(apply Notification "mail" :to Host (assign "true") (ignore "false"))
                  "// raw text")
                 "include <itl>

include \"zones.conf\"

include_recursive \"conf.d\"

const NodeName = \"master-1\\n\"

apply Service \"ping\" {
  check_command = \"ping4\"
  assign where host.address
}

apply Notification \"mail\" to Host {
  assign where true
  ignore where false
}

// raw text
"))
  (should-error (iit-config-render '(bogus 1)))
  (should-error (iit-config-render '(object Host "h" (vars :unknown)))))

(ert-deftest iit-cluster-spec-parsing ()
  :tags '(:unit)
  (let* ((n 2)
         (spec `((master-1 :zone "master")
                 (master-2 :zone "master")
                 ,@(mapcar (lambda (i) `(,(intern (format "sat-%d" i)) :zone "sat" :parent "master"))
                           (number-sequence 1 n))
                 agent-1
                 :global-zones ("global-templates")))
         (cluster (iit-parse-cluster-spec spec)))
    (should (equal (mapcar #'iit-node-name (iit-cluster-nodes cluster))
                   '("master-1" "master-2" "sat-1" "sat-2" "agent-1")))
    (should (equal (iit--cluster-zones cluster)
                   '(("master" nil "master-1" "master-2")
                     ("sat" "master" "sat-1" "sat-2")
                     ("agent-1" nil "agent-1"))))
    (should (string-match-p (regexp-quote "object Zone \"global-templates\" {\n  global = true\n}")
                            (apply #'iit-config-render (iit--zones-conf cluster)))))
  (should-error (iit-parse-cluster-spec '((master-1 :zoen "master"))))
  (should-error (iit-parse-cluster-spec '(master-1 master-1)))
  (should-error (iit-parse-cluster-spec '((sat-1 :parent "master"))))
  (should-error (iit-parse-cluster-spec '((a :zone "z" :parent "x") (b :zone "z" :parent "y") x y)))
  (should-error (iit-parse-cluster-spec '(master-1 :bogus t)))
  (should-error (iit-parse-cluster-spec '((master-1 :log-throttle ("Copying" "1")))))
  (should-error (iit-parse-cluster-spec '((master-1 :log-throttle "Copying")))))

(ert-deftest iit-node-config-files ()
  :tags '(:unit)
  (let* ((cluster (iit-parse-cluster-spec
                   '((master-1 :zone "master"
                               :features (api debuglog)
                               :files (("zones.d/master/hosts.conf"
                                        (object Host "h1" (check_command "dummy")))
                                       ("conf.d/api-users.conf" "// replaced")))
                     (master-2 :zone "master"))))
         (files (iit--node-config-files cluster (car (iit-cluster-nodes cluster)))))
    (should (string-match-p "const NodeName = \"master-1\"" (alist-get "constants.conf" files nil nil #'equal)))
    (should (equal (mapcar #'car (seq-filter (lambda (f) (string-prefix-p "features-enabled/" (car f))) files))
                   '("features-enabled/api.conf" "features-enabled/debuglog.conf")))
    (should (equal (cdr (assoc "zones.d/master/hosts.conf" files))
                   "object Host \"h1\" {\n  check_command = \"dummy\"\n}\n"))
    ;; Later entries win when the files are written.
    (should (equal (cdr (car (last files))) "// replaced\n")))
  (let* ((cluster (iit-parse-cluster-spec '((m :log-throttle ("Copying file" 0.01)))))
         (files (iit--node-config-files cluster (car (iit-cluster-nodes cluster)))))
    (should (equal (cdr (assoc "features-enabled/log-throttle.conf" files))
                   "object FileLogger \"log-throttle\" {
  severity = \"information\"
  path = LogDir + \"/log-throttle.fifo\"
}
")))
  (let ((cluster (iit-parse-cluster-spec '((m :features (nonexistent))))))
    (should-error (iit--node-config-files cluster (car (iit-cluster-nodes cluster))))))

(provide 'iit-lib)
;;; iit-lib.el ends here
