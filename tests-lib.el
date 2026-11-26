;;; tests-lib.el --- Run Icinga 2 clusters in podman containers for ERT tests -*- lexical-binding: t -*-

;;; Commentary:

;; Infrastructure for integration tests that run against real Icinga 2 clusters.
;;
;; The image is built from an Icinga 2 source tree (`tests-icinga2-source') once per run.  Each
;; `tests-with-cluster' form then sets up a complete cluster from a declarative spec: it generates
;; a CA and node certificates, the configuration for every node, a podman network and one container
;; per node.  Everything is removed again when the form exits.
;;
;; Only rootless podman is supported.  Containers run with `--userns=keep-id' mapped to the icinga
;; user, so every file the containers write into bind mounts belongs to the user running the tests
;; and can be removed without `podman unshare'.
;;
;; Per node, the following host directories (below `tests-node-dir') are bind-mounted:
;;
;;   etc/    -> /etc/icinga2              (generated config, see `tests--node-config-files')
;;   certs/  -> /var/lib/icinga2/certs
;;   log/    -> /var/log/icinga2          (read directly by `tests-expect-log')
;;
;; The rest of /data, including /var/lib/icinga2/api, lives in the container's anonymous volume.
;; It can be accessed with TRAMP through `tests-node-file', or mounted to the host with the :mounts
;; node option.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'json)
(require 'subr-x)
(require 'tramp)
(require 'url-util)

(defconst tests-directory (file-name-directory (or load-file-name buffer-file-name))
  "Directory containing the test files.")

;;;; Settings

(defvar tests-podman-program "podman"
  "The podman executable.")

(defvar tests-icinga2-source (or (getenv "ICINGA2_SOURCE")
                                 (expand-file-name "../icinga2" tests-directory))
  "Icinga 2 source tree to build the image from.")

(defvar tests-image "localhost/config-sync-tests/icinga2:latest"
  "Name of the Icinga 2 image to build and test.")

(defvar tests-build-image t
  "Whether to build `tests-image' before running the first cluster test.
When nil, the image has to exist already.")

(defvar tests-build-args '("ICINGA2_BUILD_TESTING=OFF")
  "Build arguments passed to `podman build'.")

(defvar tests-keep-on-failure nil
  "When non-nil, don't remove the containers of a failed cluster test.
They are still removed at the start of the next cluster test, so this is mostly
useful when running a single test.")

(defvar tests-prefix "cst-"
  "Prefix for the names of containers and networks.")

(defvar tests-label "config-sync-tests"
  "Label attached to all containers and networks, used to clean up leftovers.")

(defvar tests-icinga-uid 5665
  "UID and GID of the icinga user in the image.")

(defvar tests-log-timeout 60
  "Default number of seconds `tests-expect-log' waits for a pattern.")

(defvar tests-stop-timeout 30
  "Seconds podman waits for Icinga 2 to shut down before killing it.")

(defvar tests-ready-regexp (regexp-quote "/ApiListener: 'api' started.")
  "Log pattern indicating that a node has finished starting up.")

(defvar tests-api-user "root")
(defvar tests-api-password "icinga")

(defvar tests-default-features '(api mainlog checker)
  "Features enabled on a node unless it specifies :features.")

(defconst tests-feature-configs
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

(defun tests-config-string (string)
  "Return STRING as a quoted Icinga 2 string literal."
  (concat "\""
          (replace-regexp-in-string
           "[\"\\\n\t\r]"
           (lambda (c)
             (pcase c
               ("\"" "\\\"") ("\\" "\\\\") ("\n" "\\n") ("\t" "\\t") ("\r" "\\r")))
           string t t)
          "\""))

(defun tests-config--indent (level)
  (make-string (* 2 level) ?\s))

(defun tests-config--value (value level)
  "Render VALUE as an Icinga 2 expression nested at indentation LEVEL."
  (pcase value
    ('t "true")
    ((or 'nil :false) "false")
    (:null "null")
    ((pred stringp) (tests-config-string value))
    ((pred numberp) (number-to-string value))
    ((pred keywordp) (error "Unknown config keyword %S" value))
    ((pred symbolp) (symbol-name value))
    ((pred vectorp)
     (if (zerop (length value))
         "[]"
       (concat "[ "
               (mapconcat (lambda (v) (tests-config--value v level)) value ", ")
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
                     (concat (tests-config--indent (1+ level))
                             (if (stringp key) (tests-config-string key) (symbol-name key))
                             " = "
                             (tests-config--value v (1+ level))))
                    (_ (error "Invalid dict entry %S" entry))))
                entries "\n")
               "\n" (tests-config--indent level) "}")))
    (_ (error "Can't render %S as a config value" value))))

(defun tests-config--body-item (item level)
  "Render ITEM of an object body at indentation LEVEL."
  (concat
   (tests-config--indent level)
   (pcase item
     (`(import ,name) (concat "import " (tests-config-string name)))
     (`(assign ,(and expr (pred stringp))) (concat "assign where " expr))
     (`(ignore ,(and expr (pred stringp))) (concat "ignore where " expr))
     (`(raw ,(and text (pred stringp))) text)
     (`(,(and attr (pred symbolp)) ,(and op (or '+= '-=)) ,value)
      (format "%s %s %s" attr op (tests-config--value value level)))
     (`(,(and attr (pred symbolp)) ,value)
      (format "%s = %s" attr (tests-config--value value level)))
     (_ (error "Invalid object body item %S" item)))))

(defun tests-config--statement (form)
  "Render the top-level config statement FORM."
  (pcase form
    ((pred stringp) form)
    (`(,(and kind (or 'object 'template 'apply)) ,type ,name . ,body)
     (let ((to nil))
       (when (eq (car body) :to)
         (setq to (cadr body)
               body (cddr body)))
       (format "%s %s %s%s {\n%s}"
               kind type (tests-config-string (format "%s" name))
               (if to (format " to %s" to) "")
               (mapconcat (lambda (item) (concat (tests-config--body-item item 1) "\n"))
                          body ""))))
    (`(const ,name ,value) (format "const %s = %s" name (tests-config--value value 0)))
    (`(include ,(and file (pred stringp))) (concat "include " (tests-config-string file)))
    (`(include ,(and file (pred symbolp))) (format "include %s" file))
    (`(include-recursive ,dir) (concat "include_recursive " (tests-config-string dir)))
    (`(raw ,(and text (pred stringp))) text)
    (_ (error "Unknown config statement %S" form))))

(defun tests-config-render (&rest forms)
  "Render the config statements FORMS to a string."
  (mapconcat (lambda (form) (concat (tests-config--statement form) "\n")) forms "\n"))

;;;; Podman

(defun tests--podman-args (args)
  (mapcar (lambda (a) (if (stringp a) a (format "%s" a))) (flatten-tree args)))

(defvar tests-verbose nil
  "When non-nil, log every podman command before running it.")

(defun tests--read-file (file)
  (with-temp-buffer
    (insert-file-contents file)
    (string-trim (buffer-string))))

(defun tests--podman-run (args)
  "Run podman with ARGS and return (STATUS STDOUT STDERR).
Output goes to temporary files rather than a pipe: with a pipe, `call-process'
waits until every process holding it has exited, which never happens when
podman leaves helper processes such as conmon or pasta running in the
background."
  (let ((args (tests--podman-args args))
        (stdout-file (make-temp-file "config-sync-tests-stdout"))
        (stderr-file (make-temp-file "config-sync-tests-stderr")))
    (when tests-verbose
      (message "+ podman %s" (string-join args " ")))
    (unwind-protect
        (let ((status (apply #'call-process tests-podman-program nil
                             (list (list :file stdout-file) stderr-file) nil args)))
          (list status (tests--read-file stdout-file) (tests--read-file stderr-file)))
      (delete-file stdout-file)
      (delete-file stderr-file))))

(defun tests-podman (&rest args)
  "Run podman with ARGS and return its trimmed output; signal an error on failure.
ARGS are flattened and nil elements are dropped, so optional arguments can
be passed as lists."
  (pcase-let ((`(,status ,out ,err) (tests--podman-run args)))
    (unless (eql status 0)
      (error "`podman %s' failed (%s): %s"
             (string-join (tests--podman-args args) " ") status err))
    out))

(defun tests-podman-ok-p (&rest args)
  "Run podman with ARGS and return non-nil if it exits successfully."
  (eql 0 (car (tests--podman-run args))))

(defun tests--podman-stream (&rest args)
  "Run podman with ARGS, passing its output through to stderr."
  (let* ((args (tests--podman-args args))
         (proc (make-process
                :name "podman"
                :command (cons tests-podman-program args)
                :connection-type 'pipe
                :noquery t
                :filter (lambda (_ output) (princ output #'external-debugging-output))
                :sentinel #'ignore)))
    (while (process-live-p proc)
      (accept-process-output proc 1))
    (accept-process-output proc 0.1)
    (unless (zerop (process-exit-status proc))
      (error "`podman %s' failed (%s)" (string-join args " ") (process-exit-status proc)))))

(defun tests--userns-args ()
  (format "--userns=keep-id:uid=%d,gid=%d" tests-icinga-uid tests-icinga-uid))

(defvar tests--image-ready nil
  "Non-nil once `tests-image' has been built or found in this run.")

(defun tests-ensure-image ()
  "Build `tests-image' once per run, or check that it exists."
  (unless tests--image-ready
    (if tests-build-image
        (let* ((source (file-name-as-directory (expand-file-name tests-icinga2-source)))
               (containerfile (expand-file-name "Containerfile" source)))
          (unless (file-exists-p containerfile)
            (error "No Containerfile in %s; set ICINGA2_SOURCE or pass --source" source))
          (message "Building %s from %s" tests-image source)
          (tests--podman-stream "build" "-t" tests-image "-f" containerfile
                                (mapcar (lambda (arg) (list "--build-arg" arg)) tests-build-args)
                                source))
      (unless (tests-podman-ok-p "image" "exists" tests-image)
        (error "Image %s doesn't exist; run once without --skip-build" tests-image)))
    (setq tests--image-ready t)))

(defun tests-sweep ()
  "Remove all containers and networks left over from earlier runs."
  (let ((label (concat "label=" tests-label)))
    (when-let* ((containers (split-string (tests-podman "ps" "-aq" "--filter" label) nil t)))
      (tests-podman "rm" "-f" "-v" "-t" "0" containers))
    (when-let* ((networks (split-string (tests-podman "network" "ls" "-q" "--filter" label) nil t)))
      (tests-podman "network" "rm" "-f" networks))))

;;;; Cluster specs

(cl-defstruct (tests-cluster (:constructor tests--make-cluster) (:copier nil))
  nodes global-zones (start t) dir network)

(cl-defstruct (tests-node (:constructor tests--make-node) (:copier nil))
  name zone parent port features files mounts (log-mark 0) cached-address)

(defvar tests-current-cluster nil
  "The cluster of the innermost `tests-with-cluster'.")

(defconst tests--node-keys '(:zone :parent :port :features :files :mounts))
(defconst tests--cluster-keys '(:global-zones :start))

(defun tests--parse-node (spec)
  "Parse the node SPEC, either NAME or (NAME . PLIST)."
  (let* ((spec (if (consp spec) spec (list spec)))
         (name (car spec))
         (plist (cdr spec)))
    (unless (and name (or (symbolp name) (stringp name)))
      (error "Invalid node name in %S" spec))
    (unless (zerop (% (length plist) 2))
      (error "Odd number of options for node %s" name))
    (cl-loop for key in plist by #'cddr
             unless (memq key tests--node-keys)
             do (error "Unknown option %S for node %s" key name))
    (let ((name (format "%s" name)))
      (tests--make-node
       :name name
       :zone (format "%s" (or (plist-get plist :zone) name))
       :parent (when-let* ((parent (plist-get plist :parent))) (format "%s" parent))
       :port (plist-get plist :port)
       :features (if (plist-member plist :features)
                     (plist-get plist :features)
                   tests-default-features)
       :files (plist-get plist :files)
       :mounts (plist-get plist :mounts)))))

(defun tests-parse-cluster-spec (spec)
  "Parse the cluster SPEC of `tests-with-cluster' into a `tests-cluster'."
  (let (nodes options)
    (while spec
      (let ((item (pop spec)))
        (cond
         ((keywordp item)
          (unless (memq item tests--cluster-keys)
            (error "Unknown cluster option %S" item))
          (unless spec
            (error "Missing value for cluster option %S" item))
          (setq options (plist-put options item (pop spec))))
         ((or (consp item) (symbolp item) (stringp item))
          (push (tests--parse-node item) nodes))
         (t (error "Invalid cluster spec element %S" item)))))
    (unless nodes
      (error "A cluster needs at least one node"))
    (setq nodes (nreverse nodes))
    (let ((names (mapcar #'tests-node-name nodes)))
      (unless (equal names (delete-dups (copy-sequence names)))
        (error "Duplicate node names in %S" names)))
    (let ((cluster (tests--make-cluster
                    :nodes nodes
                    :global-zones (mapcar (lambda (z) (format "%s" z))
                                          (plist-get options :global-zones))
                    :start (if (plist-member options :start) (plist-get options :start) t))))
      (tests--cluster-zones cluster)    ; validates zones
      cluster)))

(defun tests--cluster-zones (cluster)
  "Return the zones of CLUSTER as a list of (NAME PARENT ENDPOINT...)."
  (let (zones)
    (dolist (node (tests-cluster-nodes cluster))
      (let* ((zone (tests-node-zone node))
             (parent (tests-node-parent node))
             (entry (assoc zone zones)))
        (if (not entry)
            (push (list zone parent (tests-node-name node)) zones)
          (when (and parent (cadr entry) (not (equal parent (cadr entry))))
            (error "Conflicting parents for zone %s: %s and %s" zone (cadr entry) parent))
          (when parent
            (setf (cadr entry) parent))
          (setf (cddr entry) (append (cddr entry) (list (tests-node-name node)))))))
    (setq zones (nreverse zones))
    (dolist (zone zones)
      (when-let* ((parent (cadr zone)))
        (unless (assoc parent zones)
          (error "Parent zone %s of zone %s has no nodes" parent (car zone)))))
    zones))

;;;; Generated config

(defun tests--zones-conf (cluster)
  "Return the zones.conf statements for CLUSTER.
Every node gets the full topology; Icinga 2 only connects to the endpoints
it is supposed to."
  (append
   (mapcar (lambda (node)
             (let ((name (tests-node-name node)))
               `(object Endpoint ,name (host ,name) (port "5665"))))
           (tests-cluster-nodes cluster))
   (mapcar (pcase-lambda (`(,zone ,parent . ,endpoints))
             `(object Zone ,zone
                      (endpoints ,(vconcat endpoints))
                      ,@(when parent `((parent ,parent)))))
           (tests--cluster-zones cluster))
   (mapcar (lambda (zone) `(object Zone ,zone (global t)))
           (tests-cluster-global-zones cluster))))

(defun tests--node-config-files (cluster node)
  "Return the files of NODE's /etc/icinga2 as an alist (PATH . CONTENT).
Files from the node's :files come last and take precedence."
  `(("icinga2.conf"
     . ,(tests-config-render
         '(include "constants.conf")
         '(include "zones.conf")
         '(include <itl>)
         '(include <plugins>)
         '(include "features-enabled/*.conf")
         '(include-recursive "conf.d")))
    ("constants.conf"
     . ,(tests-config-render
         '(const PluginDir "/usr/lib/nagios/plugins")
         '(const ManubulonPluginDir "/usr/lib/nagios/plugins")
         '(const PluginContribDir "/usr/lib/nagios/plugins")
         `(const NodeName ,(tests-node-name node))
         `(const ZoneName ,(tests-node-zone node))
         '(const TicketSalt "")))
    ("zones.conf" . ,(apply #'tests-config-render (tests--zones-conf cluster)))
    ("conf.d/api-users.conf"
     . ,(tests-config-render
         `(object ApiUser ,tests-api-user
                  (password ,tests-api-password)
                  (permissions ["*"]))))
    ,@(mapcar (lambda (feature)
                (cons (format "features-enabled/%s.conf" feature)
                      (apply #'tests-config-render
                             (or (alist-get feature tests-feature-configs)
                                 (error "Unknown feature %S for node %s"
                                        feature (tests-node-name node))))))
              (tests-node-features node))
    ,@(mapcar (pcase-lambda (`(,path . ,forms))
                (cons path (apply #'tests-config-render forms)))
              (tests-node-files node))))

;;;; Nodes

(defun tests--node (node)
  "Return the `tests-node' designated by NODE: a struct, symbol or string."
  (if (tests-node-p node)
      node
    (or (and tests-current-cluster
             (cl-find (format "%s" node) (tests-cluster-nodes tests-current-cluster)
                      :key #'tests-node-name :test #'equal))
        (error "No node %s in the current cluster" node))))

(defun tests--nodes (nodes)
  "Return NODES as structs, or all nodes of the current cluster if NODES is nil."
  (if nodes
      (mapcar #'tests--node nodes)
    (tests-cluster-nodes tests-current-cluster)))

(defun tests-default-node ()
  "Return the first node of the current cluster."
  (car (tests-cluster-nodes tests-current-cluster)))

(defun tests-node-container (node)
  "Return the container name of NODE."
  (concat tests-prefix (tests-node-name (tests--node node))))

(defun tests-node-dir (node &optional path)
  "Return the host directory of NODE, or PATH relative to it."
  (let ((dir (file-name-as-directory
              (expand-file-name (tests-node-name (tests--node node))
                                (tests-cluster-dir tests-current-cluster)))))
    (if path (expand-file-name path dir) dir)))

(defun tests-node-file (node path)
  "Return a TRAMP file name for PATH inside the container of NODE."
  (concat "/podman:" (tests-node-container node) ":" path))

(defun tests-node-running-p (node)
  "Return non-nil if the container of NODE is running."
  (equal (car (split-string
               (cadr (tests--podman-run
                      (list "inspect" "-f" "{{.State.Running}}" (tests-node-container node))))))
         "true"))

(defun tests-node-address (node)
  "Return HOST:PORT under which the API of NODE is reachable from the host."
  (let ((node (tests--node node)))
    (or (tests-node-cached-address node)
        (setf (tests-node-cached-address node)
              (car (split-string (tests-podman "port" (tests-node-container node) "5665/tcp")
                                 "\n" t))))))

(defun tests--forget-connections (node)
  "Drop cached connection state for NODE after its container changed state."
  (setf (tests-node-cached-address node) nil)
  (ignore-errors
    (tramp-cleanup-connection (tramp-dissect-file-name (tests-node-file node "/")) t t)))

(defun tests--log-action (action nodes)
  (message "%s %s" action (string-join (mapcar #'tests-node-name nodes) ", ")))

(defun tests-node-start (&rest nodes)
  "Start NODES and wait until their API listeners have started.
Sets the log mark of each node to before the start."
  (let ((nodes (tests--nodes nodes)))
    (apply #'tests-log-mark nodes)
    (tests--log-action "Starting" nodes)
    (tests-podman "start" (mapcar #'tests-node-container nodes))
    (dolist (node nodes)
      (tests--forget-connections node)
      (tests-expect-log node tests-ready-regexp))))

(defun tests-node-stop (&rest nodes)
  "Stop NODES."
  (let ((nodes (tests--nodes nodes)))
    (tests--log-action "Stopping" nodes)
    (tests-podman "stop" "-t" tests-stop-timeout (mapcar #'tests-node-container nodes))
    (mapc #'tests--forget-connections nodes)))

(defun tests-node-restart (&rest nodes)
  "Restart NODES and wait until their API listeners have started.
Sets the log mark of each node to before the restart."
  (let ((nodes (tests--nodes nodes)))
    (apply #'tests-log-mark nodes)
    (tests--log-action "Restarting" nodes)
    (tests-podman "restart" "-t" tests-stop-timeout (mapcar #'tests-node-container nodes))
    (dolist (node nodes)
      (tests--forget-connections node)
      (tests-expect-log node tests-ready-regexp))))

;;;; Logs

(defun tests-node-log-file (node)
  "Return the host path of the main log of NODE."
  (tests-node-dir node "log/icinga2.log"))

(defun tests--file-size (file)
  (or (file-attribute-size (file-attributes file)) 0))

(defun tests-log-mark (&rest nodes)
  "Only consider log lines written from now on for NODES (default: all).
`tests-node-start' and `tests-node-restart' also set the mark."
  (dolist (node (tests--nodes nodes))
    (setf (tests-node-log-mark node) (tests--file-size (tests-node-log-file node)))))

(defun tests-node-log (node &optional from-start)
  "Return the log of NODE written since its mark, or all of it if FROM-START."
  (let* ((node (tests--node node))
         (file (tests-node-log-file node))
         (size (tests--file-size file))
         (mark (if from-start 0 (tests-node-log-mark node))))
    (when (< size mark)                 ; truncated
      (setq mark 0))
    (with-temp-buffer
      (when (> size mark)
        (insert-file-contents-literally file nil mark size))
      (decode-coding-region (point-min) (point-max) 'utf-8)
      (buffer-string))))

(defun tests--last-lines (string n)
  (string-join (last (split-string string "\n" t) n) "\n"))

(defun tests--node-diagnostics (node)
  "Return recent log output of NODE for failure messages."
  (pcase-let ((`(,_ ,out ,err)
               (tests--podman-run (list "logs" "--tail" "30" (tests-node-container node)))))
    (format "Last lines of icinga2.log:\n%s\n\nLast lines of podman logs:\n%s"
            (tests--last-lines (tests-node-log node t) 30)
            (string-trim (concat out "\n" err)))))

(defun tests-expect-log (node regexp &optional count timeout)
  "Wait until REGEXP appears COUNT times in the log of NODE after its mark.
Fails the test after TIMEOUT seconds (default `tests-log-timeout') or when the
node stops running."
  (let* ((node (tests--node node))
         (timeout (or timeout tests-log-timeout))
         (deadline (+ (float-time) timeout))
         (next-check (+ (float-time) 2))
         (stopped nil))
    (cl-flet ((found-p ()
                (with-temp-buffer
                  (insert (tests-node-log node))
                  (goto-char (point-min))
                  (re-search-forward regexp nil t count))))
      (while (not (found-p))
        (when stopped
          (ert-fail (format "%s stopped running while waiting for %S in its log\n\n%s"
                            (tests-node-name node) regexp (tests--node-diagnostics node))))
        (when (> (float-time) deadline)
          (ert-fail (format "Timed out after %ss waiting for %S%s in the log of %s\n\n%s"
                            timeout regexp (if count (format " (%d times)" count) "")
                            (tests-node-name node) (tests--node-diagnostics node))))
        (when (> (float-time) next-check)
          (setq next-check (+ (float-time) 2))
          ;; Check once more after noticing, the pattern may have been the last thing logged.
          (setq stopped (not (tests-node-running-p node))))
        (unless stopped
          (sleep-for 0.2)))
      t)))

;;;; HTTP API

(defvar tests-curl-program "curl"
  "The curl executable used for HTTP requests.")

(defun tests-http-request (method url &optional body username password)
  "Send an HTTP request using METHOD to URL and return the parsed JSON response.
The request can have an optional BODY and use basic authentication with
USERNAME and PASSWORD.  TLS certificates are not verified.

This uses curl rather than url.el, whose connection reuse breaks when the
server goes away between requests, as it does when nodes are restarted."
  (let ((stdout-file (make-temp-file "config-sync-tests-http"))
        (stderr-file (make-temp-file "config-sync-tests-stderr")))
    (unwind-protect
        (let ((status (apply #'call-process tests-curl-program nil
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
                   method url status (tests--read-file stderr-file)))
          (with-temp-buffer
            (let ((coding-system-for-read 'utf-8))
              (insert-file-contents stdout-file))
            (condition-case nil
                (json-parse-buffer :object-type 'alist :array-type 'list)
              (json-error
               (error "%s %s returned invalid JSON: %s" method url (buffer-string))))))
      (delete-file stdout-file)
      (delete-file stderr-file))))

(defun tests-api-request (method node path &optional body)
  "Send a request using METHOD to the API of NODE (default: first node) at PATH."
  (tests-http-request method
                      (format "https://%s/%s"
                              (tests-node-address (or node (tests-default-node)))
                              (string-remove-prefix "/" path))
                      body tests-api-user tests-api-password))

;;;; Cluster lifecycle

(defun tests--write-node-config (cluster node)
  "Write the generated /etc/icinga2 of NODE to its host directory."
  (let ((etc (file-name-as-directory (tests-node-dir node "etc"))))
    (make-directory etc t)
    (make-directory (tests-node-dir node "log/crash") t)
    (pcase-dolist (`(,path . ,content) (tests--node-config-files cluster node))
      (let ((file (expand-file-name path etc)))
        (unless (string-prefix-p etc file)
          (error "File %s of node %s is outside of /etc/icinga2" path (tests-node-name node)))
        (make-directory (file-name-directory file) t)
        (let ((coding-system-for-write 'utf-8-unix))
          (write-region content nil file nil 'silent))))))

(defun tests--generate-certificates (cluster)
  "Create a CA and a certificate for each node of CLUSTER."
  (let ((pki (expand-file-name "pki" (tests-cluster-dir cluster)))
        (names (mapcar #'tests-node-name (tests-cluster-nodes cluster))))
    (message "Generating certificates")
    (make-directory (expand-file-name "ca" pki) t)
    (tests-podman "run" "--rm" "--label" tests-label (tests--userns-args)
                  "-v" (concat pki ":/pki:z")
                  "-v" (concat pki "/ca:/data/var/lib/icinga2/ca:z")
                  "--entrypoint" "/bin/sh"
                  tests-image
                  "-c" "set -e
icinga2 pki new-ca
for n in \"$@\"; do
  icinga2 pki new-cert --cn \"$n\" --key \"/pki/$n.key\" --csr \"/pki/$n.csr\"
  icinga2 pki sign-csr --csr \"/pki/$n.csr\" --cert \"/pki/$n.crt\"
done"
                  "sh" names)
    (dolist (node (tests-cluster-nodes cluster))
      (let ((name (tests-node-name node))
            (certs (file-name-as-directory (tests-node-dir node "certs"))))
        (make-directory certs t)
        (copy-file (expand-file-name (concat name ".crt") pki) certs t)
        (copy-file (expand-file-name (concat name ".key") pki) certs t)
        (copy-file (expand-file-name "ca/ca.crt" pki) certs t)))))

(defun tests--mount-args (node mount)
  "Return the podman arguments for MOUNT of NODE: (HOST CONTAINER [OPTIONS]).
A relative HOST path is relative to the node's directory and created if needed."
  (pcase-let ((`(,host ,container ,options) mount))
    (let ((host (expand-file-name host (tests-node-dir node))))
      (unless (file-name-absolute-p container)
        (error "Mount target %s of node %s must be absolute" container (tests-node-name node)))
      (make-directory host t)
      (list "-v" (concat host ":" container ":z" (if options (concat "," options) ""))))))

(defun tests--create-container (cluster node)
  "Create (but don't start) the container for NODE."
  (let ((name (tests-node-name node)))
    (tests-podman "create"
                  "--name" (tests-node-container node)
                  "--hostname" name
                  "--network" (tests-cluster-network cluster)
                  "--network-alias" name
                  "--label" tests-label
                  (tests--userns-args)
                  "--stop-timeout" tests-stop-timeout
                  "-p" (format "127.0.0.1:%s:5665" (or (tests-node-port node) ""))
                  "-v" (concat (tests-node-dir node "etc") ":/data/etc/icinga2:z")
                  "-v" (concat (tests-node-dir node "certs") ":/data/var/lib/icinga2/certs:z")
                  "-v" (concat (tests-node-dir node "log") ":/data/var/log/icinga2:z")
                  (mapcar (lambda (mount) (tests--mount-args node mount)) (tests-node-mounts node))
                  tests-image)))

(defun tests--setup-cluster (cluster)
  (setf (tests-cluster-dir cluster)
        (file-name-as-directory (make-temp-file "config-sync-tests-" t)))
  (message "Setting up cluster in %s" (tests-cluster-dir cluster))
  (dolist (node (tests-cluster-nodes cluster))
    (tests--write-node-config cluster node))
  (tests--generate-certificates cluster)
  (message "Creating network and containers")
  (tests-podman "network" "create" "--label" tests-label (tests-cluster-network cluster))
  (dolist (node (tests-cluster-nodes cluster))
    (tests--create-container cluster node)))

(defun tests--teardown-cluster (cluster ok)
  "Remove everything belonging to CLUSTER, keeping some of it if not OK."
  (let ((nodes (tests-cluster-nodes cluster))
        (dir (tests-cluster-dir cluster)))
    (if (and (not ok) tests-keep-on-failure)
        (message "Keeping containers %s"
                 (string-join (mapcar #'tests-node-container nodes) ", "))
      (with-demoted-errors "Cluster cleanup failed: %S"
        (message "Removing cluster")
        (mapc #'tests--forget-connections nodes)
        (tests--podman-run (list "rm" "-f" "-v" "--ignore" "-t" "0"
                                 (mapcar #'tests-node-container nodes)))
        (tests--podman-run (list "network" "rm" "-f" (tests-cluster-network cluster)))))
    (when dir
      (if ok
          (with-demoted-errors "Removing test directory failed: %S"
            (delete-directory dir t))
        (message "Keeping test directory %s" dir)))))

(defun tests-call-with-cluster (spec function)
  "Set up the cluster described by SPEC, call FUNCTION and tear it down again.
See `tests-with-cluster'."
  (let* ((cluster (tests-parse-cluster-spec spec))
         (tests-current-cluster cluster)
         (ok nil))
    (setf (tests-cluster-network cluster) (concat tests-prefix "net"))
    (tests-ensure-image)
    (tests-sweep)
    (unwind-protect
        (progn
          (tests--setup-cluster cluster)
          (when (tests-cluster-start cluster)
            (apply #'tests-node-start (tests-cluster-nodes cluster)))
          (funcall function)
          (setq ok t))
      (tests--teardown-cluster cluster ok))))

(defmacro tests-with-cluster (spec &rest body)
  "Run BODY with an Icinga 2 cluster described by SPEC.

SPEC is a list of node specs, optionally followed by cluster options.  It is
implicitly backquoted, so parts of it can be computed with `,' and `,@'.

A node spec is NAME or (NAME . OPTIONS) with these options:

  :zone NAME       zone of the node, defaults to the node name
  :parent NAME     parent zone of the node's zone
  :port PORT       host port for the API, defaults to a random free port
  :features LIST   features to enable, see `tests-feature-configs'
                   (default `tests-default-features')
  :files FILES     extra files in /etc/icinga2, as (PATH STATEMENT...) where
                   the statements are rendered with `tests-config-render'.
                   They replace generated files with the same PATH.
  :mounts MOUNTS   extra bind mounts as (HOST CONTAINER [OPTIONS]); a relative
                   HOST is below `tests-node-dir'

Cluster options:

  :global-zones LIST  names of global zones
  :start BOOL         start all nodes before BODY (default t)

All nodes share a network, where they can reach each other by node name.
Inside BODY, functions like `tests-node-start' or `tests-expect-log' take
node names as symbols."
  (declare (indent 1) (debug (sexp body)))
  `(tests-call-with-cluster ,(list '\` spec) (lambda () ,@body)))

;;;; Command line

(defun tests-parse-command-line (args)
  "Apply the options in ARGS and return the ERT selector given there.

  --source DIR   Icinga 2 source tree (default: $ICINGA2_SOURCE or ../icinga2)
  --image NAME   image name to build and use
  --skip-build   use the existing image instead of building it
  --keep         keep the containers of a failed test
  --verbose      log every podman command
  SELECTOR       ERT selector: a regexp matching test names, or a Lisp form
                 like (tag :unit)"
  (let (selector)
    (while args
      (let ((arg (pop args)))
        (pcase arg
          ((or "--source" "--image")
           (let ((value (or (pop args) (error "%s needs an argument" arg))))
             (if (equal arg "--source")
                 (setq tests-icinga2-source (expand-file-name value))
               (setq tests-image value))))
          ("--skip-build" (setq tests-build-image nil))
          ("--keep" (setq tests-keep-on-failure t))
          ("--verbose" (setq tests-verbose t))
          ((pred (string-prefix-p "--")) (error "Unknown option %s" arg))
          (_
           (when selector
             (error "Only one test selector can be given"))
           (setq selector (if (string-match-p "\\`\\(?:[(:]\\|t\\'\\)" arg)
                              (car (read-from-string arg))
                            arg))))))
    (or selector t)))

(provide 'tests-lib)
;;; tests-lib.el ends here
