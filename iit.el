#!/usr/bin/env -S emacs -Q --script
;; -*- lexical-binding: t mode: emacs-lisp -*-

;; Integration tests for Icinga 2 config sync.
;;
;; Usage: ./iit.el [--source DIR] [--image NAME] [--skip-build] [--keep] [--verbose] [SELECTOR]
;;
;;   ./iit.el                                 build ../icinga2 and run all tests
;;   ./iit.el --source ~/icinga2 api-object   only tests matching a regexp
;;   ./iit.el --skip-build '(tag :cluster)'   reuse the last built image
;;   ./iit.el '(tag :unit)'                   tests that don't need podman
;;
;; Requires rootless podman.  See iit-lib.el for how clusters are set up.

(eval-and-compile
  (require 'ert)
  (load (expand-file-name "iit-lib" (file-name-directory
                                       (or load-file-name byte-compile-current-file buffer-file-name)))
        nil t))

(setq remote-file-name-inhibit-cache t)

;;;; Helpers

(defun api-object-path (type name)
  "Return the API path of the object of TYPE (e.g. `host') with NAME.
For services, NAME is the full name \"HOST!SERVICE\"."
  (format "v1/objects/%ss/%s" type (url-hexify-string name)))

(defun api-create-object (type name &optional node)
  "Create an object of TYPE (e.g. `host') with NAME through the API of NODE."
  (let ((response (iit-api-request 'PUT node (api-object-path type name)
                                   '((attrs . ((check_command . "dummy")))))))
    (should (equal (caar response) 'results))
    (should (equal (alist-get 'code (cadar response)) 200))))

(defun api-delete-object (type name &optional node)
  "Delete the object of TYPE (e.g. `host') with NAME through the API of NODE."
  (let ((response (iit-api-request 'DELETE node (api-object-path type name)
                                   '((cascade . t)))))
    (should (equal (caar response) 'results))
    (should (equal (alist-get 'code (cadar response)) 200))))

(defun api-object-exists-p (type name &optional node)
  "Return non-nil if the object of TYPE with NAME exists in the memory of NODE."
  (let ((response (iit-api-request 'GET node (api-object-path type name))))
    (alist-get 'results response)))

(defun api-objects-dir (type node)
  "Return the TRAMP name of the directory with API-created TYPE objects of NODE."
  (let* ((api-path (iit-node-file node "/var/lib/icinga2/api/packages/_api/"))
         (active-stage (with-temp-buffer
                         (insert-file-contents-literally
                          (expand-file-name "active-stage" api-path))
                         (string-trim (buffer-string)))))
    (expand-file-name (format "conf.d/%ss/" type) (expand-file-name active-stage api-path))))

(defun api-create-host (name &optional node)
  "Create a host with NAME through the API of NODE."
  (api-create-object 'host name node))

(defun api-delete-host (name &optional node)
  "Delete the host with NAME through the API of NODE."
  (api-delete-object 'host name node))

(defun api-host-exists-p (name &optional node)
  "Return non-nil if the host with NAME exists in the memory of NODE."
  (api-object-exists-p 'host name node))

(defun api-hosts-dir (node)
  "Return the TRAMP name of the directory with the API-created hosts of NODE."
  (api-objects-dir 'host node))

(defun api-create-service (host name &optional node)
  "Create a service with NAME on HOST through the API of NODE."
  (api-create-object 'service (concat host "!" name) node))

(defun api-delete-service (host name &optional node)
  "Delete the service with NAME on HOST through the API of NODE."
  (api-delete-object 'service (concat host "!" name) node))

(defun api-service-exists-p (host name &optional node)
  "Return non-nil if the service with NAME on HOST exists in the memory of NODE."
  (api-object-exists-p 'service (concat host "!" name) node))

(defun api-services-dir (node)
  "Return the TRAMP name of the directory with the API-created services of NODE."
  (api-objects-dir 'service node))

(defun load-config (hosts services)
  "Return config statements for HOSTS hosts with SERVICES services each.
They do nothing but make loading the config, and thus a reload, take longer."
  (let (forms)
    (dotimes (h hosts)
      (let ((host (format "load-host-%d" h)))
        (push `(object Host ,host (check_command "dummy") (enable_active_checks nil)) forms)
        (dotimes (s services)
          (push `(object Service ,(format "load-service-%d" s)
                         (host_name ,host) (check_command "dummy") (enable_active_checks nil))
                forms))))
    (nreverse forms)))

;;;; Unit tests for the test infrastructure

(ert-deftest config-render-object ()
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

(ert-deftest config-render-statements ()
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

(ert-deftest cluster-spec-parsing ()
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
  (should-error (iit-parse-cluster-spec '(master-1 :bogus t))))

(ert-deftest node-config-files ()
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
  (let ((cluster (iit-parse-cluster-spec '((m :features (nonexistent))))))
    (should-error (iit--node-config-files cluster (car (iit-cluster-nodes cluster))))))

;;;; Cluster tests

(ert-deftest api-object-creation-consistent-delete ()
  "Test if object creation through the API properly propagates through the cluster."
  :tags '(:cluster)
  (iit-with-cluster ((master-1 :zone "master")
                       (master-2 :zone "master"))
    (let ((num-objects 100))
      (cl-labels ((host-name (i)
                    (format "test-host-%d" i))
                  (last-host ()
                    (host-name (1- num-objects)))
                  (check-host-files (node exist)
                    (let ((dir (api-hosts-dir node)))
                      (dotimes (i num-objects)
                        (let ((file (expand-file-name (concat (host-name i) ".conf") dir)))
                          (if exist
                              (should (file-exists-p file))
                            (should-not (file-exists-p file))))))))
        ;; Both masters have been started by `iit-with-cluster' at this point.
        (dotimes (_ 2)
          (iit-log-mark)

          ;; We create a number of host objects and check if they get synced to both masters
          ;; properly.
          (dotimes (i num-objects)
            (api-create-host (host-name i) 'master-1))
          (dolist (node '(master-1 master-2))
            (iit-expect-log node (regexp-quote
                                    (format "Created and activated object '%s'" (last-host)))))
          (check-host-files 'master-1 t)
          (check-host-files 'master-2 t)

          ;; We now shut down master-2 so it doesn't get any updates of what is happening in the
          ;; cluster for a while.
          (iit-node-stop 'master-2)

          ;; With master-2 now unaware of what's going on, we delete the objects again via the API
          ;; and check that that has been properly executed.
          (dotimes (i num-objects)
            (api-delete-host (host-name i) 'master-1))
          (iit-expect-log 'master-1 (regexp-quote
                                       (format "Deleted object '%s' of type 'Host'" (last-host))))
          (check-host-files 'master-1 nil)

          ;; Now restart master-1 to ensure that the mechanism correctly persists the data across
          ;; restarts. This also sets master-1's log mark.
          (iit-node-restart 'master-1)

          ;; Now we start master-2 again, which will try to sync back some or all of the files.
          (iit-node-start 'master-2)
          (iit-expect-log 'master-1 (regexp-quote "Finished syncing endpoint 'master-2'"))
          (iit-expect-log 'master-2 (regexp-quote
                                       "Finished syncing runtime objects to endpoint 'master-1'."))

          ;; Check that the objects have not been recreated.
          (check-host-files 'master-1 nil)
          (check-host-files 'master-2 nil)

          ;; Make sure that master-1 has correctly thwarted the attempt to restore the old objects.
          (dotimes (i num-objects)
            (iit-expect-log 'master-1 (regexp-quote
                                         (format "Ignoring config update for deleted object '%s'"
                                                 (host-name i))))))

        ;; Now we stop master-1 to recreate the hosts properly on master-2.
        (iit-node-stop 'master-1)

        ;; We create a new set of objects on master-2 while master-1 is down.
        (iit-log-mark 'master-2)
        (dotimes (i num-objects)
          (api-create-host (host-name i) 'master-2))
        (iit-expect-log 'master-2 (regexp-quote
                                     (format "Created and activated object '%s'" (last-host))))
        (check-host-files 'master-2 t)

        ;; Then we start master-1 again and check if the objects get synced back successfully.
        (iit-node-start 'master-1)
        (iit-expect-log 'master-2 (regexp-quote "Finished syncing endpoint 'master-1'"))
        (iit-expect-log 'master-1 (regexp-quote
                                     "Finished syncing runtime objects to endpoint 'master-2'."))

        ;; Now for the final test we check if the objects have been recreated.
        (check-host-files 'master-1 t)
        (check-host-files 'master-2 t)))))

(ert-deftest api-object-delete-during-reload ()
  "Test that objects deleted on one master while the other one reloads stay deleted.

While a node reloads, its old worker keeps applying config updates from the
cluster, but the new worker has already read its config and doesn't see them.
Objects deleted in that window used to live on in the memory of the new worker
without a config file (\"ghost objects\", case A of Icinga/icinga2#11181)."
  :tags '(:cluster)
  (iit-with-cluster ((master-1 :zone "master")
                     ;; The large config makes master-2 take long enough to reload that many
                     ;; deletions happen during the reload.
                     (master-2 :zone "master"
                               :files (("conf.d/load.conf" ,@(load-config 5000 5)))))
    (let ((num-objects 300)
          (num-deleted 0))
      (cl-labels ((host-name (i)
                    (format "test-host-%d" i))
                  (last-host ()
                    (host-name (1- num-objects)))
                  (check-hosts (node)
                    ;; Hosts below num-deleted must be gone, all others must still exist, both
                    ;; in memory and as config files.
                    (let ((dir (api-hosts-dir node)))
                      (dotimes (i num-objects)
                        (let ((exists-p (api-host-exists-p (host-name i) node))
                              (file (expand-file-name (concat (host-name i) ".conf") dir)))
                          (if (< i num-deleted)
                              (progn
                                (should-not exists-p)
                                (should-not (file-exists-p file)))
                            (should exists-p)
                            (should (file-exists-p file))))))))
        ;; We create a number of host objects on master-1 and wait until both masters have them.
        (iit-log-mark)
        (dotimes (i num-objects)
          (api-create-host (host-name i) 'master-1))
        (dolist (node '(master-1 master-2))
          (iit-expect-log node (regexp-quote
                                (format "Created and activated object '%s'" (last-host)))))
        (check-hosts 'master-1)
        (check-hosts 'master-2)

        ;; Now we make master-2 reload and keep deleting hosts on master-1 until master-2's new
        ;; worker has taken over.  Some of these deletions are applied by master-2's old worker
        ;; after the new worker has already read its config.
        (iit-log-mark 'master-1)
        (iit-node-reload 'master-2)
        (while (not (iit-node-reloaded-p 'master-2))
          ;; If this fails, master-2 took longer to reload than expected.
          (should (< num-deleted num-objects))
          (api-delete-host (host-name num-deleted) 'master-1)
          (setq num-deleted (1+ num-deleted))
          (sleep-for 0.05))
        (message "Deleted %d hosts while master-2 was reloading" num-deleted)

        ;; If this fails, master-2 reloaded too fast for the test to be meaningful.
        (should (>= num-deleted 20))

        ;; Deletions that master-2's old worker didn't get anymore reach the new worker through
        ;; the replay log after reconnecting.
        (iit-expect-log 'master-1 (regexp-quote "Finished syncing endpoint 'master-2'"))
        (iit-expect-log 'master-2 (regexp-quote
                                   "Finished syncing runtime objects to endpoint 'master-1'."))
        (iit-expect-log 'master-2 (regexp-quote
                                   (format "Deleted object '%s' of type 'Host'"
                                           (host-name (1- num-deleted)))))

        ;; Check that the deleted hosts are gone and all others still exist on both masters.
        (check-hosts 'master-1)
        (check-hosts 'master-2)))))

;; With EMACS_TEST_JUNIT_REPORT set, ERT writes a JUnit report to iit.xml.  It finds this file
;; through `load-history', which Emacs only updates once the file is completely loaded.
(when load-file-name
  (push (cons load-file-name current-load-list) load-history))

(ert-run-tests-batch-and-exit (iit-parse-command-line command-line-args-left))
