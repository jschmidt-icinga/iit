#!/usr/bin/env -S emacs -Q --script
;; -*- lexical-binding: t mode: emacs-lisp -*-

;; Integration tests for Icinga 2 config sync.
;;
;; Usage: ./tests.el [--source DIR] [--image NAME] [--skip-build] [--keep] [--verbose] [SELECTOR]
;;
;;   ./tests.el                                 build ../icinga2 and run all tests
;;   ./tests.el --source ~/icinga2 api-object   only tests matching a regexp
;;   ./tests.el --skip-build '(tag :cluster)'   reuse the last built image
;;   ./tests.el '(tag :unit)'                   tests that don't need podman
;;
;; Requires rootless podman.  See tests-lib.el for how clusters are set up.

(eval-and-compile
  (require 'ert)
  (load (expand-file-name "tests-lib" (file-name-directory
                                       (or load-file-name byte-compile-current-file buffer-file-name)))
        nil t))

(setq remote-file-name-inhibit-cache t)

;;;; Helpers

(defun api-create-object (name &optional node)
  "Create a host with NAME through the API of NODE."
  (let ((response (tests-api-request 'PUT node (concat "v1/objects/hosts/" (url-hexify-string name))
                                     '((attrs . ((check_command . "dummy")))))))
    (should (equal (caar response) 'results))
    (should (equal (alist-get 'code (cadar response)) 200))))

(defun api-delete-object (name &optional node)
  "Delete the host with NAME through the API of NODE."
  (let ((response (tests-api-request 'DELETE node (concat "v1/objects/hosts/" (url-hexify-string name))
                                     '((cascade . t)))))
    (should (equal (caar response) 'results))
    (should (equal (alist-get 'code (cadar response)) 200))))

;;;; Unit tests for the test infrastructure

(ert-deftest config-render-object ()
  :tags '(:unit)
  (should (equal (tests-config-render
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
  (should (equal (tests-config-render
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
  (should-error (tests-config-render '(bogus 1)))
  (should-error (tests-config-render '(object Host "h" (vars :unknown)))))

(ert-deftest cluster-spec-parsing ()
  :tags '(:unit)
  (let* ((n 2)
         (spec `((master-1 :zone "master")
                 (master-2 :zone "master")
                 ,@(mapcar (lambda (i) `(,(intern (format "sat-%d" i)) :zone "sat" :parent "master"))
                           (number-sequence 1 n))
                 agent-1
                 :global-zones ("global-templates")))
         (cluster (tests-parse-cluster-spec spec)))
    (should (equal (mapcar #'tests-node-name (tests-cluster-nodes cluster))
                   '("master-1" "master-2" "sat-1" "sat-2" "agent-1")))
    (should (equal (tests--cluster-zones cluster)
                   '(("master" nil "master-1" "master-2")
                     ("sat" "master" "sat-1" "sat-2")
                     ("agent-1" nil "agent-1"))))
    (should (string-match-p (regexp-quote "object Zone \"global-templates\" {\n  global = true\n}")
                            (apply #'tests-config-render (tests--zones-conf cluster)))))
  (should-error (tests-parse-cluster-spec '((master-1 :zoen "master"))))
  (should-error (tests-parse-cluster-spec '(master-1 master-1)))
  (should-error (tests-parse-cluster-spec '((sat-1 :parent "master"))))
  (should-error (tests-parse-cluster-spec '((a :zone "z" :parent "x") (b :zone "z" :parent "y") x y)))
  (should-error (tests-parse-cluster-spec '(master-1 :bogus t))))

(ert-deftest node-config-files ()
  :tags '(:unit)
  (let* ((cluster (tests-parse-cluster-spec
                   '((master-1 :zone "master"
                               :features (api debuglog)
                               :files (("zones.d/master/hosts.conf"
                                        (object Host "h1" (check_command "dummy")))
                                       ("conf.d/api-users.conf" "// replaced")))
                     (master-2 :zone "master"))))
         (files (tests--node-config-files cluster (car (tests-cluster-nodes cluster)))))
    (should (string-match-p "const NodeName = \"master-1\"" (alist-get "constants.conf" files nil nil #'equal)))
    (should (equal (mapcar #'car (seq-filter (lambda (f) (string-prefix-p "features-enabled/" (car f))) files))
                   '("features-enabled/api.conf" "features-enabled/debuglog.conf")))
    (should (equal (cdr (assoc "zones.d/master/hosts.conf" files))
                   "object Host \"h1\" {\n  check_command = \"dummy\"\n}\n"))
    ;; Later entries win when the files are written.
    (should (equal (cdr (car (last files))) "// replaced\n")))
  (let ((cluster (tests-parse-cluster-spec '((m :features (nonexistent))))))
    (should-error (tests--node-config-files cluster (car (tests-cluster-nodes cluster))))))

;;;; Cluster tests

(ert-deftest api-object-creation-consistent-delete ()
  "Test if object creation through the API properly propagates through the cluster."
  :tags '(:cluster)
  (tests-with-cluster ((master-1 :zone "master")
                       (master-2 :zone "master"))
    (let ((num-objects 100))
      (cl-labels ((host-name (i)
                    (format "test-host-%d" i))
                  (last-host ()
                    (host-name (1- num-objects)))
                  (hosts-dir (node)
                    (let* ((api-path (tests-node-file node "/var/lib/icinga2/api/packages/_api/"))
                           (active-stage (with-temp-buffer
                                           (insert-file-contents-literally
                                            (expand-file-name "active-stage" api-path))
                                           (string-trim (buffer-string)))))
                      (expand-file-name "conf.d/hosts/"
                                        (expand-file-name active-stage api-path))))
                  (check-host-files (node exist)
                    (let ((dir (hosts-dir node)))
                      (dotimes (i num-objects)
                        (let ((file (expand-file-name (concat (host-name i) ".conf") dir)))
                          (if exist
                              (should (file-exists-p file))
                            (should-not (file-exists-p file))))))))
        ;; Both masters have been started by `tests-with-cluster' at this point.
        (dotimes (_ 2)
          (tests-log-mark)

          ;; We create a number of host objects and check if they get synced to both masters
          ;; properly.
          (dotimes (i num-objects)
            (api-create-object (host-name i) 'master-1))
          (dolist (node '(master-1 master-2))
            (tests-expect-log node (regexp-quote
                                    (format "Created and activated object '%s'" (last-host)))))
          (check-host-files 'master-1 t)
          (check-host-files 'master-2 t)

          ;; We now shut down master-2 so it doesn't get any updates of what is happening in the
          ;; cluster for a while.
          (tests-node-stop 'master-2)

          ;; With master-2 now unaware of what's going on, we delete the objects again via the API
          ;; and check that that has been properly executed.
          (dotimes (i num-objects)
            (api-delete-object (host-name i) 'master-1))
          (tests-expect-log 'master-1 (regexp-quote
                                       (format "Deleted object '%s' of type 'Host'" (last-host))))
          (check-host-files 'master-1 nil)

          ;; Now restart master-1 to ensure that the mechanism correctly persists the data across
          ;; restarts. This also sets master-1's log mark.
          (tests-node-restart 'master-1)

          ;; Now we start master-2 again, which will try to sync back some or all of the files.
          (tests-node-start 'master-2)
          (tests-expect-log 'master-1 (regexp-quote "Finished syncing endpoint 'master-2'"))
          (tests-expect-log 'master-2 (regexp-quote
                                       "Finished syncing runtime objects to endpoint 'master-1'."))

          ;; Check that the objects have not been recreated.
          (check-host-files 'master-1 nil)
          (check-host-files 'master-2 nil)

          ;; Make sure that master-1 has correctly thwarted the attempt to restore the old objects.
          (dotimes (i num-objects)
            (tests-expect-log 'master-1 (regexp-quote
                                         (format "Ignoring config update for deleted object '%s'"
                                                 (host-name i))))))

        ;; Now we stop master-1 to recreate the hosts properly on master-2.
        (tests-node-stop 'master-1)

        ;; We create a new set of objects on master-2 while master-1 is down.
        (tests-log-mark 'master-2)
        (dotimes (i num-objects)
          (api-create-object (host-name i) 'master-2))
        (tests-expect-log 'master-2 (regexp-quote
                                     (format "Created and activated object '%s'" (last-host))))
        (check-host-files 'master-2 t)

        ;; Then we start master-1 again and check if the objects get synced back successfully.
        (tests-node-start 'master-1)
        (tests-expect-log 'master-2 (regexp-quote "Finished syncing endpoint 'master-1'"))
        (tests-expect-log 'master-1 (regexp-quote
                                     "Finished syncing runtime objects to endpoint 'master-2'."))

        ;; Now for the final test we check if the objects have been recreated.
        (check-host-files 'master-1 t)
        (check-host-files 'master-2 t)))))

(ert-run-tests-batch-and-exit (tests-parse-command-line command-line-args-left))
