;;; iit-cluster.el --- Integration tests for Icinga 2 clusters -*- lexical-binding: t -*-

;; SPDX-FileCopyrightText: 2026 Icinga GmbH <https://icinga.com>
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Tests that run against real Icinga 2 clusters in podman containers, see iit-lib.el.  They are
;; tagged :cluster.

;;; Code:

(require 'ert)
(require 'iit-lib)

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

(defun api-modify-object (type name attrs &optional node)
  "Set ATTRS, an alist, on the object of TYPE with NAME through the API of NODE."
  (let ((response (iit-api-request 'POST node (api-object-path type name)
                                   `((attrs . ,attrs)))))
    (should (equal (caar response) 'results))
    (should (equal (alist-get 'code (cadar response)) 200))))

(defun api-object-attr (type name attr &optional node)
  "Return the value of ATTR (a symbol) of the object of TYPE with NAME.
The value is the one in the memory of NODE."
  (let ((response (iit-api-request 'GET node (format "%s?attrs=%s" (api-object-path type name)
                                                     attr))))
    (alist-get attr (alist-get 'attrs (car (alist-get 'results response))))))

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

(defun api-modify-host (name attrs &optional node)
  "Set ATTRS, an alist, on the host with NAME through the API of NODE."
  (api-modify-object 'host name attrs node))

(defun api-host-attr (name attr &optional node)
  "Return the value of ATTR (a symbol) of the host with NAME in the memory of NODE."
  (api-object-attr 'host name attr node))

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

(defun zone-host-files (zone count)
  "Return :files entries for zones.d/ZONE with COUNT hosts, one file per host."
  (let (files)
    (dotimes (i count)
      (let ((host (format "zone-host-%d" i)))
        (push `(,(format "zones.d/%s/hosts/%s.conf" zone host)
                (object Host ,host (check_command "dummy") (enable_active_checks nil)))
              files)))
    (nreverse files)))

(defun api-object-count (type node)
  "Return the number of objects of TYPE (e.g. `host') in the memory of NODE."
  (length (alist-get 'results (iit-api-request 'GET node (format "v1/objects/%ss?attrs=name"
                                                                type)))))

(defun config-update-sizes (node zone)
  "Return the sizes of the config updates for ZONE that NODE applied.
Only updates since NODE's log mark count.  The sizes are in bytes, oldest
update first."
  (let ((log (iit-node-log node))
        (regexp (concat "Applying configuration file update for path '[^']*/"
                        (regexp-quote zone) "' (\\([0-9]+\\) Bytes)"))
        (start 0)
        sizes)
    (while (string-match regexp log start)
      (push (string-to-number (match-string 1 log)) sizes)
      (setq start (match-end 0)))
    (nreverse sizes)))

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

(ert-deftest api-object-create-and-delete-during-reload ()
  "Test that objects created and deleted while a master reloads leave no files behind.

While a node reloads, its old worker keeps applying config updates from the
cluster, but the new worker has already read its config and doesn't see them.
An object created in that window gets a config file but doesn't exist in the
memory of the new worker.  When the new worker then gets the deletion, there is
no object to delete, so the file used to stay behind.  The deleted object then
came back at the next reload (case B of Icinga/icinga2#11181).  If it was a
service whose host was deleted, the next reload failed validation instead
\(case C)."
  :tags '(:cluster)
  (iit-with-cluster ((master-1 :zone "master")
                     ;; The large config makes master-2 take long enough to reload that many
                     ;; services are created and deleted during the reload.
                     (master-2 :zone "master"
                               :files (("conf.d/load.conf" ,@(load-config 5000 30)))))
    (let ((num-hosts 300)
          ;; Each service is deleted this many iterations of the loop below after it was created,
          ;; which is about one to two seconds.
          (delete-lag 20)
          (num-created 0)
          (num-deleted 0))
      (cl-labels ((host-name (i)
                    (format "test-host-%d" i))
                  (last-host ()
                    (host-name (1- num-hosts)))
                  (delete-next-service ()
                    (api-delete-service (host-name num-deleted) "test-service" 'master-1)
                    (setq num-deleted (1+ num-deleted)))
                  (check-no-test-objects (node)
                    ;; Neither the hosts nor their services may exist, in memory or as config files.
                    (dotimes (i num-hosts)
                      (should-not (api-host-exists-p (host-name i) node))
                      (should-not (api-service-exists-p (host-name i) "test-service" node)))
                    (should-not (directory-files (api-hosts-dir node) nil "^test-host-"))
                    (should-not (directory-files (api-services-dir node) nil "^test-host-"))))
        ;; We create a number of host objects on master-1 and wait until both masters have them.
        (iit-log-mark)
        (dotimes (i num-hosts)
          (api-create-host (host-name i) 'master-1))
        (dolist (node '(master-1 master-2))
          (iit-expect-log node (regexp-quote
                                (format "Created and activated object '%s'" (last-host)))))

        ;; Now we make master-2 reload and, until master-2's new worker has taken over, keep
        ;; creating a service on master-1 and deleting it again a little later.  Some services are
        ;; created by master-2's old worker after the new worker has already read its config, and
        ;; then deleted on the new worker.
        (iit-node-reload 'master-2)
        (while (not (iit-node-reloaded-p 'master-2))
          ;; If this fails, master-2 took longer to reload than expected.
          (should (< num-created num-hosts))
          (api-create-service (host-name num-created) "test-service" 'master-1)
          (setq num-created (1+ num-created))
          (when (>= num-created delete-lag)
            (delete-next-service))
          (sleep-for 0.05))
        (message "Created %d services while master-2 was reloading" num-created)

        ;; If this fails, master-2 reloaded too fast for the test to be meaningful.
        (should (>= (- num-created delete-lag) 20))

        ;; Once master-2 has reconnected, we delete the remaining services, and then all hosts.
        (iit-expect-log 'master-1 (regexp-quote "Finished syncing endpoint 'master-2'"))
        (iit-expect-log 'master-2 (regexp-quote
                                   "Finished syncing runtime objects to endpoint 'master-1'."))
        (while (< num-deleted num-created)
          (delete-next-service))
        (iit-log-mark)
        (dotimes (i num-hosts)
          (api-delete-host (host-name i) 'master-1))
        (dolist (node '(master-1 master-2))
          (iit-expect-log node (regexp-quote
                                (format "Deleted object '%s' of type 'Host'" (last-host)))))

        ;; Check that none of the hosts and services are left on either master.  Leftover service
        ;; files are what makes them come back (case B) or break the config (case C).
        (check-no-test-objects 'master-1)
        (check-no-test-objects 'master-2)

        ;; Finally, master-2 must still be able to reload, and no deleted objects may come back.
        (iit-node-reload 'master-2)
        (iit-expect-log 'master-2 iit-ready-regexp)
        (check-no-test-objects 'master-2)))))

(ert-deftest api-object-modify-during-reload ()
  "Test that objects created and modified while a master reloads keep the changes.

While a node reloads, its old worker keeps applying config updates from the
cluster, but the new worker has already read its config and doesn't see them.
An object created in that window doesn't exist in the memory of the new worker.
When the new worker then gets an update for it, it creates the object from its
config, but used to discard the modified attributes that came with the update.
The object then kept its initial attributes, even after the next reload (case E
of Icinga/icinga2#11181)."
  :tags '(:cluster)
  (iit-with-cluster ((master-1 :zone "master")
                     ;; The large config makes master-2 take long enough to reload that many hosts
                     ;; are created and modified during the reload.
                     (master-2 :zone "master"
                               :files (("conf.d/load.conf" ,@(load-config 5000 30)))))
    (let ((num-hosts 300)
          ;; Each host is modified this many iterations of the loop below after it was created,
          ;; which is about one to two seconds.
          (modify-lag 20)
          (num-created 0)
          (num-modified 0))
      (cl-labels ((host-name (i)
                    (format "test-host-%d" i))
                  (display-name (i)
                    (format "Modified test host %d" i))
                  (modify-next-host ()
                    (api-modify-host (host-name num-modified)
                                     `((display_name . ,(display-name num-modified)))
                                     'master-1)
                    (setq num-modified (1+ num-modified)))
                  (check-hosts (node)
                    ;; All hosts that were created must have the modified display name.
                    (dotimes (i num-created)
                      (should (equal (api-host-attr (host-name i) 'display_name node)
                                     (display-name i))))))
        ;; We make master-2 reload and, until master-2's new worker has taken over, keep creating a
        ;; host on master-1 and modifying it a little later.  Some hosts are created by master-2's
        ;; old worker after the new worker has already read its config, and then modified on the
        ;; new worker.
        (iit-node-reload 'master-2)
        (while (not (iit-node-reloaded-p 'master-2))
          ;; If this fails, master-2 took longer to reload than expected.
          (should (< num-created num-hosts))
          (api-create-host (host-name num-created) 'master-1)
          (setq num-created (1+ num-created))
          (when (>= num-created modify-lag)
            (modify-next-host))
          (sleep-for 0.05))
        (message "Created %d hosts while master-2 was reloading" num-created)

        ;; If this fails, master-2 reloaded too fast for the test to be meaningful.
        (should (>= (- num-created modify-lag) 20))

        ;; Once master-2 has reconnected, we modify the remaining hosts.
        (iit-expect-log 'master-1 (regexp-quote "Finished syncing endpoint 'master-2'"))
        (iit-expect-log 'master-2 (regexp-quote
                                   "Finished syncing runtime objects to endpoint 'master-1'."))
        (while (< num-modified num-created)
          (modify-next-host))

        ;; Cluster messages are processed in order, so once master-2 has a host created after
        ;; all modifications, it has processed the modifications as well.
        (iit-log-mark 'master-2)
        (api-create-host "sync-marker" 'master-1)
        (iit-expect-log 'master-2 (regexp-quote "Created and activated object 'sync-marker'"))

        ;; Check that the modifications have been applied on both masters.
        (check-hosts 'master-1)
        (check-hosts 'master-2)

        ;; The modified attributes must survive the next reload of master-2 as well.
        (iit-node-reload 'master-2)
        (iit-expect-log 'master-2 iit-ready-regexp)
        (check-hosts 'master-2)))))

(ert-deftest config-sync-during-activation ()
  "Test that a master never syncs a partially copied zone config to a satellite.

When a master has validated a zone config it received from the config master,
it replaces its production zones directory with the new config file by file.
Satellites that connect in the meantime get the config from that directory,
which used to be read without waiting for the copy to finish.  So they got an
incomplete config, but with the new timestamp, which is copied first.  They
activated it, and then ignored the complete config with the same timestamp until
the config changed again (Icinga/icinga2#11175)."
  :tags '(:cluster)
  ;; master-2 copies the zone config file by file and logs every file.  Throttling that log
  ;; makes the copy take long enough for the satellite to start and connect: about 15 s, as the
  ;; first few hundred files are copied without waiting.
  (let ((num-hosts 2000))
    (iit-with-cluster ((master-1 :zone "master"
                                 :files ,(zone-host-files "satellite" num-hosts))
                       (master-2 :zone "master"
                                 :log-throttle ("Copying file '" 0.01))
                       (satellite :parent "master")
                       :start nil)
      ;; master-2 gets the zone config from master-1, validates it and copies it into its
      ;; production zones directory.
      (iit-node-start 'master-1 'master-2)
      (iit-expect-log 'master-2 (regexp-quote
                                 "from endpoint 'master-1' are different to production"))

      ;; The satellite must not get the zone config from master-1, which always sends a complete
      ;; one.  If that one came first, the satellite would ignore the incomplete one from master-2,
      ;; which has the same timestamp.
      (iit-node-stop 'master-1)

      ;; The satellite is started once master-2 has started copying, so that it connects while
      ;; master-2's production zones directory is incomplete.
      (iit-expect-log 'master-2 "Config validation for stage '[^']*' was OK, replacing into")
      (iit-node-start 'satellite)
      (iit-expect-log 'satellite "Received configuration updates ([0-9]+) from endpoint 'master-2'")

      ;; Once master-2's new worker has taken over, the old one has written its complete log.
      (iit-expect-log 'master-2 iit-ready-regexp 2)

      ;; If this fails, the satellite connected too late (or too early) for the test to be
      ;; meaningful: master-2 must still have copied many files after it started to sync the
      ;; satellite.  A longer delay per file in :log-throttle makes the copy take longer.
      (with-temp-buffer
        (insert (iit-node-log 'master-2))
        (goto-char (point-min))
        (should (search-forward "Sending config updates for endpoint 'satellite'" nil t))
        (let ((copied-before (how-many (regexp-quote "Copying file '") (point-min) (point)))
              (copied-after (how-many (regexp-quote "Copying file '") (point) (point-max))))
          (message "master-2 copied %d files before and %d after it started to sync the satellite"
                   copied-before copied-after)
          (should (> copied-before 0))
          (should (> copied-after (/ num-hosts 10)))))

      ;; The satellite must have got the same config as master-2.
      (should (equal (car (config-update-sizes 'satellite "satellite"))
                     (car (config-update-sizes 'master-2 "satellite"))))

      ;; The satellite reloads with the config it got and then gets the complete config again from
      ;; master-2, which it considers equal.  It must have all hosts then.
      (iit-expect-log 'satellite iit-ready-regexp 2)
      (iit-expect-log 'satellite (regexp-quote "from endpoint 'master-2' are equal to production"))
      (should (= (api-object-count 'host 'satellite) num-hosts)))))

(provide 'iit-cluster)
;;; iit-cluster.el ends here
