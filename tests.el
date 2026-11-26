#!/usr/bin/env -S emacs -Q --script
;; -*- lexical-binding: t mode: emacs-lisp -*-

(require 'url)
(require 'ert)

(setq default-directory (file-name-directory load-file-name)
      network-security-level 'low
      remote-file-name-inhibit-cache t)

(defun tests-start-container (name port)
  "")

;; TODO: Generate
(defun icinga-endpoint-generate-zones-conf ()
  "Generate a zones.conf for ENDPOINT in ZONE")

(defun http-request (method url &optional body username password)
  "Send an http-request using METHOD to URL.
The request can have an optional BODY and use basic authentication with
USERNAME and PASSWORD."
  (let ((url-request-method (symbol-name method))
        (url-mime-accept-string "application/json")
        (url-request-extra-headers
         `(("Content-Type" . "application/json")
           ,@(when (and username password)
               `(("Authorization" .
                  ,(concat "Basic "
                           (base64-encode-string
                            (concat username ":" password) t)))))))
        (url-request-data (when body (encode-coding-string (json-encode body) 'utf-8))))
    (with-current-buffer (url-retrieve-synchronously url t t)
      (goto-char url-http-end-of-headers)
      (json-parse-buffer :object-type 'alist :array-type 'list))))

(defun api-create-object (name &optional host)
  "Create an object with NAME in the icinga api of HOST."
  (unless host (setq host "localhost:5665"))
  (let ((response (http-request 'PUT (concat "https://" host "/v1/objects/hosts/" name)
                                    '((attrs . ((check_command . "dummy"))))
                                    "root" "icinga")))
        (should (equal (caar response) 'results))
        (should (equal (alist-get 'code (cadar response)) 200))))

(defun api-delete-object (name &optional host)
  "Create an object with NAME in the icinga api of HOST."
  (unless host (setq host "localhost:5665"))
  (let ((response (http-request 'DELETE (concat "https://" host "/v1/objects/hosts/" name)
                                '((cascade . t))
                                "root" "icinga")))
        (should (equal (caar response) 'results))
        (should (equal (alist-get 'code (cadar response)) 200))))

(defun clear-log (container)
  "Clear the icinga2 log-file for CONTAINER."
  (with-temp-file (concat "/docker:" container ":/var/log/icinga2/icinga2.log")
    (erase-buffer)))

(defun expect-log-pattern (container regexp &optional num)
  "Check if the log pattern REGEXP appears for CONTAINER NUM times with in a timeout."
  (with-temp-buffer
    (let ((start-time (current-time)))
      (while (not (re-search-forward regexp nil t num))
        (should (time-less-p (current-time) (time-add start-time (seconds-to-time 50))))
        (sleep-for 0.1)
        (insert-file-contents-literally (concat "/docker:" container ":/var/log/icinga2/icinga2.log")
                                        nil nil nil t)
        (goto-char (point-min))))))

(ert-deftest simple-reload-instance ()
  "Test if an instance can be reloaded after some activity without errors or dying.")

(ert-deftest api-object-creation-consistent-delete ()
  "Test if object creation through the API properly propagates through the cluster."
  (cl-labels ((get-host-file-name (container num)
                (let* ((api-path (concat "/docker:" container
                                         "://var/lib/icinga2/api/packages/_api/"))
                       (active-stage (with-temp-buffer
                                       (insert-file-contents-literally
                                        (expand-file-name "active-stage" api-path))
                                       (buffer-string)))
                       (hosts-path (expand-file-name "conf.d/hosts"
                                                     (expand-file-name active-stage
                                                                       api-path))))
                  (expand-file-name (concat "test-host-" (number-to-string num) ".conf")
                                    hosts-path))))
    (let ((num-objects 100))
      ;; We start up both masters, wait for them to sync and report that they are fully started
      ;; up. We then clear both their logs to not confuse the subsequent asserts on the logs.
      (should (zerop (call-process "docker" nil nil nil "compose" "down" "-v")))
      (should (zerop (call-process "docker" nil nil nil "compose" "up" "-d")))
      (expect-log-pattern "master-1" "'api' started")
      (expect-log-pattern "master-2" "'api' started" 2)

      (dotimes (_ 2)
        (clear-log "master-2")
        (clear-log "master-1")

        ;; We create a number of host objects and check if they get synced to both masters
        ;; properly
        (dotimes (i num-objects)
          (api-create-object (concat "test-host-" (number-to-string i))))
        (dolist (host '("master-1" "master-2"))
          (expect-log-pattern host (concat "Created and activated object 'test-host-"
                                           (number-to-string (1- num-objects))
                                           "'")))
        (dotimes (i num-objects)
          (should (file-exists-p (get-host-file-name "master-1" i)))
          (should (file-exists-p (get-host-file-name "master-2" i))))

        ;; We now shut down master-2 so it doesn't get any updates of what is happening in the
        ;; cluster for a while.
        (should (zerop (call-process "docker" nil nil nil "stop" "master-2")))

        ;; With master-2 now unaware of what's going on, we delete the objects again via the API
        ;; and check that that has been properly executed.
        (dotimes (i num-objects)
          (api-delete-object (concat "test-host-" (number-to-string i))))
        (expect-log-pattern "master-1" (concat "Deleted object 'test-host-"
                                               (number-to-string (1- num-objects))
                                               "' of type 'Host'"))
        (dotimes (i num-objects)
          (should-not (file-exists-p (get-host-file-name "master-1" i))))

        ;; Now restart master-1 to ensure that the mechanism correctly persists the data across
        ;; restarts.
        (clear-log "master-1")
        (should (zerop (call-process "docker" nil nil nil "restart" "master-1")))
        (expect-log-pattern "master-1" "'api' started")

        ;; Now we restart master-2, which will try to sync back some or all of the files.
        (should (zerop (call-process "docker" nil nil nil "start" "master-2")))
        (expect-log-pattern "master-1" "Finished syncing endpoint 'master-2'")
        (expect-log-pattern "master-2" "Finished syncing runtime objects to endpoint 'master-1'.")

        ;; Now for the final test we check if the objects have been recreated.
        (dotimes (i num-objects)
          (should-not (file-exists-p (get-host-file-name "master-1" i)))
          (should-not (file-exists-p (get-host-file-name "master-2" i))))

        ;; Make sure that master-1 has correctly thwarted the attempt to restore the old objects.
        (dotimes (i num-objects)
          (expect-log-pattern "master-1" (concat "Ignoring config update for deleted object "
                                                 "'test-host-" (number-to-string i) "'"))))

      ;; Now we stop master-1 to recreate the hosts properly on master-2
      (clear-log "master-1")
      (should (zerop (call-process "docker" nil nil nil "stop" "master-1")))

      ;; We create a new set of objects on master-2 while master-1 is down.
      (dotimes (i num-objects)
        (api-create-object (concat "test-host-" (number-to-string i))
                           "localhost:15665"))
      (expect-log-pattern "master-2" (concat "Created and activated object 'test-host-"
                                             (number-to-string (1- num-objects)) "'"))
      (dotimes (i num-objects)
        (should (file-exists-p (get-host-file-name "master-2" i))))

      ;; Then we start master-1 again and check if the objects get synced back successfully.
      (should (zerop (call-process "docker" nil nil nil "start" "master-1")))
      (expect-log-pattern "master-2" "Finished syncing endpoint 'master-1'")
      (expect-log-pattern "master-1" "Finished syncing runtime objects to endpoint 'master-2'.")

      ;; Now for the final test we check if the objects have been recreated.
      (dotimes (i num-objects)
        (should (file-exists-p (get-host-file-name "master-1" i)))
        (should (file-exists-p (get-host-file-name "master-2" i)))))))



(ert-run-tests-batch-and-exit (car command-line-args-left))
