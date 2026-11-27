#!/usr/bin/env -S emacs -Q --script
;; -*- lexical-binding: t mode: emacs-lisp -*-

;; SPDX-FileCopyrightText: 2026 Icinga GmbH <https://icinga.com>
;; SPDX-License-Identifier: GPL-3.0-or-later

;; Command line interface for the Icinga 2 integration tests.
;;
;; Usage: ./iit.el [--source DIR] [--image NAME] [--skip-build] [--keep] [--verbose] [SELECTOR]
;;
;;   ./iit.el                                 build ../icinga2 and run all tests
;;   ./iit.el --source ~/icinga2 api-object   only tests matching a regexp
;;   ./iit.el --skip-build '(tag :cluster)'   reuse the last built image
;;   ./iit.el '(tag :unit)'                   tests that don't need podman
;;
;; Requires rootless podman.  See iit-lib.el for how clusters are set up.
;;
;; The iit-*.el files are plain Emacs Lisp files, so the tests can also be run without this
;; script, with the settings in iit-lib.el at their defaults:
;;
;;   emacs -Q --batch -L . -l iit-cluster -f ert-run-tests-batch-and-exit

(eval-and-compile
  (add-to-list 'load-path (file-name-directory
                           (or load-file-name byte-compile-current-file buffer-file-name))))

(require 'ert)
(require 'iit-lib)
(require 'iit-cluster)

;;;; Command line

(defun iit-parse-command-line (args)
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
                 (setq iit-icinga2-source (expand-file-name value))
               (setq iit-image value))))
          ("--skip-build" (setq iit-build-image nil))
          ("--keep" (setq iit-keep-on-failure t))
          ("--verbose" (setq iit-verbose t))
          ((pred (string-prefix-p "--")) (error "Unknown option %s" arg))
          (_
           (when selector
             (error "Only one test selector can be given"))
           (setq selector (if (string-match-p "\\`\\(?:[(:]\\|t\\'\\)" arg)
                              (car (read-from-string arg))
                            arg))))))
    (or selector t)))

;; With EMACS_TEST_JUNIT_REPORT set, ERT writes a JUnit report named after this file, iit.xml.
(let ((ert-load-file-name load-file-name))
  (ert-run-tests-batch-and-exit (iit-parse-command-line command-line-args-left)))
