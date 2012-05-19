;;; websocket-test.el --- Unit tests for the websocket layer

;; Copyright (c) 2010 Andrew Hyatt
;;
;; Author: Andrew Hyatt <ahyatt at gmail dot com>
;; Maintainer: Andrew Hyatt <ahyatt at gmail dot com>
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
;; 02110-1301, USA.

;;; Commentary:
;; This defines and runs ert unit tests.  You can download ert from:
;; http://github.com/ohler/ert, it also comes with Emacs 24 and above.

(require 'ert)
(require 'websocket)
(eval-when-compile (require 'cl))

(defun websocket-test-get-filtered-response-with-error
  (outputs &optional callback)
  (let* ((packet-data nil)
         (websocket
          (make-websocket :conn "fake-conn"
                          :filter (lambda (packet)
                                    (push packet packet-data)
                                    (when callback (funcall callback)))
                          :close-callback (lambda (not-called) (assert nil))
                          :url "ws://foo/bar"
                          :v75 nil))
         err-list)
    (dolist (output outputs)
      (condition-case err
          (websocket-outer-filter websocket output)
        (error (push err err-list))))
    (list (nreverse packet-data) (nreverse err-list))))

(defun websocket-test-get-filtered-response (outputs)
  (destructuring-bind (packet-data err-list)
      (websocket-test-get-filtered-response-with-error outputs)
    (assert (eq (length err-list) 0))
    packet-data))


(ert-deftest websocket-genbytes-length ()
  (loop repeat 100
        do (should (= (string-bytes (websocket-genbytes)) 8))))

(ert-deftest websocket-filter-basic ()
  (should (equal
           '("foo")
           (websocket-test-get-filtered-response '("\0foo\377"))))
  (should (equal
           '("foo" "bar")
           (websocket-test-get-filtered-response
            '("\0foo\377\0bar\377"))))
  (should (equal
           '("foo" "bar")
           (websocket-test-get-filtered-response
            '("\0foo\377" "\0bar\377")))))

(ert-deftest websocket-filter-inflight-packets ()
  (should (equal
           '("foo" "bar")
           (websocket-test-get-filtered-response
            '("\0foo\377\0b" "a" "r\377"))))
  (should (equal
           '("foo" "bar")
           (websocket-test-get-filtered-response
            '("\0foo\377\0ba" "r\377baz")))))

(ert-deftest websocket-filter-first-response ()
  (should (equal
           '("foo" "bar")
           (websocket-test-get-filtered-response
            '("HTTP 1.1\0foo\377\0bar\377"))))
  (should (equal
           '("foo")
           (websocket-test-get-filtered-response
            '("HTTP 1.1" "\0foo\377")))))

(ert-deftest websocket-filter-handle-error-in-filter ()
  (destructuring-bind (packet-data err-list)
      (websocket-test-get-filtered-response-with-error
       '("\0foo\377\0bar\377")
       (lambda () (error "See if websocket can handle this")))
    (should (equal packet-data '("foo" "bar")))
    (should (equal err-list nil)))
  (destructuring-bind (packet-data err-list)
      (websocket-test-get-filtered-response-with-error
       '("\0foo\377\0bar\377")
       (lambda () "Raise another type of error" (/ 1 0)))
    (should (equal packet-data '("foo" "bar")))
    (should (equal err-list nil)))
  (destructuring-bind (packet-data err-list)
      (websocket-test-get-filtered-response-with-error
       '("\0foo\377" "\0bar\377")
       (lambda () (error "See if websocket can handle this")))
    (should (equal packet-data '("foo" "bar")))
    (should (equal err-list nil))))

(ert-run-tests-interactively 'websocket-genbytes-length)
(ert-run-tests-interactively 'websocket-filter-basic)
(ert-run-tests-interactively 'websocket-filter-inflight-packets)
(ert-run-tests-interactively 'websocket-filter-first-response)
(ert-run-tests-interactively 'websocket-filter-handle-error-in-filter)
