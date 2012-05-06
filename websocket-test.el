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
(require 'cl)

(defun websocket-test-get-filtered-response (outputs)
  (let ((packet-data nil)
        (websocket
         (make-websocket :conn "fake-conn"
                         :filter (lambda (packet) (push packet packet-data))
                         :close-callback (lambda (not-called) (assert nil))
                         :url "ws://foo/bar"
                         :v75 nil)))
    (dolist (output outputs)
      (websocket-outer-filter websocket output))
    (nreverse packet-data)))

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

(ert-run-tests-interactively 'websocket-filter-basic)
(ert-run-tests-interactively 'websocket-filter-inflight-packets)
(ert-run-tests-interactively 'websocket-filter-first-response)
