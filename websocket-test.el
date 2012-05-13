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

(defun websocket-test-get-filtered-response (outputs)
  (let* ((packet-data nil)
         (websocket
          (make-websocket :conn "fake-conn"
                          :filter (lambda (packet) (push packet packet-data))
                          :close-callback (lambda (not-called) (assert nil))
                          :url "ws://foo/bar")))
    (dolist (output outputs)
      (websocket-outer-filter websocket output))
    (nreverse packet-data)))

(ert-deftest websocket-genbytes-length ()
  (loop repeat 100
        do (should (= (string-bytes (websocket-genbytes)) 16))))

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

(ert-deftest websocket-calculate-accept ()
  ;; This example comes straight from RFC 6455
  (should
   (equal "s3pPLMBiTxaQ9kYGzzhZRbK+xOo="
    (websocket-calculate-accept "dGhlIHNhbXBsZSBub25jZQ=="))))

(ert-deftest websocket-get-bits ()
  ;; 0         1         2         3
  ;; 01234567890123456789012345678901
  ;; For testing, let's use this fairly random sequence
  ;; 01010001000100101011011110010101
  ;; = 
  (let ((test-num 1360181141))
    (should (equal 1 (websocket-get-bits test-num 0 1)))
    (should (equal 1 (websocket-get-bits test-num 30 31)))
    ;; 16-20 = 10110
    (should (equal 22 (websocket-get-bits test-num 16 20)))))

(ert-run-tests-interactively 'websocket-genbytes-length)
(ert-run-tests-interactively 'websocket-filter-basic)
(ert-run-tests-interactively 'websocket-filter-inflight-packets)
(ert-run-tests-interactively 'websocket-filter-first-response)
(ert-run-tests-interactively 'websocket-calculate-accept)
