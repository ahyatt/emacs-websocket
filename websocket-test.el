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

(ert-deftest websocket-calculate-accept ()
  ;; This example comes straight from RFC 6455
  (should
   (equal "s3pPLMBiTxaQ9kYGzzhZRbK+xOo="
    (websocket-calculate-accept "dGhlIHNhbXBsZSBub25jZQ=="))))

(defconst websocket-test-hello "\x81\x05\x48\x65\x6c\x6c\x6f"
  "'Hello' string example, taken from the RFC.")

(ert-deftest websocket-get-bytes ()
  (should (equal #x5 (websocket-get-bytes "\x5" 1)))
  (should (equal #x101 (websocket-get-bytes "\x1\x1" 2)))
  (should (equal #x100000001
                 (websocket-get-bytes "\x0\x0\x0\x1\x0\x0\x0\x1" 8)))
  (should-error (websocket-get-bytes "\x0\x0\x0" 3))
  (should-error (websocket-get-bytes "\x0" 2)))

(ert-deftest websocket-get-opcode ()
  (should (equal 'text (websocket-get-opcode websocket-test-hello))))

(ert-deftest websocket-get-payload-len ()
  (should (equal '(5 . 1)
                 (websocket-get-payload-len
                  (substring websocket-test-hello 1))))
  (should (equal '(200 . 3)
                 (websocket-get-payload-len
                  (bindat-pack '((:len u8) (:val u16))
                               `((:len . 126)
                                 (:val . 200))))))
  ;; we don't want to hit up any limits even on strange emacs builds,
  ;; so this test has a pretty small test value
  (should (equal '(70000 . 9)
                 (websocket-get-payload-len
                  (bindat-pack '((:len u8) (:val vec 2 u32))
                               `((:len . 127)
                                 (:val . [0 70000])))))))

(ert-deftest websocket-read-frame ()
  (should (equal (make-websocket-frame :opcode 'text :payload "Hello")
                 (websocket-read-frame websocket-test-hello))))
