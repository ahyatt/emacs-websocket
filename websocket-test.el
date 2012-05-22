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
(defconst websocket-test-masked-hello
  "\x81\x85\x37\xfa\x21\x3d\x7f\x9f\x4d\x51\x58"
  "'Hello' masked string example, taken from the RFC.")

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
  (should (equal (make-websocket-frame :opcode 'text :payload "Hello"
                                       :length (length websocket-test-hello)
                                       :completep t)
                 (websocket-read-frame websocket-test-hello)))
  (should (equal (make-websocket-frame :opcode 'text :payload "Hello"
                                       :length (length websocket-test-hello)
                                       :completep t)
                 (websocket-read-frame (concat websocket-test-hello
                                               "should-not-be-read"))))
  (should (equal (make-websocket-frame :opcode 'text :payload "Hello"
                                       :length (length websocket-test-masked-hello)
                                       :completep t)
                 (websocket-read-frame websocket-test-masked-hello)))
  (should (equal (make-websocket-frame :opcode 'text :payload "Hello"
                                       :length (length websocket-test-hello)
                                       :completep nil)
                 (websocket-read-frame (concat (unibyte-string
                                                (logand (string-to-char
                                                         (substring websocket-test-hello 0 1))
                                                        127))
                                               (substring websocket-test-hello 1)))))
  (dotimes (i (- (length websocket-test-hello) 1))
    (should-not (websocket-read-frame
                 (substring websocket-test-hello 0
                            (- (length websocket-test-hello) (+ i 1))))))
  (dotimes (i (- (length websocket-test-masked-hello) 1))
    (should-not (websocket-read-frame
                 (substring websocket-test-masked-hello 0
                            (- (length websocket-test-masked-hello) (+ i 1)))))))

(defun websocket-test-make-websocket-with-accept-string (s)
  (make-websocket :conn "fake-conn" :url "ws://foo/bar" :filter t :close-callback t 
                  :accept-string s))

(ert-deftest websocket-verify-handshake ()
  ;; This examples comes from the RFC
  (should (websocket-verify-handshake
           (websocket-test-make-websocket-with-accept-string "s3pPLMBiTxaQ9kYGzzhZRbK+xOo=")
           "Sec-WebSocket-Accept: s3pPLMBiTxaQ9kYGzzhZRbK+xOo=\r\n"))
  (should-error (websocket-verify-handshake
                 (websocket-test-make-websocket-with-accept-string "s3pPLMBiTxaQ9kYGzzhZRbK+xOo=")
                 "Sec-WebSocket-Accept: foo\r\n")))

(ert-deftest websocket-process-frame ()
  (let* ((sent)
         (processed)
         (deleted)
         (websocket (make-websocket :conn "fake-conn"
                                    :url "ws://foo/bar"
                                    :filter (lambda (frame) (setq processed
                                                             (websocket-frame-payload frame)))
                                    :close-callback t
                                    :accept-string "accept-string")))
    (dolist (opcode '(text binary continuation))
      (setq processed nil)
      (should (equal
               "hello"
               (progn
                 (websocket-process-frame websocket
                                          (make-websocket-frame :opcode opcode :payload "hello"))
                 processed))))
    (setq sent nil)
    (flet ((websocket-send (websocket content) (setq sent content)))
      (should (equal
               "\xA"
               (progn
                 (websocket-process-frame websocket
                                          (make-websocket-frame :opcode 'ping))
                 sent))))
    (flet ((delete-process (conn) (setq deleted t)))
      (should (progn
                (websocket-process-frame websocket
                                         (make-websocket-frame :opcode 'close))
                deleted)))))

(ert-deftest websocket-to-bytes ()
  ;; We've tested websocket-get-bytes by itself, now we can use it to
  ;; help test websocket-to-bytes.
  (should (equal 30 (websocket-get-bytes (websocket-to-bytes 30 1) 1)))
  (should (equal 300 (websocket-get-bytes (websocket-to-bytes 300 2) 2)))
  (should (equal 70000 (websocket-get-bytes (websocket-to-bytes 70000 8) 8)))
  (should-error (websocket-to-bytes 30 3))
  (should-error (websocket-to-bytes 300 1)))

(ert-deftest websocket-encode-frame ()
  ;; We've tested websocket-read-frame, now we can use that to help
  ;; test websocket-encode-frame.
  (should (equal
           websocket-test-hello
           (websocket-encode-frame
            (make-websocket-frame :opcode 'text :payload "Hello" :completep t))))
  (should-not
   (websocket-frame-completep
    (websocket-read-frame
     (websocket-encode-frame (make-websocket-frame :opcode 'text :payload "Hello" :completep nil)))))
  (dolist ((opcode '(close ping pong)))
    (should (equal
             opcode
             (websocket-frame-opcode
              (websocket-read-frame
               (websocket-encode-frame (make-websocket-frame :opcode opcode))))))))

