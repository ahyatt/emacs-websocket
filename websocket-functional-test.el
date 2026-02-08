;;; websocket-functional-test.el --- Simple functional testing -*- lexical-binding:t -*-

;; Copyright (c) 2013, 2016, 2026  Free Software Foundation, Inc.

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
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; These are functional tests that may fail for various environmental reasons,
;; such as blocked ports. For example Windows users have to have gnutls DLLs in
;; the Emacs bin directory for this to work. A firewall may also interfere with
;; these tests.
;;
;; This test requires `uv' to be installed.
;;
;; These tests are written to test the basic connectivity and message-sending.
;; Corner-cases and error handling is tested in websocket-test.el.

(require 'nsm)
(require 'websocket)
(require 'ert)

;;; Code:

(defmacro websocket-test-wait-with-timeout (timeout &rest body)
  "Run BODY until true or TIMEOUT (in seconds) is reached.

Will return false if the timeout was reached. This macro is not
written to be used widely."
  `(let ((begin (current-time))
         (result nil))
     (while (and (< (- (float-time (time-subtract (current-time) begin))) ,timeout) (not result))
       (setq result ,@body)
       (sleep-for 0.5))
     result))

(defun websocket-functional-client-test (wstest-server-url)
  "Run the main part of an ert test against WSTEST-SERVER-URL."
  ;; the server may have an untrusted certificate, for the test to proceed, we
  ;; need to disable trust checking.
  (let* ((nsm-trust-local-network t)
         ;; (wstest-closed nil)
         (wstest-msg)
         ;; (wstest-server-proc)
         (wstest-ws
          (websocket-open
           wstest-server-url
           :on-message (lambda (_websocket frame)
                         (setq wstest-msg (websocket-frame-text frame)))
           :on-close (lambda (_websocket)
                       ;; (setq wstest-closed t)
                       t))))
    (should (websocket-test-wait-with-timeout 2 (websocket-openp wstest-ws)))
    (should (websocket-test-wait-with-timeout 2 (eq 'open (websocket-ready-state wstest-ws))))
    (should (null wstest-msg))
    (websocket-send-text wstest-ws "Hi!")
    (should (websocket-test-wait-with-timeout 5 (equal wstest-msg "Hi!")))
    (websocket-close wstest-ws)))

;; Hack because we have to be able to find the testserver.py script.
(defconst websocket-ft-testserver (format "%s/testserver.py"
                                          (file-name-directory
                                           (if (fboundp 'macroexp-file-name)
                                               (macroexp-file-name) ;Emacs-28
                                             load-file-name))))

(ert-deftest websocket-client-with-local-server ()
  ;; If testserver.py cannot start, this test will fail.
  (let ((proc (start-process
               "websocket-testserver" "*websocket-testserver*"
               websocket-ft-testserver)))
    (when proc
      (sleep-for 1)
      (websocket-functional-client-test "ws://127.0.0.1:9999"))))

(ert-deftest websocket-server ()
  (let* ((wstest-closed)
         (wstest-msg)
         (server-conn (websocket-server
                       9998
                       :host 'local
                       :on-message (lambda (ws frame)
                                     (websocket-send-text
                                      ws (websocket-frame-text frame)))
                       :on-close (lambda (_websocket)
                                   (setq wstest-closed t))))
         (wstest-ws (websocket-open
                     "ws://localhost:9998"
                     :on-message (lambda (_websocket frame)
                                   (setq wstest-msg (websocket-frame-text frame))))))
    (should (websocket-test-wait-with-timeout 1 (websocket-openp wstest-ws)))
    (websocket-send-text wstest-ws "你好")
    (should (websocket-test-wait-with-timeout 1 (equal wstest-msg "你好")))
    (websocket-server-close server-conn)
    (should (websocket-test-wait-with-timeout 1 wstest-closed))))

(provide 'websocket-functional-test)
;;; websocket-functional-test.el ends here
