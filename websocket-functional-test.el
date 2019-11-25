;;; websocket-functional-test.el --- Simple functional testing

;; Copyright (c) 2013, 2016  Free Software Foundation, Inc.

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
;; These tests are written to test the basic connectivity and message-sending.
;; Corner-cases and error handling is tested in websocket-test.el.

(require 'tls)   ;; tests a particular bug we had on Emacs 23
(require 'websocket)
(require 'cl)

;;;;;;;;;;;;;;;;;;;;;;;
;; Local server test ;;
;;;;;;;;;;;;;;;;;;;;;;;

(message "Testing with local server")

(setq websocket-debug t)

(defvar wstest-server-buffer (get-buffer-create "*wstest-server*"))
(defvar wstest-server-name "wstest-server")
(defvar wstest-server-proc
  (start-process wstest-server-name wstest-server-buffer
                 "python" "testserver.py" "--log_to_stderr" "--logging=debug"))
(sleep-for 1)

(defvar wstest-msgs nil)
(defvar wstest-closed nil)

(message "Opening the websocket")

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

(ert-deftest websocket-client-with-remote-server ()
  ;; Emacs previous to Emacs 24 cannot handle wss.
  (when (>= (string-to-number (substring emacs-version 0 2)) 24)
    ;; echo.websocket.org has an untrusted certificate, for the test to
    ;; proceed, we need to disable trust checking.
    (let* ((tls-checktrust nil)
             (wstest-closed nil)
             (wstest-msg)
             (wstest-ws
              (websocket-open
               "wss://echo.websocket.org"
               :on-message (lambda (_websocket frame)
                             (setq wstest-msg (websocket-frame-text frame)))
               :on-close (lambda (_websocket) (setq wstest-closed t)))))
        (should (websocket-test-wait-with-timeout 2 (websocket-openp wstest-ws)))
        (should (websocket-test-wait-with-timeout 2 (eq 'open (websocket-ready-state wstest-ws))))
        (should (null wstest-msg))
        (websocket-send-text wstest-ws "Hi!")
        (should (websocket-test-wait-with-timeout 5 (equal wstest-msg "Hi!")))
        (websocket-close wstest-ws))))

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
