;; Simple functional testing
;; Usage: emacs -batch -Q -L . -l websocket-functional-test.el

(require 'websocket)
(eval-when-compile (require 'cl))

(setq websocket-debug t)
(toggle-debug-on-error)

(defvar wstest-server-buffer (get-buffer-create "*wstest-server*"))
(defvar wstest-server-name "wstest-server")
(defvar wstest-server-proc
  (start-process wstest-server-name wstest-server-buffer
                 "python" "testserver.py" "--log_to_stderr" "--logging=debug"))
(sleep-for 1)

(defvar wstest-msgs nil)
(defvar wstest-closed nil)

(setq websocket-require-server-accept t)

(defvar wstest-ws
  (websocket-open
   "ws://127.0.0.1:9999"
   (lambda (frame) (push (websocket-frame-payload frame) wstest-msgs)
     (message "ws frame: %S" (websocket-frame-payload frame)))
   (lambda () (setq wstest-closed t))))

(defun wstest-pop-to-debug ()
  "Open websocket log buffer. Not used in testing. Just for debugging."
  (interactive)
  (pop-to-buffer (websocket-get-debug-buffer-create wstest-ws)))

(sleep-for 0.1)
(assert (websocket-openp wstest-ws))

(assert (null wstest-msgs))
(websocket-send-text wstest-ws "Hi!")
(sleep-for 0.1)
(assert (equal (car wstest-msgs) "You said: Hi!"))

(websocket-close wstest-ws)
(assert (null (websocket-openp wstest-ws)))

(stop-process wstest-server-proc)
