;; Simple functional testing
;; Usage: emacs -batch -Q -L . -l websocket-functional-test.el

(require 'websocket)
(eval-when-compile (require 'cl))

(setq websocket-debug t)

(defvar wstest-server-buffer (get-buffer-create "*wstest-server*"))
(defvar wstest-server-name "wstest-server")
(defvar wstest-server-proc
  (start-process wstest-server-name wstest-server-buffer
                 "python" "testserver.py" "--log_to_stderr" "--logging=debug"))
(sleep-for 1)

(defvar wstest-msgs nil)
(defvar wstest-closed nil)

(defvar wstest-ws
  (websocket-open
   "ws://127.0.0.1:9999"
   (lambda (p) (push p wstest-msgs) (message "ws packet: %S" p))
   (lambda () (setq wstest-closed t))))

(sleep-for 0.1)
(assert (websocket-openp wstest-ws))

(assert (null wstest-msgs))
(websocket-send wstest-ws "Hi!")
(sleep-for 0.1)
(assert (equal (car wstest-msgs) "You said: Hi!"))

(websocket-close wstest-ws)
(assert (null (websocket-openp wstest-ws)))

(stop-process wstest-server-proc)
