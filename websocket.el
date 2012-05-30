;;; websocket.el --- Emacs WebSocket client

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
;; This implements RFC 6455, which can be found at
;; http://tools.ietf.org/html/rfc6455.

(require 'bindat)
(require 'url-parse)
(require 'calc)
(eval-when-compile (require 'cl))

;;; Code:
(defstruct websocket
  (conn (assert nil) :read-only t)
  (filter (assert nil) :read-only t)
  (close-callback (assert nil) :read-only t)
  (open-callback nil :read-only t)
  (url (assert nil) :read-only t)
  (accept-string (assert nil))
  (handshake-accept-passed-p nil)
  (header-read-p nil)
  (inflight-packet nil))

(defvar websocket-debug nil
  "Set to true to output debugging info to a per-websocket buffer.
The buffer is ` *websocket URL debug*' where URL is the
URL of the connection.")

(defvar websocket-ignore-error nil
  "Set to true to suppress error messages.")

(defconst websocket-guid "258EAFA5-E914-47DA-95CA-C5AB0DC85B11"
  "The websocket GUID as defined in RFC 6455.
Do not change unless the RFC changes.")

(defvar websocket-require-server-accept t
  "If true, we check the Sec-WebSocket-Accept header.
This is required by the RFC as part of the connection
handshake.")

(defvar websocket-mask-frames t
  "If true, we mask frames as defined in the spec.
This is recommended to be true, and some servers will refuse to
communicate with unmasked clients.")

(defun websocket-genbytes (nbytes)
  "Generate NBYTES random bytes."
  (let ((s (make-string nbytes ?\s)))
    (dotimes (i nbytes)
      (aset s i (random 256)))
    s))

(defun websocket-genkey ()
  "Generate a key suitable for the websocket handshake."
  (base64-encode-string (websocket-genbytes 16)))

(defun websocket-calculate-accept (key)
  "Calculate the expect value of the accept header.
This is based on the KEY from the Sec-WebSocket-Key header."
  (base64-encode-string
   (sha1 (concat key websocket-guid) nil nil t)))

(defun websocket-get-bytes (s n)
  "From string S, retrieve the value of N bytes.
Return the value as an unsigned integer.  The value N must be a
power of 2, up to 8."
  (if (= n 8)
    (let* ((32-bit-parts
            (bindat-get-field (bindat-unpack '((:val vec 2 u32)) s) :val))
           (cval (calc-eval '("(2^32 * $ + $$)") nil
                            (aref 32-bit-parts 0) (aref 32-bit-parts 1))))
      (when (calc-eval '("$ > $$") 'pred cval most-positive-fixnum)
        (error "websocket-get-bytes: value too large to parse!"))
      (string-to-number cval))
    ;; n is not 8
    (bindat-get-field (bindat-unpack
                     `((:val
                        ,(cond ((= n 1) 'u8)
                               ((= n 2) 'u16)
                               ((= n 4) 'u32)
                               (t (error
                                   "websocket-get-bytes: Unknown N: %s" n)))))
                     s) :val)))

(defun websocket-to-bytes (val nbytes)
  "Encode the integer VAL in NBYTES of data.
NBYTES much be a power of 2, up to 8."
  (unless (or (and (< nbytes 8)
                   (< val (expt 2 (* 8 nbytes))))
              (and (= nbytes 8)
                   (calc-eval "% < 2^(8 * %%)" 'pred val nbytes)))
      (error "websocket-to-bytes: Value %d could not be expressed in %d bytes"
             val nbytes))
  (if (= nbytes 8)
      (bindat-pack `((:val vec 2 u32))
                   `((:val . [,(/ val 4294967296)
                              ,(mod val 4294967296)])))
    (bindat-pack
     `((:val ,(cond ((= nbytes 1) 'u8)
                    ((= nbytes 2) 'u16)
                    ((= nbytes 4) 'u32)
                    (t (error "websocket-to-bytes: Unknown NBYTES: %s" nbytes)))))
     `((:val . ,val)))))

(defun websocket-get-opcode (s)
  "Retrieve the opcode from first byte of string S."
  (websocket-ensure-length s 1)
  (let ((opcode (logand #xf (websocket-get-bytes s 1))))
    (cond ((= opcode 0) 'continuation)
          ((= opcode 1) 'text)
          ((= opcode 2) 'binary)
          ((= opcode 8) 'close)
          ((= opcode 9) 'ping)
          ((= opcode 10) 'pong))))

(defun websocket-get-payload-len (s)
  "Parse out the payload length from the string S.
We start at position 0, and return a cons of the payload length and how
many bytes were consumed from the string."
  (websocket-ensure-length s 1)
  (let* ((initial-val (logand 127 (websocket-get-bytes s 1))))
    (cond ((= initial-val 127)
           (websocket-ensure-length s 9)
           (cons (websocket-get-bytes (substring s 1) 8) 9))
          ((= initial-val 126)
           (websocket-ensure-length s 3)
           (cons (websocket-get-bytes (substring s 1) 2) 3))
          (t (cons initial-val 1)))))

(defstruct websocket-frame opcode payload length completep)

(defun websocket-mask (key data)
  "Using string KEY, mask string DATA according to the RFC.
This is used to both mask and unmask data."
  (apply
   'string
   (loop for b across data
         for i from 0 to (length data)
         collect (logxor (websocket-get-bytes (substring key (mod i 4)) 1) b))))

(defun websocket-ensure-length (s n)
  "Ensure the string S has at most N bytes.
Otherwise we throw the error `websocket-incomplete-frame'."
  (when (< (length s) n)
    (throw 'websocket-incomplete-frame nil)))

(defun websocket-encode-frame (frame)
  "Encode the FRAME struct to the binary representation."
  (let* ((opcode (websocket-frame-opcode frame))
         (payload (websocket-frame-payload frame))
         (fin (websocket-frame-completep frame))
         (payloadp (memq opcode '(continuation text binary)))
         (mask-key (when websocket-mask-frames  (websocket-genbytes 4))))
    (apply 'unibyte-string
           (append (list
                    (logior (cond ((eq opcode 'continuation) 0)
                                  ((eq opcode 'text) 1)
                                  ((eq opcode 'binary) 2)
                                  ((eq opcode 'close) 8)
                                  ((eq opcode 'ping) 9)
                                  ((eq opcode 'pong) 10))
                            (if fin 128 0)))
                   (when payloadp
                     (list
                      (logior
                       (if websocket-mask-frames 128 0)
                       (cond ((< (length payload) 126) (length payload))
                             ((< (length payload) 65536) 126)
                             (t 127)))))
                   (when (and payloadp (>= (length payload) 126))
                     (append (websocket-to-bytes (length payload)
                                          (cond ((< (length payload) 126) 1)
                                                ((< (length payload) 65536) 2)
                                                (t 8))) nil))
                   (when (and payloadp websocket-mask-frames)
                     (append mask-key nil))
                   (when payloadp
                     (append (if websocket-mask-frames
                                 (websocket-mask mask-key payload)
                               payload)
                             nil))))))

(defun websocket-read-frame (s)
  "Read from string S a `websocket-frame' struct with the contents.
This only gets complete frames.  Partial frames need to wait until
the frame finishes.  If the frame is not completed, return NIL."
  (catch 'websocket-incomplete-frame
    (websocket-ensure-length s 1)
    (let* ((opcode (websocket-get-opcode s))
           (fin (logand 128 (websocket-get-bytes s 1)))
           (payloadp (memq opcode '(continuation text binary)))
           (payload-len (when payloadp
                          (websocket-get-payload-len (substring s 1))))
           (maskp (and
                   payloadp
                   (= 128 (logand 128 (websocket-get-bytes (substring s 1) 1)))))
           (payload-start (when payloadp (+ (if maskp 5 1) (cdr payload-len))))
           (payload-end (when payloadp (+ payload-start (car payload-len))))
           (unmasked-payload (when payloadp
                               (websocket-ensure-length s payload-end)
                               (substring s payload-start payload-end))))
      (make-websocket-frame
       :opcode opcode
       :payload
       (if maskp
           (let ((masking-key (substring s (+ 1 (cdr payload-len))
                                         (+ 5 (cdr payload-len)))))
             (websocket-mask masking-key unmasked-payload))
         unmasked-payload)
       :length payload-end
       :completep (> fin 0)))))

(defun websocket-open (url filter &optional close-callback)
  "Open a websocket connection to URL.
`websocket-frame' structs are sent as the only argument to
FILTER, and if the connection is closed, then CLOSE-CALLBACK is
called.  Errors from FILTER function will be ignored.  You should
catch it in the FILTER function in order to recover from the
error.  Errors will be messaged using `messsage' function.  To
suppress messaging errors, set `websocket-ignore-error' to t.
You can log errors by setting variable `websocket-debug' to t."
  (let* ((name (format "websocket to %s" url))
         (url-struct (url-generic-parse-url url))
         (key (websocket-genkey))
         (buf-name (format " *%s*" name))
         (coding-system-for-read 'binary)
         (coding-system-for-write 'binary)
         (conn (if (equal (url-type url-struct) "ws")
                   (make-network-process :name name
                                         :buffer buf-name
                                         :host (url-host url-struct)
                                         :service (url-port url-struct)
                                         :nowait nil)
                 (if (equal (url-type url-struct) "wss")
                     (error "Not implemented yet")
                   (error "Unknown protocol"))))
         (websocket (make-websocket :conn conn :url url :filter filter
                                    :close-callback close-callback
                                    :accept-string
                                    (websocket-calculate-accept key))))
    (lexical-let ((websocket websocket))
      (set-process-filter conn
                          (lambda (process output)
                            (websocket-outer-filter websocket output)))
      (when close-callback
        (set-process-sentinel conn
                              (lambda (process change)
                                (websocket-debug websocket
                                                 "State change to %s" change)
                                (unless (websocket-openp websocket)
                                  (funcall (websocket-close-callback
                                            websocket)))))))
    (set-process-query-on-exit-flag conn nil)
    (process-send-string conn
                         (format "GET %s HTTP/1.1\r\n"
                                 (let ((path (url-filename url-struct)))
                                   (if (> (length path) 0) path "/"))))
    (websocket-debug websocket "Sending handshake, key: %s, acceptance: %s"
                     key (websocket-accept-string websocket))
    (process-send-string
     conn
     (format (concat "Host: %s\r\n"
                     "Upgrade: websocket\r\n"
                     "Connection: Upgrade\r\n"
                     "Sec-WebSocket-Key: %s\r\n"
                     "Origin: %s\r\n"
                     "Sec-WebSocket-Version: 13\r\n"
                     "\r\n")
             (url-host (url-generic-parse-url url))
             key
             system-name))
    (websocket-debug websocket "Websocket opened")
    websocket))

(defun websocket-get-debug-buffer-create (websocket)
  "Get or create the buffer corresponding to WEBSOCKET."
  (get-buffer-create (format " *websocket %s debug*"
                             (websocket-url websocket))))

(defun websocket-error (msg &rest args)
  "Report error message MSG."
  (unless websocket-ignore-error
    (apply 'message msg args))
  (apply 'websocket-debug msg args))

(defun websocket-debug (websocket msg &rest args)
  "In the WEBSOCKET's debug buffer, send MSG, with format ARGS."
  (when websocket-debug
    (let ((buf (websocket-get-debug-buffer-create websocket)))
      (save-excursion
        (with-current-buffer buf
          (goto-char (point-max))
          (insert "[WS] ")
          (insert (apply 'format (append (list msg) args)))
          (insert "\n"))))))

(defun websocket-verify-handshake (websocket output)
  "Based on WEBSOCKET's key, verify that OUTPUT contains a valid handshake.
The output is assumed to have complete headers.  This function
will either return t or call `error'."
  (let ((accept-string
         (concat "Sec-WebSocket-Accept: " (websocket-accept-string websocket))))
    (websocket-debug websocket "Handshake received, checking for: %s" accept-string)
    (if (string-match (regexp-quote accept-string) output)
        (progn
          (setf (websocket-handshake-accept-passed-p websocket) t)
          (websocket-debug websocket "Handshake accepted")
          ;; return true
          t)
      (error "Incorrect handshake from websocket: is this really a websocket connection?"))))

(defun websocket-process-frame (websocket frame)
  "Using the WEBSOCKET's filter and connection, process the FRAME.
If the frame has a payload, the frame is passed to the filter
slot of WEBSOCKET.  If the frame is a ping, we reply with a pong.
If the frame is a close, we terminate the connection."
  (let ((opcode (websocket-frame-opcode frame)))
    (cond ((memq opcode '(continuation text binary))
           (condition-case err
               (funcall (websocket-filter websocket) frame)
             (error (websocket-error "Got error from the filter function: %s"
                                     (error-message-string err)))))
          ((eq opcode 'ping)
           (websocket-send websocket
                           (make-websocket-frame :opcode 'pong :completep t)))
          ((eq opcode 'close)
           (delete-process (websocket-conn websocket))))))

(defun websocket-outer-filter (websocket output)
  "Filter the WEBSOCKET server's OUTPUT.
This will parse headers and process frames repeatedly until there
is no more output or the connection closes."
  (websocket-debug websocket "Received: %s" output)
  (let ((start-point)
        (end-point 0)
        (text (concat (websocket-inflight-packet websocket) output))
        (header-end-pos))
    ;; If we've received the complete header, check to see if we've
    ;; received the desired handshake.
    (when (and (not (websocket-header-read-p websocket))
               (setq header-end-pos (string-match "\r\n\r\n" text))
               (setq start-point (+ 4 header-end-pos)))
      (setf (websocket-header-read-p websocket) t)
      (when (and websocket-require-server-accept
                (not (websocket-handshake-accept-passed-p websocket))
                start-point)
       (websocket-verify-handshake websocket text))
      (when (websocket-open-callback websocket)
        (funcall (websocket-open-callback websocket))))
    (when (websocket-header-read-p websocket)
      (unless start-point (setq start-point 0))
      (let ((current-frame))
        (while (and (setq current-frame (websocket-read-frame
                                         (substring text start-point))))
          (websocket-process-frame websocket current-frame)
          (incf start-point (websocket-frame-length current-frame)))))
    ;; TODO(ahyatt) Rename websocket-inflight-packet (it isn't a packet)
    (setf (websocket-inflight-packet websocket) (substring text (or start-point 0)))))

(defun websocket-send-text (websocket text)
  "To the WEBSOCKET, send TEXT as a complete frame."
  (websocket-send websocket (make-websocket-frame :opcode 'text :payload text
                                                  :completep t)))

(defun websocket-check (frame)
  "Check FRAME for correctness, returning true if correct."
  (and (equal (not (memq (websocket-frame-opcode frame)
                         '(continuation text binary)))
              (and (not (websocket-frame-payload frame))
                   (websocket-frame-completep frame)))))

(defun websocket-send (websocket frame)
  "To the WEBSOCKET server, send the FRAME.
This will raise an error if the frame is illegal."
  (unless (websocket-check frame)
    (error "Cannot send illegal frame to websocket"))
  (websocket-debug websocket "Sending frame, opcode: %s payload: %s"
                   (websocket-frame-opcode frame)
                   (websocket-frame-payload frame))
  (websocket-ensure-connected websocket)
  (unless (websocket-openp websocket)
    (error "No webserver process to send data to!"))
  (process-send-string (websocket-conn websocket)
                       (websocket-encode-frame frame)))

(defun websocket-openp (websocket)
  "Check WEBSOCKET and return non-nil if it is open and connected."
  (and websocket (eq 'open (process-status (websocket-conn websocket)))))

(defun websocket-close (websocket)
  "Close WEBSOCKET and erase all the old websocket data."
  (websocket-debug websocket "Closing websocket")
  (when (websocket-openp websocket)
    (websocket-send websocket
                    (make-websocket-frame :opcode 'close
                                          :completep t)))
  (kill-buffer (process-buffer (websocket-conn websocket))))

(defun websocket-ensure-connected (websocket)
  "If the WEBSOCKET connection is closed, open it."
  (unless (and (websocket-conn websocket)
               (ecase (process-status (websocket-conn websocket))
                 ((run open listen) t)
                 ((stop exit signal closed connect failed nil) nil)))
    (websocket-close websocket)
    (websocket-open (websocket-url websocket)
                      (websocket-filter websocket)
                      (websocket-close-callback websocket))))

(provide 'websocket)

;;; websocket.el ends here
