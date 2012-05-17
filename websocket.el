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
  (url (assert nil) :read-only t)
  (accept-string (assert nil))
  (handshake-accept-passed-p nil)
  (inflight-packet nil))

(defvar websocket-debug nil
  "Set to true to output debugging info to a per-websocket buffer.
The buffer is ` *websocket URL debug*' where URL is the
URL of the connection.")

(defconst websocket-guid "258EAFA5-E914-47DA-95CA-C5AB0DC85B11"
  "The websocket GUID as defined in RFC 6455. Do not change
  unless the RFC changes.")

(defvar websocket-require-server-accept t
  "If true, we require the correct Sec-WebSocket-Accept header
as part of the connection handshake.")

(defun websocket-genbytes ()
  "Generate bytes used at the end of the handshake."
  (let ((s "                "))
    (dotimes (i 16)
      (aset s i (random 256)))
    s))

(defun websocket-random-insert (str-to-insert target-str)
  "Insert STR-TO-INSERT at a random position in TARGET-STR."
  (let ((r (+ 1 (random (- (length target-str) 2)))))
    (concat (substring target-str 0 r) str-to-insert
            (substring target-str r))))

(defun websocket-genkey ()
  "Generate a key suitable for the websocket handshake."
  (base64-encode-string (websocket-genbytes)))

(defun websocket-calculate-accept (key)
  "Calculate the expect value of the accept header.
This is based on the KEY from the Sec-WebSocket-Key header."
  (base64-encode-string
   (sha1 (concat key websocket-guid) nil nil t)))

(defun websocket-get-bits (dword start-bit end-bit)
  "Return the value of DWORD between START-BIT and END-BIT.
START-BIT must be less than END-BIT.  The range is inclusive at
both ends.  Although the ordering of bits is big-endian the bits
are numbed most significant first.  That is, the most
significant, leftmost bit is 0."
  (when (> start-bit end-bit)
      (error
       "In websocket-get-bits: Start bit must be less than end-bit."))
  (logand (lsh dword (- (- 31 end-bit)))
          (loop for i from 0 upto
                (- end-bit start-bit)
                sum (expt 2 i))))

(defun websocket-get-dword (s)
  "From string S, retrieve the first dword."
  (bindat-get-field (bindat-unpack '((:val dword)) s) :val))

(defun websocket-get-opcode (s)
  "Retrieve the opcode from the dword at the start of the frame
given by string."
  (let ((opcode (websocket-get-bits (websocket-get-dword s) 4 7)))
    (cond ((= opcode 0) 'continuation)
          ((= opcode 1) 'text)
          ((= opcode 2) 'binary)
          ((= opcode 8) 'close)
          ((= opcode 9) 'ping)
          ((= opcode 10) 'pong))))

(defun websocket-get-payload-len (s)
  "Parses out the payload length from the string.
We start at position 0, and return a cons of the payload length and how
many bytes were consumed from the string."
  (let* ((dword (websocket-get-dword s))
         (initial-val (websocket-get-bits dword 9 15)))
    (cond ((< initial-val 126)
           (cons initial-val 0))
          ((= initial-val 126)
           (cons
            (bindat-get-field (bindat-unpack '((:val u16)) (substring s 4)) :val)
            3))
          (t (let* ((32-bit-parts
                     (bindat-get-field (bindat-unpack '((:val vec 2 u32))
                                                      (substring s 4)) :val))
                   (cval (calc-eval "(2^32 * $ + $$)" nil
                                    (aref 32-bit-parts 0) (aref 32-bit-parts 1))))
              (when (calc-eval "$ > $$" 'pred cval most-positive-fixnum)
                (error "Websocket sent a frame too large for emacs!"))
              (cons (string-to-int cval) 9))))))

(defun websocket-open (url filter &optional close-callback)
  "Open a websocket connection to URL.
Websocket packets are sent as the only argument to FILTER, and if
the connection is closed, then CLOSE-CALLBACK is called."
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
    (websocket-debug websocket "Sending handshake")
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
             system-name
             key))
    (websocket-debug websocket "Websocket opened")
    websocket))

(defun websocket-get-debug-buffer-create (websocket)
  (get-buffer-create (format " *websocket %s debug*"
                             (websocket-url websocket))))

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

(defun websocket-outer-filter (websocket output)
  "Removes connection strings, only passes packets."
  (websocket-debug websocket "Received: %s" output)
  (let ((start-point 0)
        (end-point 0)
        (text (concat (websocket-inflight-packet websocket) output)))
    (setq start-point (string-match "\0" text))
    ;; If we've received the first packet, check to see if we've
    ;; received the desired handshake.
    (when (and websocket-require-server-accept
               (not (websocket-handshake-accept-passed-p websocket))
               start-point)
      (let ((accept-string
             (concat "Sec-WebSocket-Accept: " (websocket-accept-string websocket))))
        (websocket-debug websocket "Handshake received, checking for: %s" accept-string)
        (if (string-match (regexp-quote accept-string) text)
            (progn
              (setf (websocket-handshake-accept-passed-p websocket) t)
              (websocket-debug websocket "Handshake accepted"))
        (error "Incorrect handshake from websocket: is this really a websocket connection?"))))
    (while (and start-point
                (setq end-point
                      (string-match "\377" text start-point)))
        (funcall (websocket-filter websocket)
                 (substring text (+ 1 start-point) end-point))
        (setq start-point (string-match "\0" text end-point)))
      (let* ((next-start (or start-point
                                     (when end-point
                                       (or (string-match "\0" text end-point)
                                           (- (length text) 1)))
                                     0))
             (next-end (or (string-match "\377" text next-start)
                            (length text))))
        (setf (websocket-inflight-packet websocket)
              (concat (substring text next-start next-end))))))

(defun websocket-send (websocket text)
  "Send the raw TEXT as a websocket packet."
  (websocket-debug websocket "Sending text: %s" text)
  (websocket-ensure-connected websocket)
  (unless (websocket-openp websocket)
    (error "No webserver process to send data to!"))
  (process-send-string (websocket-conn websocket)
                       (concat (unibyte-string ?\0) text
                               (unibyte-string ?\377))))

(defun websocket-openp (websocket)
  "Returns true if the websocket exists and is open."
  (and websocket (eq 'open (process-status (websocket-conn websocket)))))

(defun websocket-close (websocket)
  "Close the websocket and erase all the old websocket data."
  (websocket-debug websocket "Closing websocket")
  (when (websocket-openp websocket)
    (process-send-string (websocket-conn websocket) (unibyte-string ?\377?\0)))
  (kill-buffer (process-buffer (websocket-conn websocket))))

(defun websocket-ensure-connected (websocket)
  "If the websocket connection is closed, open it."
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
