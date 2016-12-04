#!/usr/bin/env clisp

(defparameter *port*      (parse-integer (or (getenv "PORT") "10923") :radix 10))
(defparameter *interface* (or (getenv "HOST") "127.0.0.1"))

; Repeatedly handle socket connections on `port` with `app`. `app` needs to
; accept a stream, which it should remember to close. No pre or post
; processing is done.
(defun serve (app &optional (port *port*) (interface *interface*))
  (let ((socket (socket-server port)))
    (format t "Listening on ~S:~D~%" interface port)
    (unwind-protect
      (loop (with-open-stream (stream (socket-accept socket))
              (funcall app stream)))
      (socket-server-close socket))))

; A stupid service which prints the stream's input to stdout and prints 'OK
; back to the stream.
(defun ok (stream)
  (let ((host (socket-stream-host stream))
        (peer (socket-stream-peer stream))
        (port (socket-stream-port stream))
        (input (read stream)))
    (format t "[~S:~D]~S Received ~S~%" host port peer input)
    (print 'OK stream)))

; A stupid client which connects to the server and sends the command-line
; arguments over the wire.
(defun client (args &optional (port *port*) (interface *interface*))
  (with-open-stream (stream (socket-connect port interface))
    (print args stream)
    (format t "~S~%" (read stream))))

(defun parse-command-line (args)
  (if (string= "serve" (car args))
    (serve #'ok)
    (client args)))

(parse-command-line *args*)
