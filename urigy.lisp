(defpackage :org.bjjb.urigy
  (:use :common-lisp)
  (:use :ext)
  (:export :server :client :run))

(in-package :org.bjjb.urigy)

; default interface to bind to
(defconstant +interface+ (or (getenv "HOST") "127.0.0.1"))
; default port
(defconstant +port+ (parse-integer (or (getenv "PORT") "10923") :radix 10))
; default encoding (can't be overridden, because UTF-8 is the best)
(defconstant +charset+ charset:utf-8)
; the number of bits in a token
(defconstant +token-size+ 256)

(defun store (hash key value)
  "Store value in hash as key"
  (setf hash (setf (gethash key *dict*) value)))

(defun fetch (hash key)
  "Fetch key from hash"
  (multiple-value-bind (v x) (gethash key hash) v))

(defun make-token ()
  (format nil "~X" (random (expt 2 +token-size+))))

(defun server (handler &optional (port +port+) (interface +interface+))
  "Accepts a connection on interface:port and calls handler on the stream"
  (flet ((accept (socket) (socket-accept socket :external-format +charset+)))
    (let ((socket (socket-server port)))
      (format t "Listening on ~S:~D~%" interface port)
      (unwind-protect
        (loop (with-open-stream (stream (accept socket))
                (let ((peer (socket-stream-peer stream)))
                  (format t "[~A]~%" peer)
                  (funcall handler stream))))
        (socket-server-close socket)))))

(defun client (args &optional (port +port+) (interface +interface+))
  "Makes a connection to the server on interface:port and sends the args"
  (flet ((connect () (socket-connect port interface :external-format +charset+)))
    (with-open-stream (stream (connect))
      (format t "~S~%" (socket-stream-host stream))
      (print args stream)
      (format t "~S~%" (read stream)))))

(defun make-cli (name description action &optional (options ()) (commands ()))
  "Gets a function which can parse a list of strings"
  (let* ((opts ())
         (cmds ()))
    (labels ((mkopt (spec)  (format t "mkopt ~S~%" spec))
             (opt-p (opt)   (assoc opt opts))
             (mkcmd (spec)  (format t "mkcmd ~S~%" spec))
             (cmd-p (cmd)   (assoc cmd cmds)))
      (mapcar #'mkopt options)
      (mapcar #'mkcmd commands)
      (lambda (args)
        (if (opt-p (car args))
          (opt (car args) (cdr args)))
        (if (cmd-p (car args))
          (cmd (car args) (cdr args) opts))
        (funcall action args)))))

(defparameter *cli*
  (make-cli "urigy"
            "Simple programmable network key/value store"
            (lambda (args) (format t "ACTION! ~S~%" args))
            '(("interface" "i" "listen host"   :string)
              ("port"      "p" "listen port"   :integer)
              ("token"     "t" "user token"    :string)
              ("help"      "h" "print help"    #'help)
              ("version"   "V" "show version"  (lambda ()
                                                 (format t "v~D" +version+)))
              ("verbose"   "v" "be noisier"    :boolean)
              ("debug"     "d" "be very noisy" :debug))
            '(("help"      "get specific help" 'help)
              ("server"    "start a server"    'server)
              ("ping"      "ping a server"     'ping)
              ("register"  "obtain a token"    'register))))

(defun run (args)
  "Runs the program; args should be a list of strings"
  (format t "Running ~A on ~A~%" *cli* args)
  (funcall *cli* args))

(run *args*)
