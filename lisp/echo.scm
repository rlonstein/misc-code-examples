#!/opt/local/bin/gosh
;;;
;;; select-based echo server in Gauche scheme adapted
;;; from Shiro Kawai's example http://practical-scheme.net/gauche/
;;;

(use gauche.net)
(use gauche.selector)
(use srfi-19)

(define (echo-server port)
  (let ((selector (make <selector>))
        (server   (make-server-socket 'inet port :reuse-addr? #t :backlog 5)))

    (define (logger str)
      (let ((now (current-time 'time-utc)))
        (format (standard-output-port) "~a - ~a~%"
                (date->string (time-utc->date now) "~Y~m~d ~H:~M:~S") str)))

    (define (accept-handler sock flag)
      (let* ((client (socket-accept server))
             (output (socket-output-port client)))
        (logger (format #f "connection from [~a]" (socket-address client)))
        (selector-add! selector
                       (socket-input-port client :buffering #f)
                       (lambda (input flag)
                         (echo client input output))
                       '(r))))

    (define (echo client input output)
      (let ((str (read-block 1024 input)))
        (if (eof-object? str)
            (begin (logger (format #f "EOF from client [~a]" (socket-address client)))
                   (selector-delete! selector input #f #f)
                   (socket-close client))
            (begin (logger (format #f "Echoed [~a] bytes for client [~a]"
                                   (string-size str) (socket-address client)))
                   (display str output)
                   (flush output)))))

    (with-signal-handlers 
        (((sys-sigset SIGINT SIGTERM SIGQUIT) => (lambda (sig)
                                                   (logger (format #f "received SIGNAL [~a]" sig))
                                                   (let ((sockstatus (socket-status server)))
                                                     (logger (format #f "Socket status [~a]" sockstatus))
                                                     (when (or (eq? sockstatus 'bound)
                                                               (eq? sockstatus 'listening)
                                                               (eq? sockstatus 'connected)
                                                               (eq? sockstatus 'closed))
                                                       (logger "Shutting down socket...")
                                                       (socket-shutdown server)))
                                                   (logger "Exiting...")
                                                   (exit 1))))
      (lambda ()
        (logger "Setting up socket...")
        (selector-add! selector
                       (socket-fd server)
                       accept-handler
                       '(r))
        (logger "Entering accept loop...")
        (do () (#f) (selector-select selector))))))

(define (main args)
  (echo-server 5050)
  0)
