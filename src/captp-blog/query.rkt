#lang racket

(require goblins
         goblins/actor-lib/bootstrap
         goblins/ocapn
         goblins/ocapn/netlayer/onion)

;; Set up machine
(define-vat-run machine-run (make-vat))
(define mycapn
  (machine-run
   (define-values (onion-netlayer _onion-private-key _onion-service-id)
     (new-onion-netlayer))
   (spawn-mycapn onion-netlayer)))

(define message-args (make-parameter null))
(define-values (sref method)
  (command-line
   #:multi
   [("-a" "--arg") arg
                   "Add an argument to the message"
                   (message-args (cons arg (message-args)))]
   #:args (sref-str symbol)
   (values (string->ocapn-sturdyref sref-str)
           (string->symbol symbol))))

(message-args (cons method (message-args)))



(define-vat-run a-run (make-vat))
(a-run
 (define other-vow (<- mycapn 'enliven sref))
 (on (apply <- (cons other-vow (message-args)))
     (lambda (heard-back)
       (displayln (format "heard back: ~a" heard-back))
       ;; we're done
       (semaphore-post finished))))

(define finished (make-semaphore))
(sync finished)
