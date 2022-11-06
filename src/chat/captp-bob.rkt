#lang racket

(require goblins
         goblins/actor-lib/bootstrap
         goblins/ocapn
         goblins/ocapn/netlayer/onion
         net/url)

(define (^greeter _bcom my-name)
  (lambda (your-name)
    (format "Hello ~a, my name is ~a!" your-name my-name)))

(define-vat-run b-run (make-vat))
(define bob (b-run (spawn ^greeter "Mikayla")))

(define-vat-run machine-run (make-vat))
(machine-run
 (define-values (onion-netlayer _onion-private-key _onion-service-id)
   (new-onion-netlayer))
 (define mycapn
   (spawn-mycapn onion-netlayer))

 (define bob-sturdyref
   ($ mycapn 'register bob 'onion))

 (displayln (format "Bob's sturdyref: ~a" (url->string (ocapn-sturdyref->url bob-sturdyref)))))


(sync (make-semaphore))
