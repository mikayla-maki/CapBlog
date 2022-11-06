#lang racket

(require goblins
         goblins/actor-lib/bootstrap
         goblins/ocapn
         goblins/ocapn/netlayer/onion
         net/url)


;; Set up machine
(define-vat-run machine-run (make-vat))
(define mycapn
  (machine-run
   (define-values (onion-netlayer _onion-private-key _onion-service-id)
     (new-onion-netlayer))
   (spawn-mycapn onion-netlayer)))


(define alice-sref
  (command-line #:args (alice-sref-str)
                (string->ocapn-sturdyref alice-sref-str)))


;; Set up "alice"
(define-vat-run a-run (make-vat))
(a-run
 (define bob-greeter-vow (<- mycapn 'enliven alice-sref))
 (on (<- bob-greeter-vow "Alice")
     (lambda (heard-back)
       (displayln (format "Alice heard back: ~a" heard-back))
       ;; we're done
       (semaphore-post finished))))

(define finished (make-semaphore))
(sync finished)
