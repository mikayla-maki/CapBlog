#lang racket
(require goblins)
(require goblins/actor-lib/bootstrap)
(require goblins/actor-lib/methods)

(define a-vat
  (make-vat))

(define (^greeter bcom our-name)
  (lambda (your-name)
  (format "Hello ~a, my name is ~a!" your-name our-name)))

(define (^methods-greeter bcom my-name)
  (methods
    [(greet your-name)
      (format "Hello ~a, my name is ~a!" your-name my-name)]
    [(name)
      my-name]))



(define (^counter bcom [count 0])
  (methods
    [(count)
      count]
    [(add1)
     (bcom (^counter bcom (add1 count)))]))


(define (^friend bcom my-name)
  (lambda (your-name)
    (format "Hello ~a, my name is ~a!" your-name my-name)))

(define (^messages-friend bcom our-name)
  (lambda (friend)
    (on (<- friend our-name)
        (lambda (what-my-friend-said)
          (displayln (format "<~a>: I messaged my friend, and they said:"
                             our-name))
          (displayln (format "   \"~a\"" what-my-friend-said)))
        #:catch
        (lambda (_err)
          (displayln
           "I messaged my friend but they broke their response promise...")))))

(a-vat 'run
  (lambda ()
    (define alice2
      (spawn ^methods-greeter "Alice"))
      ($ alice2 'name)
      ($ alice2 'greet "Chris")))

; Actors + Functional + Immutable -> Goblin Magic!

(define-vat-run a-run
    a-vat)

(define alice
  (a-run (spawn ^friend "Alice")))

(define archie
  (a-run (spawn ^messages-friend "Archie")))

(define b-vat
  (make-vat))

(define-vat-run b-run
  b-vat)


(define bob
  (b-run (spawn ^messages-friend "Bob")))



;  {
;    Object_IDs -> (closure, last_msg_result)
;  }
;
;  Just keep around old maps
;
;


define (^counter bcom [count 0])
(methods
 ;; return the current count
 [(count)
  count]
 ;; Add one to the current counter
 [(add1)
  (bcom (^counter bcom (add1 count)))]))






(define (^memory-friend bcom my-name)
  (define greet-counter
    (spawn ^counter))
  (define recent-friend
    (spawn ^cell #f))
  (methods
   ;; Greet the user using their name
   [(greet your-name)
    ;; Increment count by one, since we were just called.
    ;; The counter starts at 0 so this will be correct.
    ($ greet-counter 'add1)
    (define greet-count
      ($ greet-counter 'count))
    ;; Who our friend was last time
    (define last-friend-name
      ($ recent-friend))
    ;; But now let's set the recent friend to be this name
    ($ recent-friend your-name)
    (if last-friend-name
        (format "Hello ~a, my name is ~a and I've greeted ~a times (last by ~a)!"
                your-name my-name greet-count last-friend-name)
        (format "Hello ~a, my name is ~a and I've greeted ~a times!"
                your-name my-name greet-count))]
   ;; return what our name is
   [(name)
    my-name]
   ;; check how many times we've greeted, without
   ;; incrementing it
   [(greet-count)
    ($ greet-counter 'count)]))


define (^memory-friend2 bcom my-name)
(define (next greet-count last-friend-name)
  (methods
   ;; Greet the user using their name
   [(greet your-name)
    (define greeting
      (if last-friend-name
          (format "Hello ~a, my name is ~a and I've greeted ~a times (last by ~a)!"
                  your-name my-name greet-count last-friend-name)
          (format "Hello ~a, my name is ~a and I've greeted ~a times!"
                  your-name my-name greet-count)))
    (bcom (next (add1 greet-count)
                your-name)
          greeting)]
   ;; return what our name is
   [(name)
    my-name]
   ;; check how many times we've greeted, without
   ;; incrementing it
   [(greet-count)
    greet-count]))
(next 1 #f))
