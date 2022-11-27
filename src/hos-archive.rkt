#lang racket

(require goblins)
(require goblins/actor-lib/bootstrap)
(require goblins/actor-lib/methods)
(require goblins/utils/simple-sealers)

(require "heart-of-spritely.rkt")

; TODO:
; - Make Blog component on 1 machine
; - Make reviewer and post components
; - Make generic message sender components for doing stuff on the command line
; - Add lots of logs to everything so we can see what happens

;; Interesting capability stuff below
(define (old-spawn-blog-and-admin title)
  (define posts
    (spawn ^cell '()))
  (define (^blog _bcom)
    (methods
     [(get-title) title]
     [(get-posts) ($ posts)]))
  (define (^admin bcom)
    (methods
     [(add-post post)
      (define current-posts
        ($ posts))
      (define new-posts
        (cons post current-posts))
      ($ posts new-posts)]))
  (define blog (spawn ^blog))
  (define admin (spawn ^admin))
  (values blog admin))

(define (old-spawn-post-and-editor #:title (title #f) #:author (author #f) #:body (body #f))
  ;; the public blogpost
  (define (^post _bcom)
    (methods
     [(get-content)
      (define data-triple
        ($ editor 'get-data))
      (cons '*post* data-triple)]))
  (define (^editor bcom title author body)
    (methods
     [(update #:title (title title) #:author (author author) #:body (body body))
      (bcom (^editor bcom title author body))]
     [(get-data)
      (list title author body)]))
  (define post (spawn ^post))
  (define editor (spawn ^editor title author body))
  (values post editor))


(define vat (make-vat))



;(define-values (post editor)
;  (vat 'run
;       (lambda ()
;         (spawn-post-and-editor
;          #:title "A day in the Park"
;          #:author "Mikayla"
;          #:body "la-di-da..."))
;       ))

(define-values (blog admin)
  (vat 'run
       (lambda ()
         (spawn-blog-and-admin "Test blog!"))
       ))

(match-define (list test-post test-editor)
  (vat 'run
       (lambda ()
         ($ admin 'new-post-and-editor
            #:title "title"
            #:author "TEST"
            #:body "TEST TEST TEST"))
       ))

(define-vat-run vat-run vat)

(define admin-log (vat-run (spawn ^logger)))

(vat-run ($ admin 'add-post test-post))

(define-values (admin-for-robert roberts-admin-revoked?)
  (vat-run (spawn-logged-revocable-proxy-pair
            "Robert"
            admin
            admin-log
            )))

(vat-run ($ admin-for-robert 'edit-post test-post #:body "ROBERT-EDITED"))
(vat-run (display-blog blog))

(define-values (review-post review-editor reviewer)
  (vat-run
   (spawn-post-guest-editor-and-reviewer "SAMPLE" admin-for-robert)))

(vat-run ($ review-editor 'set-body "ATTENUATED EDITOR"))
(vat-run ($ review-editor 'set-title "ATTENUATED EDITOR TITLE"))

(vat-run ($ reviewer 'approve))

(vat-run (display-blog blog))
(vat-run ($ admin-log 'get-log))
