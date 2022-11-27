#lang racket

(require goblins)
(require goblins/actor-lib/bootstrap)
(require goblins/actor-lib/methods)
(require goblins/ocapn)
(require goblins/ocapn/netlayer/onion)
(require net/url)

(define (spawn-post-guest-editor-and-reviewer author blog-admin)
  (define post-and-editor-vow
    (<- blog-admin 'new-post-and-editor #f author #f))
  (define post-vow
    (on post-and-editor-vow first
        #:promise? #t))
  (define editor-vow
    (on post-and-editor-vow second
        #:promise? #t))

  (define submitted-already? (spawn ^cell #f))

  (define (ensure-not-submitted)
    (when ($ submitted-already?)
      (error "Already Submitted!")))

  (define (^reviewer _bcom)
    (methods
     [(approve)
      (on post-vow
          (lambda (post)
            (ensure-not-submitted)
            ($ submitted-already? #t)
            (<- blog-admin 'add-post post))
          #:promise? #t)]))

  (define (^restricted-editor _bcom)
    (methods
     [(set-title new-title)
      (ensure-not-submitted)
      (<- editor-vow 'update-title new-title)]
     [(set-body new-body)
      (ensure-not-submitted)
      (<- editor-vow 'update-body new-body)]))

  (define reviewer (spawn ^reviewer))
  (define restricted-editor (spawn ^restricted-editor))
  (values post-vow restricted-editor reviewer))

(define-vat-run machine-run (make-vat))
(define mycapn
  (machine-run
   (define-values (onion-netlayer _onion-private-key _onion-service-id)
     (new-onion-netlayer))
   (spawn-mycapn onion-netlayer)))


(define (hydrate-sturdyref str)
  (machine-run
   ($ mycapn 'enliven (string->ocapn-sturdyref str))))

(define (new-reviewable-post title live-admin-vow)
  (machine-run
   (define-values
     (post restricted-editor reviewer)
     (spawn-post-guest-editor-and-reviewer title live-admin-vow))

   (define editor-sref ($ mycapn 'register restricted-editor 'onion))
   (display "editor sref: ")
   (displayln (format "~a\n" (url->string (ocapn-sturdyref->url editor-sref))))

   (define reviewer-sref ($ mycapn 'register reviewer 'onion))
   (display "reviewer sref: ")
   (displayln (format "~a\n" (url->string (ocapn-sturdyref->url reviewer-sref))))

   (define post-sref ($ mycapn 'register post 'onion))
   (display "post sref: ")
   (displayln (format "~a\n" (url->string (ocapn-sturdyref->url post-sref))))

   (values post restricted-editor reviewer)))

;(vat-run ($ review-editor 'set-body "ATTENUATED EDITOR"))
;(vat-run ($ review-editor 'set-title "ATTENUATED EDITOR TITLE"))

;(vat-run ($ reviewer 'approve))
