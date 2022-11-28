#lang web-server/insta

(require goblins
         goblins/actor-lib/bootstrap
         goblins/actor-lib/methods
         goblins/utils/simple-sealers
         goblins/ocapn
         goblins/actor-lib/joiners
         goblins/ocapn/netlayer/onion
         goblins/ocapn/netlayer/fake-intarwebs
         xml
         net/url
)


(define-vat-run machine-run (make-vat))
(define mycapn
  (machine-run
   (define-values (onion-netlayer _onion-private-key _onion-service-id)
     (new-onion-netlayer))
   (spawn-mycapn onion-netlayer)))


(define (has-obj-ref? bindings)
  (and (exists-binding? 'obj bindings)
       (ocapn-sturdyref-url? (string->url (extract-binding/single 'obj bindings)))))

(define (binding-to-sref bindings)
  (string->ocapn-sturdyref
    (extract-binding/single 'obj bindings)))


(define (html-template body-content)
  (response/xexpr
   `(html

     (head (title "Object Fetcher"))
     (body ((style "margin:0;")) (div ((style "margin:0; display: flex;align-items: center;justify-content: center;min-height: 50vh;font-size: 1.25em; max-width: 10in; margin: 0 auto; margin-top: 1em;"))
                                      ,body-content)))))

(define go-back "<br/> <a href='javascript:history.back()'>Go back</a>")

(define (fetch-html sref)
  (define channel (make-channel))

  (thread
   (lambda ()
     (sleep 60)
     (channel-put channel
                  (format
                   "<div>Request timed out, maybe check if the sref is still live? ~a </div>" go-back))))

  (machine-run (define obj-vow (<- mycapn 'enliven sref))
               (define get-html-vow (<- obj-vow 'get-html))
               (on get-html-vow
                   (lambda (content)
                     (channel-put
                      channel content))
                   #:catch
                   (lambda (err)
                     (channel-put
                      channel
                      (format
                       "<div> ERROR: Failed to fetch html from object :( ~a </div>" go-back)))
                   #:finally
                   (lambda ()
                     (channel-put
                      channel
                      (format
                       "<div>If you're seeing this, something has gone _very_ wrong! ~a</div>" go-back)))))

     (html-template (string->xexpr (channel-get channel))))


(define (start req)
  (cond [(has-obj-ref? (request-bindings req))
         (define obj-ref (binding-to-sref (request-bindings req)))
         (fetch-html obj-ref)]
        [else
         (html-template
          `(div
            (div "Enter the sturdy ref for the object you want to display: ")
            (form
             (input ((name "obj")))
             (input ((type "submit"))))
            ,(cond
               [(exists-binding? 'obj (request-bindings req))
                `(div ((style "color:red;"))"Failed to parse sturdy ref, try again")]
               [else
                `(div)])))]
         )
  )
