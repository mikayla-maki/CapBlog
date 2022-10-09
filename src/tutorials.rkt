#lang web-server/insta

(struct post (title body))

(define BLOG (list (post "First post!"
                         "First post content...")
                   (post "Second post..."
                         "Second post content...")))

(define (print-post post)
  `(div (span ,(string-append "title: " (post-title post)))
        (div ,(string-append "body: " (post-body post)))))

(define (start request)
  (response/xexpr
   `(html
     (head (title "my blog"))
     (body (h1 "under construction")
           (div ,@(map print-post BLOG)))
     )
   ))

















; Notes from Bart:
; Question of imagination, to what extent are our existing problems
; caused by permissions?
; Cockpit problem: 40 years of permission based systems
; How to make it interesting:
; - How could we hope to do better?
; Place to start: Kerberos

; Biggest remaining problem in security: timeout problem.
; Timeout shorter -> More secure? Now you have problems with re-issuing
; Timeout longer ->
; Remote printing:
; If you have this | that -> You can do whatever you want
; Think about a case where a really sophisticated, customizable security policy.

; Role based authentication: Prove to you that I have a role
; The channel through which you get the capability:

; Capabilities are in sharing.
; What kind of systems today are suffering from these kinds of problems
;   - ROC tradeoff
; THESE ARE THE CAPABILITIES I NEED TO PERFORM THIS OPERATION:
;  - The kinds of priveleges I want to provide:
;    - Post
;    - Read
;    - Share
;    - Comment
;    - How do we get these capabilities?
;    - What do we do when we get them?
;    - Revocation is very important!

; Work like a software engineer, write out a coherent requirements spec
;  - Focus on User Authentication and Management / Priveleges
;    (what can people do and not do)
;  - How much of this even needs capabilities?
;  - Everything is identity based, not role based :(
; Everything is tied to a specific identity
