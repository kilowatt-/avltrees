#lang racket
(require racket/struct)

(provide NULL node node-l node-r node-v)

(define NULL empty)


(struct node (l r v)
  #:methods gen:custom-write
  [(define write-proc
     (make-constructor-style-printer
      (lambda (obj) 'node)
      (lambda (obj) (list
                     (print-node (node-l obj)) (print-node(node-r obj)) (node-v obj)))))]

  #:methods gen:equal+hash
  [(define (equal-proc n1 n2 node=?)
     (and (= (node-v n1) (node-v n2))
          (node=? (node-l n1) (node-l n2))
          (node=? (node-r n1) (node-r n2))))
   (define (hash-proc n hash-recur)
     (node-v n))
   (define (hash2-proc n hash2-recur)
     (node-v n))])

;; Node is one of:
;; - NULL
;; - (make-node Node Node Natural)
;; interp. A node in a binary search tree, with a left subnode, a right subnote and a value (natural)
;; INVARIANT: l < v, r > v

#;
(define (fn-for-node t)
  (cond [(empty? t) (...)]
        [else (... (fn-for-node (node-l t))
                   (fn-for-node (node-r t))
                   (... (node-v t)))]))

;; Node -> Node or String
;; produces NULL if given node is empty, produces node otherwise

(define (print-node t)
  (cond [(empty? t) "NULL"]
        [else t]))

