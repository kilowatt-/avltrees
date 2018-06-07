#lang racket
(require "AVLtrees_DD.rkt")
(require 2htdp/image)

;; Functions for operating on an AVL Tree.
;; Copyright 2018 Kean Wong

(provide height balanced? balance rotate insert node node-l node-r node-v NULL delete)

;; Node -> Boolean
;; returns true if tree is balanced (absolute value of R-L <= 1)
;(define (balanced? t) false)

(define (balanced? t)
  (cond [(empty? t) true]
        [else (<= (abs (- (height (node-l t))
                          (height (node-r t)))) 1)]))

;; Node -> Natural
;; calculates the height of a given tree

;(define (height t) 0)

(define (height t)
  (local [(define (height t rsf)
            (cond [(empty? t) rsf]
                  [else (max (height (node-l t) (add1 rsf))
                             (height (node-r t) (add1 rsf)))]))]
    (height t -1)))

;; Number Node -> Node
;; Inserts a node with value n into the appropriate place in the tree


;(define (insert n t) empty)

(define (insert n t)
  (local [(define new-node (node NULL NULL n))]
    (cond [(empty? t) new-node]
          [else (if (< n (node-v t))
                    (balance (node (insert n (node-l t)) (node-r t) (node-v  t)))
                    (balance (node (node-l t) (insert n (node-r t)) (node-v t))))])))

;; Node -> Node
;; rebalances a tree, if necessary


;;(define (balance n) n)

(define (balance n)
  (if (balanced? n)
      n
      (rotate n)))

;; Node -> Node
;; rotates a tree appropriately
;; ASSUME: Tree is unbalanced



;; Template from Node
(define (rotate n)
  (local [(define (left-heavy? n) (> (height (node-l n)) (height (node-r n))))
          (define (right-heavy? n) (> (height (node-r n)) (height (node-l n))))]
    (cond [(left-heavy? n) (if (right-heavy? (node-l n))
                               (rl-rotate n)
                               (r-rotate n))]
          [else (if (left-heavy? (node-r n))
                    (lr-rotate n)
                    (l-rotate n))])))

;; Node -> Node
;; Performs a L rotation on a tree

(define (l-rotate n)
  (node (node (node-l n) (node-l (node-r n)) (node-v n)) (node-r (node-r n)) (node-v (node-r n))))

;; Node -> Node
;; Performs a R rotation on a tree


(define (r-rotate n)
  (node (node-l (node-l n)) (node (node-r (node-l n)) (node-r n) (node-v n)) (node-v (node-l n))))

;; Node -> Node
;; Performs a LR rotation on a tree

(define (lr-rotate n)
  (l-rotate (node (node-l n) (r-rotate (node-r n)) (node-v n))))

;; Node -> Node
;; Performs a RL rotation on a tree

(define (rl-rotate n)
  (r-rotate (node (l-rotate (node-l n)) (node-r n) (node-v n))))

;; Natural Node -> Node
;; Deletes a node from a tree

(define (delete t n)
  (cond [(empty? t) t]
        [else (cond [(= n (node-v t)) (balance (handle-delete t))]
                    [(< n (node-v t)) (balance (node (delete (node-l t) n) (node-r t) (node-v t)))]
                    [else (balance (node (node-l t) (delete (node-r t) n) (node-v t)))])]))

;; Node -> Node
;; Appropriately handles deletion of the node
;; ASSUME: Tree is not empty

(define (handle-delete t)
  (cond [(and (empty? (node-l t)) (empty? (node-r t))) empty]
        [(and (not (empty? (node-l t))) (not (empty? (node-r t)))) (predecessor-delete t)]
        [else (if (empty? (node-l t))
                  (node-r t)
                  (node-l t))]))

;; Node -> Node
;; Performs a predecessor deletion on a tree

(define (predecessor-delete t)
  (local [(define predecessor (get-furthest-right (node-l t)))]
    (node (delete-rightmost (node-l t)) (node-r t) (node-v predecessor))))
    
;; Node -> Node
;; Gets the right most node in a tree

(define (get-furthest-right t)
  (local [(define (get-furthest-right t rsf)
            (cond [(empty? t) rsf]
                  [else (get-furthest-right (node-r t) t)]))]
    (get-furthest-right t t)))

;; Node -> Node
;; Deletes the rightmost element in a tree

(define (delete-rightmost t)
  (cond [(empty? t) empty]
        [else (if (empty? (node-r t))
                  empty
                  (node (node-l t) (delete-rightmost (node-r t)) (node-v t)))]))

;; Node -> Image
;; Produces an image of a tree

(define RADIUS 15)
(define COLOUR "blue")
(define TEXT-COLOUR "white")
(define TEXT-SIZE 12)


(define (render t)
  (cond [(empty? t) empty-image]
        [else (above (node-image (node-v t))
                     (beside (render (node-l t)) (render (node-r t))))]))

;; Natural -> Image
;; Produces an image of a node in a tree represented by a circle with text in the center

(define (node-image n)
  (overlay
   (text (number->string n) TEXT-SIZE TEXT-COLOUR)
   (circle RADIUS "solid" COLOUR)))











