#lang racket
(require "AVLtrees_DD.rkt")
(provide height balanced? balance rotate insert node node-l node-r node-v NULL)

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









