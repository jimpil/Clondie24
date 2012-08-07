(ns Clondie24.rules
       (:refer-clojure :exclude [== >= <= > < =])
       (:use clojure.core.logic 
             clojure.core.logic.arithmetic))
             
(def ^:const board (vec (range 8)))             

(defn bishop-moves 
"Returns the available moves for a bishop (on a 8x8 grid) given its current position and direction."
[x y]
(run* [q] 
(fresh [a b] 
  (membero a board) 
  (membero b board)
   (!= a x) 
   (!= b y)
    (project [x y a b]
    (== (Math/abs (- x a)) 
        (Math/abs (- y b)))
           (== q [a b])))))
     
(defn pawn-moves 
"Returns the available moves for a pawn (on a 8x8 grid) given its current position and direction." 
[x y dir]
 (let [xmax 8 ymax 8]
 (if (pos? dir) ;if moving south
  (run* [q] 
  (fresh [a b]
  (conde  
    [(< (+ y 2) ymax) (= y 1) (== a x) (== b (+ y 2))] ;1st possibility (2 steps)
    [(< (+ y 1) ymax) (== a x) (== b (+ y 1))])        ;2nd possibility (1 step)
    (== q [a b])))
  (run* [q] ;else moving north
  (fresh [a b]
  (conde  
    [(< (- y 2) ymax) (= y 6) (== a x) (== b (- y 2))]  ;1st possibility (2 steps)
    [(< (- y 1) ymax) (== a x) (== b (- y 1))])         ;2nd possibility (1 step)
    (== q [a b])))))) 
    
(defn checker-moves 
"Returns the available moves for a checker (on a 8x8 grid) given its current position. No kills."
[x y dir] 
(let [xmax 8 ymax 8]
  (if (pos? dir) 
  (run* [q] 
   (fresh [a b] 
    (conde 
     [(< (+ y 1) ymax) (< (+ x 1) xmax) (== a x) (== b y)]
     [(< (+ y 1) ymax) (< (- x 1) xmax) (== a x) (== b y)]
     (== q [a b]))))
  (run* [q] 
   (fresh [a b] 
    (conde 
     [(< (- y 1) ymax) (< (+ x 1) xmax) (== a x) (== b y)]
     [(< (- y 1) ymax) (< (- x 1) xmax) (== a x) (== b y)]
     (== q [a b])))))))    
    
(defn rook-moves 
"Returns the available moves for a rook (on a 8x8 grid) given its current position."
[x y]
 (run* [q]
 (fresh [a b]
 (conde 
  [(membero a board) (!= a x) (== b y)]  ;y is constant
  [(membero b board) (!= b y) (== a x)]) ;x is constant
  (== q [a b]))))
  
(defn queen-moves 
"Returns the available moves for a queen (on a 8x8 grid) given its current position and direction.
 A quen basically has the moving abilities of a rook combined with a bishop." 
[x y]
(concat (rook-moves x y) 
        (bishop-moves x y))) 
   
(defn king-moves 
"Returns the available moves for a king (on a 8x8 grid) given its current position."
[x y]
(let [xmax 8 ymax 8]
 (run* [q]
 (fresh [a b]
  (conde 
    [(< (+ x 1) xmax) (< (+ y 1) ymax) (== a (+ x 1)) (== b (+ y 1))] ;1st possibility (diagonally)
    [(>= (- x 1) 0) (>= (- y 1) 0) (== a (- x 1)) (== b (- y 1))]     ;2nd possibility (diagonally)
    [(< (+ y 1) ymax) (== a x) (== b (+ y 1))]                        ;3rd possibility (x is constant)
    [(>= (- y 1) 0) (== a x) (== b (- y 1))]                          ;4th possibility (x is constant)
    [(>= (- x 1) 0) (== b y) (== a (- x 1))]                          ;5th possibility (y is constant)
    [(< (+ x 1) xmax) (== b y) (== a (+ x 1))]                        ;6th possibility (y is constant)
    [(< (+ x 1) xmax) (> (- y 1) 0) (== a (+ x 1)) (== b (- y 1))]    ;7th possibility (diagonally)
    [(>= (- x 1) 0) (< (+ y 1) ymax) (== a (- x 1)) (== b (+ y 1))]   ;8th possibility (diagonally)
  ) 
   (== q [a b]))))) ;return each solution in a vector [x, y]

;(run number-of-results [q] & goals)
;(run* [q] & goals)

(defn knight-moves 
"Returns the available moves for a knight (on a 8x8 grid) given its current position." 
 [x y]
(let [xmax 8 ymax 8]
 (run* [q] ;bring back all possible solutions
 (fresh [a b]
  (conde ;;like OR
    [(< (+ x 1) xmax) (< (+ y 2) ymax) (== a (+ x 1)) (== b (+ y 2))] ;1st possibility
    [(< (+ x 2) xmax) (< (+ y 1) ymax) (== a (+ x 2)) (== b (+ y 1))] ;2nd possibility
    [(< (+ x 2) xmax) (>= (- y 1)   0) (== a (+ x 2)) (== b (- y 1))] ;3rd possibility
    [(< (+ x 1) xmax) (>= (- y 2)   0) (== a (+ x 1)) (== b (- y 2))] ;4th possibility
    [(>= (- x 1)   0) (>= (- y 2)   0) (== a (- x 1)) (== b (- y 2))] ;5th possibility
    [(>= (- x 2)   0) (>= (- y 1)   0) (== a (- x 2)) (== b (- y 1))] ;6th possibility
    [(>= (- x 2)   0) (< (+ y 1) ymax) (== a (- x 2)) (== b (+ y 1))] ;7th possibility
    [(>= (- x 1)   0) (< (+ y 2) ymax) (== a (- x 1)) (== b (+ y 2))] ;8th possibility
  ) 
   (== q [a b]))))) ;return each solution in a vector [x, y]




