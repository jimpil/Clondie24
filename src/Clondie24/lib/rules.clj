(ns Clondie24.lib.rules
       (:refer-clojure :exclude [== >= <= > < =])
       (:use clojure.core.logic 
             clojure.core.logic.arithmetic
            [Clondie24.lib.core :only [translate-position collides?]])
       (:require [Clondie24.lib.util :as ut]))
       
(set! *unchecked-math* true)       
             
(def ^:const board (vec (range 8)))
                           

(defn bishop-moves 
"Returns the logical moves for a bishop (on a 8x8 grid) given its current position and direction."
[x y]
;(remove #(collides? [x y] % (ut/make-walker (ut/resolve-direction [x y] %)) boa m dir)  
(run* [q] 
(fresh [a b] 
  (membero a board) 
  (membero b board)
   (!= a x) 
   (!= b y)
    (project [x y a b]
       (== (Math/abs ^long (- x a)) 
           (Math/abs ^long (- y b)))   
   (== q [a b])))));)
     
(defn pawn-moves 
"Returns the logical moves for a pawn (on a 8x8 grid) given its current position and direction." 
[m boa x y dir]
 (let [xmax 8 ymax 8]
 (if (pos? dir) ;if moving south
  (run* [q] 
  (fresh [a b]
  (conde  
     [(= y 1) (= nil (get boa (try (translate-position x (+ y 2) m) (catch Exception e -1)))) 
      (== a x) (== b (+ y 2))] ;1st possibility (2 steps)
     [(< (+ y 1) ymax) (= nil (get boa (try (translate-position x (+ y 1) m) (catch Exception e -1))))  
      (== a x) (== b (+ y 1))] ;2nd possibility (1 step)
     [(< (+ y 1) ymax) (< (+ x 1) xmax) (!= nil (get boa (try (translate-position (+ x 1) (+ y 1) m) (catch Exception e -1))))
      (!= dir (:direction (get boa (try (translate-position (+ x 1) (+ y 1) m) (catch Exception e -1))))) 
      (== a (+ x 1)) (== b (+ y 1))] ;kill
     [(< (+ y 1) ymax) (>= (- x 1) 0)  (!= nil (get boa (try (translate-position (- x 1) (+ y 1) m) (catch Exception e -1)))) 
      (!= dir (:direction (get boa (try (translate-position (- x 1) (+ y 1) m) (catch Exception e -1))))) 
      (== a (- x 1)) (== b (+ y 1))]) ;kill 
    (== q [a b])))
  (run* [q] ;else moving north
  (fresh [a b]
  (conde  
    [(= y 6) (= nil (get boa (try (translate-position x (- y 2) m) (catch Exception e -1)))) 
     (== a x) (== b (- y 2))]  ;1st possibility (2 steps)
    [(>= (- y 1) 0) (= nil (get boa (try (translate-position x (- y 1) m) (catch Exception e -1)))) 
     (== a x) (== b (- y 1))]  ;2nd possibility (1 step)
    [(>= (- y 1) 0) (< (+ x 1) xmax) (!= nil (get boa (try (translate-position (+ x 1) (- y 1) m) (catch Exception e -1))))
     (!= dir (:direction (get boa (try (translate-position (+ x 1) (- y 1) m) (catch Exception e -1)))))
      (== a (+ x 1)) (== b (- y 1))] ;kill
    [(>= (- y 1) 0) (>= (- x 1) 0) (!= nil (get boa (try (translate-position (- x 1) (- y 1) m) (catch Exception e -1))))  
    (!= dir (:direction (get boa (try (translate-position (- x 1) (- y 1) m) (catch Exception e -1))))) 
    (== a (- x 1)) (== b (- y 1))]) ;kill
    (== q [a b]))))))
    
    
(defn checker-moves 
"Returns the logical moves for a checker (on a 8x8 grid) given its current position, board and direction."
[m boa x y dir] 
(let [xmax 8 ymax 8]
  (if (pos? dir) 
  (run* [q] 
   (fresh [a b] 
    (conde 
     [(< (+ y 1) ymax) (< (+ x 1) xmax) (== a (+ x 1)) (== b (+ y 1))
                       (= nil (get boa (try  (translate-position (+ x 1) (+ y 1) m) 
                                       (catch Exception e -1))))] ;landing pos must be vacant
     [(< (+ y 1) ymax) (>= (- x 1) 0) (== a (- x 1)) (== b (+ y 1))
                       (= nil (get boa (try (translate-position (- x 1) (+ y 1)  m)
                                       (catch Exception e -1))))] ;landing pos must be vacant
                       
     [(< (+ y 2) ymax) (< (+ x 2) xmax) (== a (+ x 2)) (== b (+ y 2))  ;attacking
                       (= (:direction (get boa (try (translate-position (+ x 1) (+ y 1)  m)
                                               (catch Exception e 0)))) (- dir))  ; pos in between must be enemy
                       (= nil  (get boa (try (translate-position (+ x 2) (+ y 2)  m)
                                        (catch Exception e -1))))] ;landing pos must be vacant
     [(>= (+ y 2) 0) (>= (- x 2) 0) (== a (- x 2)) (== b (+ y 2))  ;attacking
                       (= (:direction (get boa (try (translate-position (- x 1) (+ y 1)  m)
                                               (catch Exception e 0)))) (- dir))   ; pos in between must be enemy
                       (= nil  (get boa (try (translate-position (- x 2) (+ y 2)  m)
                                        (catch Exception e -1))))]) ;landing pos must be vacant
     (== q [a b])))
  (run* [q] 
   (fresh [a b ] 
    (conde 
     [(>= (- y 1) 0) (< (+ x 1) xmax) (== a (+ x 1)) (== b (- y 1)) 
                       (= nil (get boa (try (translate-position (+ x 1) (- y 1)  m)
                                        (catch Exception e -1))))]   ;landing pos must be vacant
     [(>= (- y 1) 0) (>= (- x 1) 0) (== a (- x 1)) (== b (- y 1))
                       (= nil (get boa (try (translate-position (- x 1) (- y 1)  m)
                                       (catch Exception e -1))))]   ;landing pos must be vacant
     [(>= (- y 2) 0) (< (+ x 2) xmax)  (== a (+ x 2)) (== b (- y 2))  ;attacking
                       (= (:direction (get boa (try (translate-position (+ x 1) (- y 1)  m)
                                               (catch Exception e 0)))) (- dir)) ; pos in between must be enemy
                       (= nil (get boa (try (translate-position (+ x 2) (- y 2)  m)
                                       (catch Exception e -1))))]    ;landing pos must be vacant                  
     [(>= (- y 2) 0) (>= (- x 2) 0) (== a (- x 2)) (== b (- y 2))  ;attacking
                         (= (:direction (get boa (try (translate-position (- x 1) (- y 1)  m)
                                                 (catch Exception e 0)))) (- dir)) ; pos in between must be enemy
                         (= nil  (get boa (try (translate-position (- x 2) (- y 2)  m)
                                          (catch Exception e -1))))])   ;landing pos must be vacant
     (== q [a b])))))) 
     
(defn prince-moves "Returns the logical moves for a checker-prince (can move in both directions)." 
[m boa x y dir]
(concat (checker-moves m boa x y dir)
        (checker-moves m boa x y (- dir))))        
    
(defn rook-moves 
"Returns the logical moves for a rook (on an empty 8x8 grid) given its current position."
[x y]
 (run* [q]
 (fresh [a b]
 (conde 
  [(membero a board) (!= a x) (== b y)]  ;y is constant
  [(membero b board) (!= b y) (== a x)]) ;x is constant
  (== q [a b]))))
  
(defn queen-moves 
"Returns the logical moves for a queen (on an empty 8x8 grid) given its current position and direction.
 A quen basically has the moving abilities of a rook combined with a bishop." 
[x y]
(concat (rook-moves   x y) 
        (bishop-moves x y))) 
   
(defn king-moves 
"Returns the logical moves for a king (on an empty 8x8 grid) given its current position."
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


(defn knight-moves 
"Returns the logical moves for a knight (on a 8x8 grid) given its current position." 
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




