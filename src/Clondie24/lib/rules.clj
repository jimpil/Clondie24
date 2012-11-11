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
     [(< (inc y) ymax) (= nil (get boa (try (translate-position x(inc y) m) (catch Exception e -1))))  
      (== a x) (== b (inc y))] ;2nd possibility (1 step)
     [(< (inc y) ymax) (< (inc x) xmax) (!= nil (get boa (try (translate-position (inc x) (inc y) m) (catch Exception e -1))))
      (!= dir (:direction (get boa (try (translate-position (inc x) (inc y) m) (catch Exception e -1))))) 
      (== a (inc x)) (== b (inc y))] ;kill
     [(< (inc y) ymax) (>= (dec x) 0)  (!= nil (get boa (try (translate-position (dec x) (inc y) m) (catch Exception e -1)))) 
      (!= dir (:direction (get boa (try (translate-position (dec x) (inc y) m) (catch Exception e -1))))) 
      (== a (dec x)) (== b (inc y))]) ;kill 
    (== q [a b])))
  (run* [q] ;else moving north
  (fresh [a b]
  (conde  
    [(= y 6) (= nil (get boa (try (translate-position x (- y 2) m) (catch Exception e -1)))) 
     (== a x) (== b (- y 2))]  ;1st possibility (2 steps)
    [(>= (dec y) 0) (= nil (get boa (try (translate-position x (dec y) m) (catch Exception e -1)))) 
     (== a x) (== b (dec y))]  ;2nd possibility (1 step)
    [(>= (dec y) 0) (< (inc x) xmax) (!= nil (get boa (try (translate-position (inc x) (dec y) m) (catch Exception e -1))))
     (!= dir (:direction (get boa (try (translate-position (inc x) (dec y) m) (catch Exception e -1)))))
      (== a (inc x)) (== b (dec y))] ;kill
    [(>= (dec y) 0) (>= (dec x) 0) (!= nil (get boa (try (translate-position (dec x) (dec y) m) (catch Exception e -1))))  
    (!= dir (:direction (get boa (try (translate-position (dec x) (dec y) m) (catch Exception e -1))))) 
    (== a (dec x)) (== b (dec y))]) ;kill
    (== q [a b]))))))
    
    
(defn checker-moves 
"Returns the logical moves for a checker (on a 8x8 grid) given its current position, board and direction."
[m boa x y dir] 
(let [xmax 8 ymax 8]
  (if (pos? dir) 
  (run* [q] 
   (fresh [a b] 
    (conde 
     [(< (inc y) ymax) (< (inc x) xmax) (== a (inc x)) (== b (inc y))
                       (= nil (get boa (try  (translate-position (inc x) (inc y) m) 
                                       (catch Exception e -1))))] ;landing pos must be vacant
     [(< (inc y) ymax) (>= (dec x) 0) (== a (dec x)) (== b (inc y))
                       (= nil (get boa (try (translate-position (dec x) (inc y)  m)
                                       (catch Exception e -1))))] ;landing pos must be vacant
                       
     [(< (+ y 2) ymax) (< (+ x 2) xmax) (== a (+ x 2)) (== b (+ y 2))  ;attacking
                       (= (:direction (get boa (try (translate-position (inc x) (inc y)  m)
                                               (catch Exception e 0)))) (- dir))  ; pos in between must be enemy
                       (= nil  (get boa (try (translate-position (+ x 2) (+ y 2)  m)
                                        (catch Exception e -1))))] ;landing pos must be vacant
     [(< (+ y 2) ymax) (>= (- x 2) 0) (== a (- x 2)) (== b (+ y 2))  ;attacking
                       (= (:direction (get boa (try (translate-position (dec x) (inc y)  m)
                                               (catch Exception e 0)))) (- dir))   ; pos in between must be enemy
                       (= nil  (get boa (try (translate-position (- x 2) (+ y 2)  m)
                                        (catch Exception e -1))))]) ;landing pos must be vacant
     (== q [a b])))
  (run* [q] 
   (fresh [a b ] 
    (conde 
     [(>= (dec y) 0) (< (inc x) xmax) (== a (inc x)) (== b (dec y)) 
                       (= nil (get boa (try (translate-position (inc x) (dec y)  m)
                                        (catch Exception e -1))))]   ;landing pos must be vacant
     [(>= (dec y) 0) (>= (dec x) 0) (== a (dec x)) (== b (dec y))
                       (= nil (get boa (try (translate-position (dec x) (dec y)  m)
                                       (catch Exception e -1))))]   ;landing pos must be vacant
     [(>= (- y 2) 0) (< (+ x 2) xmax)  (== a (+ x 2)) (== b (- y 2))  ;attacking
                       (= (:direction (get boa (try (translate-position (inc x) (dec y)  m)
                                               (catch Exception e 0)))) (- dir)) ; pos in between must be enemy
                       (= nil (get boa (try (translate-position (+ x 2) (- y 2)  m)
                                       (catch Exception e -1))))]    ;landing pos must be vacant                  
     [(>= (- y 2) 0) (>= (- x 2) 0) (== a (- x 2)) (== b (- y 2))  ;attacking
                         (= (:direction (get boa (try (translate-position (dec x) (dec y)  m)
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
    [(< (inc x) xmax) (< (inc y) ymax) (== a (inc x)) (== b (inc y))] ;1st possibility (diagonally)
    [(>= (dec x) 0) (>= (dec y) 0) (== a (dec x)) (== b (dec y))]     ;2nd possibility (diagonally)
    [(< (inc y) ymax) (== a x) (== b (inc y))]                        ;3rd possibility (x is constant)
    [(>= (dec y) 0) (== a x) (== b (dec y))]                          ;4th possibility (x is constant)
    [(>= (dec x) 0) (== b y) (== a (dec x))]                          ;5th possibility (y is constant)
    [(< (inc x) xmax) (== b y) (== a (inc x))]                        ;6th possibility (y is constant)
    [(< (inc x) xmax) (>= (dec y) 0) (== a (inc x)) (== b (dec y))]    ;7th possibility (diagonally)
    [(>= (dec x) 0) (< (inc y) ymax) (== a (dec x)) (== b (inc y))]   ;8th possibility (diagonally)
  ) 
   (== q [a b]))))) ;return each solution in a vector [x, y]


(defn knight-moves 
"Returns the logical moves for a knight (on a 8x8 grid) given its current position." 
 [x y]
(let [xmax 8 ymax 8]
 (run* [q] ;bring back all possible solutions
 (fresh [a b]
  (conde ;;like OR
    [(< (inc x) xmax) (< (+ y 2) ymax) (== a (inc x)) (== b (+ y 2))] ;1st possibility
    [(< (+ x 2) xmax) (< (inc y) ymax) (== a (+ x 2)) (== b (inc y))] ;2nd possibility
    [(< (+ x 2) xmax) (>= (dec y)   0) (== a (+ x 2)) (== b (dec y))] ;3rd possibility
    [(< (inc x) xmax) (>= (- y 2)   0) (== a (inc x)) (== b (- y 2))] ;4th possibility
    [(>= (dec x)   0) (>= (- y 2)   0) (== a (dec x)) (== b (- y 2))] ;5th possibility
    [(>= (- x 2)   0) (>= (dec y)   0) (== a (- x 2)) (== b (dec y))] ;6th possibility
    [(>= (- x 2)   0) (< (inc y) ymax) (== a (- x 2)) (== b (inc y))] ;7th possibility
    [(>= (dec x)   0) (< (+ y 2) ymax) (== a (dec x)) (== b (+ y 2))] ;8th possibility
  ) 
   (== q [a b]))))) ;return each solution in a vector [x, y]




