(ns Clondie24.rules
       (:refer-clojure :exclude [== >= <= > < =])
       (:use clojure.core.logic 
             clojure.core.logic.arithmetic))

(defn ^Boolean diagonally? 
"Returns true if moving diagonally, false otherwise. Expects 2 vectors denoting start/end poisitions." 
[from to] )
(defn ^Boolean forward? 
"Returns true if moving forwards, false otherwise. Expects 2 vectors denoting start/end poisitions." 
[from to])
(defn ^Boolean backward? 
"Returns true if moving backwards, false otherwise. Expects 2 vectors denoting start/end positions."
[from to])
(defn ^Boolean left? 
"Returns true if moving left, false otherwise. Expects 2 vectors denoting start/end positions."
[from to])
(defn ^Boolean right? 
"Returns true if moving right , false otherwise. Expects 2 vectors denoting start/end positions." 
[from to])

(defn ^Boolean legal-move? 
"Returns true if the move is legal according to the rules , false otherwise. Expects a direction (1/-1) and 2 vectors denoting start/end positions ." 
[board direction from to])



;(run number-of-results [q] & goals)
;(run* [q] & goals)

(defn knight-moves [x y]
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
    [(>= (- x 1)   0) (< (+ y 2) ymax) (== a (+ x 1)) (== b (+ y 2))] ;8th possibility
  ) 
   (== q [a b])) ;return each solution in a vector [x, y]
)))

; The following 8 moves describe all the possible moves a knight
; may make on a chess board, without leaving the chess board. The
; size of the chess board is given by (Xmax, Ymax).
(comment 
move([X, Y, Xmax, Ymax], [A, B, Xmax, Ymax]) :-
X + 1 < Xmax,
Y + 2 < Ymax,
A is X + 1,
B is Y + 2. ;;---------------

move([X, Y, Xmax, Ymax], [A, B, Xmax, Ymax]) :-
X + 2 < Xmax,
Y + 1 < Ymax,
A is X + 2,
B is Y + 1.;;------------------

move([X, Y, Xmax, Ymax], [A, B, Xmax, Ymax]) :-
X + 2 < Xmax,
Y - 1 >= 0,
A is X + 2,
B is Y - 1.;;------------------

move([X, Y, Xmax, Ymax], [A, B, Xmax, Ymax]) :-
X + 1 < Xmax,
Y - 2 >= 0,
A is X + 1,
B is Y - 2. ;--------------

move([X, Y, Xmax, Ymax], [A, B, Xmax, Ymax]) :-
X - 1 >= 0,
Y - 2 >= 0,
A is X - 1,
B is Y - 2. ;;------------------

move([X, Y, Xmax, Ymax], [A, B, Xmax, Ymax]) :-
X - 2 >= 0,
Y - 1 >= 0,
A is X - 2,
B is Y - 1. ;;--------------------

move([X, Y, Xmax, Ymax], [A, B, Xmax, Ymax]) :-
X - 2 >= 0,
Y + 1 < Ymax,
A is X - 2,
B is Y + 1. ;;--------------------

move([X, Y, Xmax, Ymax], [A, B, Xmax, Ymax]) :-
X - 1 >= 0,
Y + 2 < Ymax,
A is X - 1,
B is Y + 2.
)

