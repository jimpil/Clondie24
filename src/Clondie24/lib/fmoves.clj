(ns Clondie24.lib.fmoves
    (:require [clojure.math.combinatorics :as combi]
              [Clondie24.lib.util :as ut]
              [Clondie24.lib.core :as core]))
              
(defn round-neighbours 
"Returns all the circular neighbouring positions using the specified distance on a dimx*dimy board."
([[x y :as coord] distance [dimx dimy]]
  (let [in-board? #(ut/within-limits? [dimx dimy] %)]  
     (->> (combi/cartesian-product (range (- x distance) (inc (+ x distance))) 
                                   (range (- y distance) (inc (+ y distance))))
     (filter #(and (in-board? %) 
                   (not= coord %)))) ))
([[x y] distance] 
  (round-neighbours [x y] distance [8 8]))
([[x y]] 
  (round-neighbours [x y] 1 [8 8])) ) 

(defn vertical-neighbours 
"Returns all the vertically neighbouring positions using the specified distance on a dimx*dimy board."
([[x y :as coord] distance [dimx dimy]]
   (let [in-board? #(ut/within-limits? [dimx dimy] %)]
      (->> (combi/cartesian-product [x] ;x stays constant
                                    (range (- y distance) (inc (+ y distance))))
        (filter #(and (in-board? %) 
                      (not= coord %))))))
([[x y] distance] 
  (vertical-neighbours [x y] distance [8 8]))
([[x y]] 
  (vertical-neighbours [x y] 1 [8 8])) )

(defn horizontal-neighbours 
"Returns all the horizontally neighbouring positions using the specified distance on a dimx*dimy board."
([[x y :as coord] distance [dimx dimy]]
   (let [in-board? #(ut/within-limits? [dimx dimy] %)]
     (->> (combi/cartesian-product (range (- x distance) (inc (+ x distance))) 
                                   [y]) ;y stays constant                                    
        (filter #(and (in-board? %) 
                      (not= coord %))))))
([[x y] distance] 
  (horizontal-neighbours [x y] distance [8 8]))
([[x y]] (horizontal-neighbours [x y] 1 [8 8])) )

(defn diagonal-neighbours 
"Returns all the diagonnally neighbouring positions using the specified distance on a dimx*dimy board."
([[x y] distance [dimx dimy]]
   (let [in-board? #(ut/within-limits? [dimx dimy] %)]
     (for [dx (range (- distance) (inc distance))
           dy (range (- distance) (inc distance)) 
        :let [[nx ny :as new-pos] [(+ dx x) (+ dy y)]]
        :when (and (not= 0 dx dy)
                   (in-board? new-pos)
                    (=  (Math/abs ^long (- x nx))
                        (Math/abs ^long (- y ny))))]
     new-pos)))
([[x y] distance] 
  (diagonal-neighbours [x y] distance [8 8]))
([[x y]] 
  (diagonal-neighbours [x y] 1 [8 8])) )  

(defn rook-moves  [[x y :as pos]]
  (concat (vertical-neighbours pos 7) 
          (horizontal-neighbours pos 7)))

(defn bishop-moves  [[x y :as pos]]
  (diagonal-neighbours pos 7))
  
(defn queen-moves [[x y :as pos]]
  (concat (bishop-moves pos)  
          (rook-moves   pos)))

(defn king-moves [[x y :as pos]]
  (round-neighbours pos 1)) 
  
(defn pawn-moves [b [x y :as pos] dir]
  (let [valid? (fn [[newX newY :as new-pos]]
                 (let [vacant? (core/vacant? core/mappings-8x8 b new-pos)]  
                   (cond 
                      (= x newX) vacant?
                      vacant?  false)))
        kings   (king-moves pos) ;;use the king to get the surrouding positions          
        front3  (filter (if (pos? dir) ;;if moving downwards
                          (fn [[kx ky]] (> ky y)) ;;keep greater ys
                          (fn [[kx ky]] (< ky y))) ;;else keep smaller ys
                  kings)
        front4  (if (pos? dir) 
                  (if (= y 1) (conj front3 [x (+ y 2)]) front3)
                  (if (= y 6) (conj front3 [x (- y 2)]) front3))] 
    (filter valid? front4) ))
     
(defn knight-moves [[x y :as pos]]
 (let [around (round-neighbours pos 2)
       valid? (fn [[nx ny]] 
                (or (and (= nx (inc x)) (= ny (+ y 2)))
                    (and (= nx (dec x)) (= ny (+ y 2)))
                    (and (= nx (+ x 2)) (= ny (inc y)))
                    (and (= nx (+ x 2)) (= ny (dec y)))
                    (and (= nx (inc x)) (= ny (- y 2))) 
                    (and (= nx (dec x)) (= ny (- y 2)))
                    (and (= nx (- x 2)) (= ny (dec y)))
                    (and (= nx (- x 2)) (= ny (inc y)))))]
   (filter valid? around)))     
                     
