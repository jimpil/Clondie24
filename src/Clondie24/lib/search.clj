(ns Clondie24.lib.search 
      (:require [Clondie24.lib.core :as core]
                [clojure.core.reducers :as r]))

(set! *unchecked-math* true)                 
      
(def curr-game "Before any searching we need a game-map." (promise))
(declare score-by-count score-naive next-level)

(defrecord Tree [root direction children])
(defrecord Move->Tree [move tree])
(defrecord Move->Board [move board])
(defrecord Move-Value  [move value])

(def mmm (atom 0))

(defn best 
([]    nil)
([best next]
  (if (pos? (compare (:value best) (:value next))) 
    best next))) ;(:move best) (:move rest)
	                  
(defn my-max 
([x y] (max x y))
([]  Integer/MIN_VALUE))
   
(defn my-min
([x y] (min y x))
([]  Integer/MAX_VALUE))             

 
(defn game-tree "Generate the game tree."
  [dir board successors-fn] 
  (Tree. board dir 
     (r/map #(Move->Tree. (:move %) 
                          (game-tree (- dir) (:board %) successors-fn)) 
             (successors-fn board dir))))

    
(defn search "The recursion of min-max algorithm." 
[eval-fn tree depth]
(letfn [(minimize ^long [tree d] (if (zero? d) (eval-fn (:root tree) (:direction tree))
                            (r/reduce my-min 
                                  (r/map #(maximize (:tree %) (dec d)) (:children tree)))))
        (maximize ^long [tree d] (if(zero? d) (eval-fn (:root tree) (:direction tree))
                            (r/reduce my-max   
                                   (r/map #(minimize (:tree %) (dec d)) (:children tree)))))] 
(minimize tree depth)))    
    

(defn evaluator
  [eval-fn depth]
  (fn [t]
    (search eval-fn t depth)))
 
(definline fscore [b dir]
`(rand-int  10))

(defn flevel [b dir]
(repeat 30 (Move->Board. 'm1 b)))            
                
(defn next-level [b ^long dir] 
 (r/map #(Move->Board. % (core/try-move %)) (core/team-moves b dir)))

(defn fake [^long dir b ^long d]
(r/fold (/ (.. Runtime getRuntime availableProcessors) 2) best best ;2 = best so far
 (r/map #(Move-Value. (:move %) (search score-naive (:tree %) (dec d))) ;starting from children so decrement depth
                       (into [] (:children (game-tree dir b next-level))))))
                         
#_(defn fake2 [dir b d] 
(r/fold best  (into [] (:children (game-tree dir b next-level)))))

(defn score-by-count  [b dir] 
(let [ hm (into [] (core/gather-team b dir))
       aw (into [] (core/gather-team b (unchecked-negate dir)))]
 (unchecked-subtract (count hm) 
                     (count aw))))      


 (defn score-naive ^long [b dir]
 (do (swap! mmm inc)
 (let [hm (core/gather-team b dir) ;fixed bug
       aw (core/gather-team b (unchecked-negate dir))]
 (unchecked-subtract (r/reduce + (r/map :value hm)) 
                     (r/reduce + (r/map :value aw)))))   )


                
