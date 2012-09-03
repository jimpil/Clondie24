(ns Clondie24.lib.search 
      (:require [Clondie24.lib.core :as core]
                [clojure.core.reducers :as r]))

(set! *unchecked-math* true) 
(def cpus-no (.. Runtime getRuntime availableProcessors)) ;may need it               
      
(def curr-game "Before any searching we need a game-map." (promise))
(declare score-by-count score-naive next-level)

(defrecord Tree [root direction children])
(defrecord Move->Tree  [move tree])
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
 (r/map #(Move->Board. % (core/try-move %)) 
   ((:team-moves @curr-game) b dir (:mover @curr-game) false))) ;performance cheating again!

(defn go [^long dir b ^long d]
(let [successors (into [] (:children (game-tree dir b next-level)))]
  (if (= 1 (count successors)) (first successors)     
(r/fold (:chunking @curr-game) best best ;2 = best so far
 (r/map #(Move-Value. (:move %) (search (:scorer @curr-game) (:tree %) (dec d))) ;starting from children so decrement depth
                    successors )))))
                         
                
