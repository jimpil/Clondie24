(ns Clondie24.lib.search 
      (:require [Clondie24.lib.core :as core]
                [clojure.core.reducers :as r]))

(set! *unchecked-math* true) 
(def cpus-no (.. Runtime getRuntime availableProcessors)) ;may need it
  
(defn set-parallelism! [x] 
 (alter-var-root #'r/pool 
  (constantly (java.util.concurrent.ForkJoinPool. (int x)))))             
      
(def curr-game "Before any searching we need a game-map." (promise))
(declare score-by-count score-naive next-level)

(defrecord Tree [root direction children])
(defrecord Move->Tree  [move tree])
(defrecord Move->Board [move board])
(defrecord Move-Value  [move value])

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

    
(defn search "The recursion of the min-max algorithm." 
[eval-fn tree depth]
(letfn [(minimize  [tree d] (if (zero? d) (eval-fn (:root tree) (:direction tree))
                            (r/reduce my-min 
                                  (r/map (fn [child] (maximize (:tree child) (dec d))) (:children tree)))))
        (maximize  [tree d] (if (zero? d) (eval-fn (:root tree) (:direction tree))
                            (r/reduce my-max   
                                   (r/map (fn [child] (minimize (:tree child) (dec d))) (:children tree)))))] 
(minimize tree (int depth))))    
    

(defn evaluator
  [eval-fn depth]
  (fn [t]
    (search eval-fn t depth)))
           
                
(defn next-level [b ^long dir] 
 (r/map #(Move->Board. % (core/try-move %)) 
   ((:team-moves @curr-game) b dir false))) ;performance cheating again!

(defn go [^long dir b ^long d scorer]
(let [successors (into [] (:children (game-tree dir b next-level)))]
  (if (= 1 (count successors)) (first successors)   ;;mandatory move detected - just return it  
(r/fold (:chunking @curr-game) best best ;2 = best so far
 (r/map #(Move-Value. (:move %) (search scorer (:tree %) (dec d))) ;starting from children so decrement depth
                    successors )))))
                         
                
