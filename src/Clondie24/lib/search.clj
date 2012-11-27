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
([x y] (if (nil? x) y
         (if (nil? y) x
           (max x y))))
([]  Integer/MIN_VALUE))
   
(defn my-min
([x y] (if (nil? x) y
         (if (nil? y) x
           (min x y))))
([]  Integer/MAX_VALUE)) 



 
(defn game-tree "Generate the game tree."
  [dir board successors-fn] 
  (Tree. board dir 
     (r/map #(Move->Tree. (:move %) 
                          (game-tree (- dir) (:board %) successors-fn)) 
             (successors-fn board dir))))

    
(defn search-exhaustive "The recursion of the min-max algorithm." 
[eval-fn tree] 
(letfn [(minimize  [tree d] (if (zero? d) (eval-fn (:root tree) (:direction tree))
                            (r/reduce my-min 
                                  (r/map (fn [child] (maximize (:tree child) (dec d))) (:children tree)))))
        (maximize  [tree d] (if (zero? d) (eval-fn (:root tree) (:direction tree))
                            (r/reduce my-max   
                                   (r/map (fn [child] (minimize (:tree child) (dec d))) (:children tree)))))] 
(minimize tree (-> (:pref-depth @curr-game)
                   dec int))))

(def search-mem (memoize #(search-exhaustive (:scorer @curr-game) %)))


(defn search-ab "The recursion of the min-max algorithm with pruning." 
[eval-fn tree] 
(letfn [(minimize  [tree d a b] (if (zero? d) (do (println {:depth d :A @a :Β @b}) (eval-fn (:root tree) (:direction tree))) 
                               (swap! a my-min 
                                    (r/reduce my-min 
                                       (r/map (fn [child] (if (<= @a @b)  (println {:depth d :A @a :Β @b})
                                                            (maximize (:tree child) (dec d) a b))) (:children tree))))))
        (maximize  [tree d a b] (if (zero? d) (do (println {:depth d :A @a :Β @b}) (eval-fn (:root tree) (:direction tree)))
                               (swap! b  my-max                           
                                  (r/reduce my-max   
                                      (r/map (fn [child] (if (<= @a @b)  (println {:depth d :A @a :Β @b})
                                                           (minimize (:tree child) (dec d) a b))) (:children tree))))))] 
(minimize tree (dec (:pref-depth @curr-game)) (atom 1000) (atom -1000))))

;(def search-ab-mem (memoize search-ab))

(definline search [eval-fn tree pruning?]
  `(if ~pruning? (search-ab ~eval-fn ~tree)
                (search-exhaustive ~eval-fn ~tree)))
    

(defn evaluator
  [eval-fn depth]
  (fn [t]
    (search eval-fn t depth)))
           
                
(defn next-level [b ^long dir] 
 (r/map #(Move->Board. % (core/try-move %)) 
   ((:team-moves @curr-game) b dir false))) ;performance cheating again!

(defn go [^long dir b pruning?]
(let [successors (into [] (:children (game-tree dir b next-level)))]
  (if (= 1 (count successors)) (first successors)   ;;mandatory move detected - just return it  
(r/fold (:chunking @curr-game) best best ;2 = best so far
 (r/map #(Move-Value. (:move %) (search (:scorer @curr-game) (:tree %) pruning?)) ;starting from children so decrement depth
                    successors )))))

                         
                
