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

(defn best-ttt 
([]    nil)
([best next] (max-key :value best next)))
    
(defn best 
([]   nil)
([best next] 
  (if (pos? (compare (:value best) (:value next))) 
    best next)))   
	                  
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

    
(defn minmax "The recursion of the min-max algorithm." 
[eval-fn tree] 
(letfn [(minimize  [tree d] (if (zero? d) (eval-fn (:root tree) (:direction tree))
                            (r/reduce my-min 
                                  (r/map (fn [child] (maximize (:tree child) (dec d))) (:children tree)))))
        (maximize  [tree d] (if (zero? d) (eval-fn (:root tree) (:direction tree))
                            (r/reduce my-max   
                                   (r/map (fn [child] (minimize (:tree child) (dec d))) (:children tree)))))] 
(minimize tree (-> (:pref-depth @curr-game)
                   dec int))))

(def minmax-TT (memoize #(minmax (:scorer @curr-game) %)))




(defn alpha-beta "The recursion of the min-max algorithm with pruning." 
[eval-fn tree] 
(letfn [(minimize  [tree d a b] (if (zero? d) (swap! b my-max (eval-fn (:root tree) (:direction tree))) 
                                 (swap! a my-min 
                                    (r/reduce my-min 
                                       (r/map (fn [child] (if-not (> @a @b) (do (println {:depth d :A @a :Β @b}) nil)
  							   (maximize (:tree child) (dec d) a b))) (:children tree))))))
        (maximize  [tree d a b] (if (zero? d) (swap! a my-min (eval-fn (:root tree) (:direction tree)))
                                 (swap! b my-max                       
                                    (r/reduce my-max   
                                       (r/map (fn [child] (if-not (> @a @b) (do (println {:depth d :A @a :Β @b})  nil)
                                                        (minimize (:tree child) (dec d) a b))) (:children tree))))))] 
(minimize tree (dec (:pref-depth @curr-game)) (atom 1000) (atom -1000))))

(def alpha-beta-TT (memoize alpha-beta))

(defn search* [eval-fn tree pruning?]
  (if pruning? (alpha-beta eval-fn tree)
               (minmax eval-fn tree)))
    

#_(defn evaluator
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
 (r/map #(Move-Value. (:move %) (search* (:scorer @curr-game) (:tree %) pruning?)) ;starting from children so decrement depth
                    successors )))))
;;---------------------------------------------------------------------------------------------------------                         
;--------------------------------LAZY-VERSION-------------------------------------------------------------- 


(defn game-tree-lazy "Generate the game tree lazily."
  [dir board successors-fn] 
  (Tree. board dir 
     (map #(Move->Tree. (:move %) 
                          (game-tree-lazy (- dir) (:board %) successors-fn)) 
             (successors-fn board dir))))

    
(defn minmax-lazy "The recursion of the min-max algorithm." 
[eval-fn tree] 
(letfn [(minimize  [tree d] (if (zero? d) (eval-fn (:root tree) (:direction tree))
                            (reduce my-min 
                                  (map (fn [child] (maximize (:tree child) (dec d))) (:children tree)))))
        (maximize  [tree d] (if (zero? d) (eval-fn (:root tree) (:direction tree))
                            (reduce my-max   
                                   (map (fn [child] (minimize (:tree child) (dec d))) (:children tree)))))] 
(minimize tree (-> (:pref-depth @curr-game)
                   dec int))))

(def minmax-TT-lazy (memoize #(minmax (:scorer @curr-game) %)))


(defn alpha-beta-lazy "The recursion of the min-max algorithm with pruning." 
[eval-fn tree] 
(letfn [(minimize  [tree d a b] (if (zero? d) (eval-fn (:root tree) (:direction tree)) 
                               (swap! a my-min 
                                    (reduce my-min 
                                       (map (fn [child] (if-not (> @a @b) @b #_(println {:depth d :A @a :Β @b})
                                                            (maximize (:tree child) (dec d) a b))) (:children tree))))))
        (maximize  [tree d a b] (if (zero? d) (eval-fn (:root tree) (:direction tree))
                               (swap! b  my-max                           
                                  (reduce my-max   
                                      (map (fn [child] (if-not (> @a @b)  @a #_(println {:depth d :A @a :Β @b})
                                                           (minimize (:tree child) (dec d) a b))) (:children tree))))))] 
(minimize tree (dec (:pref-depth @curr-game)) (atom 1000) (atom -1000))))

(def alpha-beta-TT-lazy (memoize alpha-beta))
    

#_(defn evaluator
  [eval-fn depth]
  (fn [t]
    (search eval-fn t depth)))

(defn minmax-ttt-lazy "The recursion of the min-max algorithm (for tic-tac-toe)." 
[eval-fn tree-parent] 
(letfn [(minimize  [tree] (if (empty? (:children tree)) (eval-fn (:root tree)  (- (:direction tree-parent)))
                            (apply min 
                                  (map (fn [child] (maximize (:tree child) ))  (:children tree)))))
        (maximize  [tree] (if (empty?  (:children tree)) (eval-fn (:root tree)  (- (:direction tree-parent)))
                            (apply max   
                                   (map (fn [child] (minimize (:tree child) )) (:children tree)))))] 
(minimize tree-parent)))
           
                
(defn next-level-lazy [b ^long dir] 
 (map #(Move->Board. % (core/try-move %)) 
   ((:team-moves @curr-game) b dir nil)))

(defn go-lazy [^long dir b]
(let [successors  (:children (game-tree-lazy dir b next-level-lazy))]
  (case  (count successors)
      1  (first successors)   ;;mandatory move detected - just return it  
      0 nil
(apply max-key :value 
 (map #(Move-Value. (:move %) (minmax-ttt-lazy (:scorer @curr-game) (:tree %))) 
                    successors )))))

