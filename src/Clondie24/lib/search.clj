(ns Clondie24.lib.search 
      (:require [Clondie24.lib.core :as core]
                [clojure.core.reducers :as r]))
      
(def curr-game "Before any searching we need a game-map." (promise))
(declare score-by-count next-level my-max)

(defrecord Tree [root direction children])
(defrecord Move->Tree [move tree])
(defrecord Move->Board [move board])
(defrecord Move-Value  [move value])

(defn best 
([] Integer/MIN_VALUE)
([best next] 
  (if (pos? (compare (:value best) (:value next))) 
    best next)))  ;(:move best) (:move rest)
	                  
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
(letfn [(minimize [tree d] (if (zero? d) (eval-fn (:root tree) (:direction tree))
                            (r/reduce my-min      ;(apply min
                                  (r/map #(maximize (:tree %) (dec d)) (:children tree)))))
        (maximize [tree d] (if(zero? d) (eval-fn (:root tree) (:direction tree))
                            (r/reduce my-max   ; (apply max
                                   (r/map #(minimize (:tree %) (dec d)) (:children tree)))))] 
(minimize tree depth)))    
    

(defn evaluator
  [eval-fn depth]
  (fn [t]
    (search eval-fn t depth)))
            
                
(defn next-level [b dir] 
  (r/map #(Move->Board. % (core/try-move %)) 
    (core/team-moves @curr-game b dir)))

(defn fake [dir b d]
(r/fold 1 best best
 (r/map #(Move-Value. (:move %) (search score-by-count (:tree %) d))
                       (into [] (:children (game-tree dir b next-level))))))
                         
#_(defn fake2 [dir b d] 
(r/fold best  (into [] (:children (game-tree dir b next-level)))))

(defn score-by-count [b dir] 
(let [hm (filter #(= dir (:direction %)) b)
      aw (filter #(not= dir (:direction %)) b)]
 (- (count hm) 
    (count (remove nil? aw)))))      
 
 
 (defn score-naive [b dir]
 (let [hm (filter #(= dir (:direction %)) b)
       aw (remove nil? (filter #(not= dir (:direction %)) b))]
 (- (apply + (map #(:value %) hm)) 
    (apply + (map #(:value %) aw)))))   


                
