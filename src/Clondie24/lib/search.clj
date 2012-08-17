(ns Clondie24.lib.search 
      (:require [Clondie24.lib.core :as core]))
      
(def curr-game "Before any searching we need a game-map." (promise))

(defn game-tree
  "Generate the entire game tree lazily."
  [root? dir board successors-fn depth]
  {:node board
   :direction dir
   :children   (if (zero? depth) '()
  ((if root? pmap map) #(game-tree false (- dir) % successors-fn (dec depth)) 
                        (successors-fn board dir)))})

    
(defn search [eval-fn tree]
(letfn [(minimize [tree]  (if (seq (:children tree))
                                (apply min
                                  (map #(maximize %) (:children tree)))
                           (eval-fn (:node tree) (:dir tree))))

        (maximize [tree] (if (seq (:children tree))
                                 (apply max
                                  (map #(minimize %) (:children tree)))
                            (eval-fn (:node tree) (:dir tree))))] 
(trampoline minimize tree)))    
    

(defn evaluator
  "Dynamic evaluation of a game tree. Returns a number representing how good
  the root position is. Uses minimax algorithm"
  [eval-fn]
  (fn [t]
    (search eval-fn t)))

(defn best-next-state
  "Get the best next state for the given game tree.
  static-evaluator evaluates single positions, without looking at the tree, and
  returning a number."
  [tree static-evaluator]
  (->> (:children tree)
       (apply max-key (evaluator static-evaluator))
       (:node)))
              
                
(defn next-level [b dir] 
(let [team (filter #(= dir (:direction %)) b) ;all the team-mates (with same direction)
      team-moves (concat (for [piece team 
                               coords (core/getMoves piece)] 
                           (core/dest->Move @curr-game piece coords)))]
 (vec (map core/try-move team-moves))))


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
    
(defn start-search [dir root-n depth]
(let [b-state (-> (game-tree true dir root-n next-level depth)
                  (best-next-state score-by-count ))
      team    (filter #(= dir (:direction %)) root-n)
      team-moves (concat (for [piece team 
                               coords (core/getMoves piece)] 
                         (core/dest->Move @curr-game piece coords)))]
(some #(when (= b-state (core/try-move %)) %) team-moves))) 
;the best move is the one that results to the best next state as returned by the recursion   

;(reset! search/curr-game details) 
    
  ;(-> (game-tree true 1 b next-level 8) 
  ;    (best-next-state score-naive))
                
