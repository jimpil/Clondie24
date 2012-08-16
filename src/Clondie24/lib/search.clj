(ns Clondie24.lib.search 
      (:require [Clondie24.lib.core :as core]))
      
(def curr-game "Before any searching we need a game-map." (atom nil))

(defn game-tree
  "Generate the entire game tree lazily. For games like chess you MUST consume only a portion of it."
  [root? dir board successors-fn depth]
  (let [mapper (if root? 'pmap 'map)]
  {:node board
   :direction dir
   :children   (if (neg? depth) '()
               ((eval mapper) #(game-tree false %2 %1 successors-fn (dec depth)) 
               (successors-fn board dir) (cycle [(- dir) dir])))}))

    
(defn search [eval-fn tree]
(letfn [(minimize [tree]  (if (seq (:children tree))
                                (apply min
                                  (map #(maximize %) (:children tree)))
                            (eval-fn (:node tree) (:dir tree))))

        (maximize [tree]  (if (seq (:children tree))
                                (apply max
                                  (map #(minimize %) (:children tree)))
                            (eval-fn (:node tree) (:dir tree))))] 
(minimize tree)))    
    

(defn evaluator
  "Dynamic evaluation of a game tree. Returns a number representing how good
  the root position is. Uses minimax algorithm"
  [eval-fn]
  (fn [t]
    (search eval-fn t)))

(defn best-next-state
  "Get the best computer move for the given game tree.
  static-evaluator evaluates single positions, without looking at the tree, and
  returning a number"
  [tree static-evaluator]
  (->> (:children tree)
       (max-key (evaluator static-evaluator))
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
 (- (count hm) (count aw))))      
 
 
 (defn score-naive [b dir] 
 (let [hm (filter #(= dir (:direction %)) b)
       aw (filter #(not= dir (:direction %)) b)]
 (- (reduce + (remove nil? (map #(:value %) hm))) 
    (reduce + (remove nil? (map #(:value %) aw))))))
    
(defn start-search [dir root-n depth]
(let [b-state (-> (game-tree true dir root-n next-level depth)
                  (best-next-state score-by-count))
      team    (filter #(= dir (:direction %)) root-n)
      team-moves (concat (for [piece *1 
                               coords (core/getMoves piece)] 
                         (core/dest->Move curr-game piece coords)))]
(some #(when (= b-state (core/try-move %)) %) team-moves))) 
;the best move is the one that results to the best next state as returned by the recursion   

;(reset! search/curr-game details) 
    
  ;(-> (game-tree true 1 b next-level 8) 
  ;    (best-next-state score-naive))
                
