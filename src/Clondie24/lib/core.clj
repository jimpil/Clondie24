(ns Clondie24.lib.core
   (:require [Clondie24.lib.util :as ut]))
;----------------------------------------<SOURCE>--------------------------------------------------------------------
;----------------------------------------<CODE>----------------------------------------------------------------------   

(def board-history 
"Log of the state of a game." 
(atom []))

(defn log-board 
"The logging function for the board ref. Will conj every new board-state into a vector." 
[dest k s old n] 
(when (not= old n) 
  (swap! dest conj n)))

(defprotocol Piece "The Piece abstraction."
 (update-position [this new-position])
 (getGridPosition [this])
 (getListPosition [this])
 (getPoint [this])
 (die [this])
 (promote [this])
 (getMoves [this]) ;pretends there is only this piece on the board - will need filtering for validity later
) 
 
 (defprotocol Movable 
 "The Command design pattern in action (allows us to do/undo moves)."
 (try-move [this]) 
 (getOrigin [this]))
 
(defn translate-position
"Translates a position from 1d to 2d and vice-versa. 
Mappings should be either 'checkers-board-mappings' or 'chess-board-mappings'." 
([i mappings] ;{:post [(not (nil? %))]}   
  (let [grid-loc (get mappings i)] ;will translate from 1d to 2d
    (if-not (nil? grid-loc) grid-loc ;not found 
    (throw (IllegalStateException. (str "NOT a valid list-location:" i))))))
([x y mappings] ;{:post [(not (== % -1))]} 
  (let [list-loc (.indexOf mappings (vector x y))] ;will translate from 2d to 1d
    (if-not (= list-loc -1) list-loc ;not found
    (throw (IllegalStateException. (str "NOT a valid grid-location: [" x ", " y "]")))))))

                    
(defn make-piece 
"The central function for creating pieces. A piece is simply a record with 4 keys."
 [game c pos &{:keys [rank direction]
               :or {rank 'zombie direction 1}}]
 ((ut/record-factory-aux (:record-name game)) c pos rank 
  ((keyword rank) (:rel-values game)) direction  
  {:alive true}   ;pieces are born 'alive'             
           nil))  ;no extra fields                          
                               
(defn starting-board [game] 
"Returns the initial board for a game with pieces on correct starting positions for the particular game."
(let [p1 (:north-player-start game)
      p2 (:south-player-start game)
      vacant (- (:board-size game) 
                (:total-pieces game))]
(vec (flatten
     (conj p2 (conj (repeat vacant nil) p1))))))
  
 
(defn alive? [p]
(:alive (meta p)))

(defn empty-board 
"Returns an empty board for the game provided - all nils." 
[game] 
(vec (repeat (:board-size game) nil)))

(defn populate-board 
"Builds a new board with nils where dead pieces." 
[board]     
(into [] (map #(if (alive? %) % nil) board)))
               

(defn move 
"The function responsible for moving Pieces. Each piece knows how to move itself. Returns the resulting board without making any state changes. " 
[game-map p coords] 
{:pre [(satisfies? Piece p)]}  ;safety comes first
(if  (some #{coords} (:mappings game-map)) ;check that the position exists on the grid
(let [newPiece (update-position p coords) ;the new piece as a result of moving 
      old-pos  (getListPosition p)
      new-pos  (getListPosition newPiece)]            
(-> @(:board-atom game-map)  ;deref the appropriate board atom 
     (assoc old-pos nil) 
     (assoc new-pos newPiece)
     (populate-board))) ;replace dead-pieces with nils
(throw (IllegalStateException. (str coords " is NOT a valid position according to the mappings provided!"))))) 

(defrecord Move [p mover ^clojure.lang.PersistentVector end-pos]
 Movable
 (try-move [this] (mover p end-pos))
 (getOrigin [this] (:position p))
 Object
 (toString [this] 
   (println "Move from:" (getOrigin this) "to:" end-pos)))   

(defn dest->Move "Helper fn for creating moves." 
[dm p dest]  (Move. p (partial move dm) dest))

(defn execute! [^Move m batom]
 (reset! batom (try-move m)))
 
(defn- undo 
"Returns the state which results from undoing this many levels. " 
[levels] 
(let [history @board-history
      x-to-last (- (count history) (if (< 2 levels) (inc levels) 2))]
  (nth history x-to-last)))

(defn undo! [levs]
(swap! board-history #(vec (drop-last %2 %)) levs))      
      
;(for [letter "ABCDEFGH" ;strings are seqable
;     number (range 1 9)]
;(format "%c%d" letter number)))

(defn clear-history! []
 (swap! board-history 
    #(vec (drop (count %) %))))

;EXAMPLEs:
 ;(make-checker    (make-color 'BLUE)  [1 5] :rank 'soldier)
 ;(make-checker    (make-color 'WHITE) [0 0])   ;rank will default to 'zombie
 ;(make-chessItem  (make-image "bishop-icon.png") [2 3] :rank 'bishop)

(defn vacant? 
 "Checks if a position [x, y] is vacant on the given board and mappings." 
  [m b pos]
 (let [[x y] pos]
 (nil? 
  (nth b (translate-position x y m)))))  
 
(defn bury-dead [c]
 (filter alive? c))  

(defn collides?
[[sx sy] [ex ey] walker b m dir]
(loop [[cx cy] [sx sy]
       [imm-x imm-y] (walker [cx cy])]
(cond 
  (and (= ex cx) ;if reached destination there is potential for attack
       (= ey cy)) (if (not= dir (:direction (get b (translate-position ex ey m)))) false true)
  (not (nil? (get b (translate-position imm-x imm-y m)))) true 
:else (recur [imm-x imm-y] (walker [imm-x imm-y])))))                 

  