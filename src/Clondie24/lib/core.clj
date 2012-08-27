(ns Clondie24.lib.core
   (:require [Clondie24.lib.util :as ut]
             [clojure.core.reducers :as r :only [map filter]]))
;----------------------------------------<SOURCE>--------------------------------------------------------------------
;----------------------------------------<CODE>----------------------------------------------------------------------   

(def board-history 
"Log of the state of a game." 
(atom []))

(defn log-board 
"The logging function for the board ref. Will conj every new board-state into a vector." 
[dest k r old n] 
 (when (not= n (peek @dest))  
  (swap! dest conj n)))

(defprotocol Piece "The Piece abstraction."
 (update-position [this new-position])
 (mutate-position [this new-position]) ;;optional in case you need mutation
 (getGridPosition [this])
 (getListPosition [this])
 (getPoint [this]) ;for gui?
 (die [this])
 (promote [this])
 (getMoves [this b])) 
 
 (defprotocol Movable 
 "The Command design pattern in action (allows us to do/undo moves)."
 (try-move [this]) 
 (undo [this]) ;;optional in case you need mutation
 (getOrigin [this]))
 
(defn translate
"Translates a position from 1d to 2d and vice-versa. 
Mappings should be either 'checkers-board-mappings' or 'chess-board-mappings'." 
([^long i mappings] ;{:post [(not (nil? %))]}   
  (let [grid-loc (get mappings i)] ;will translate from 1d to 2d
    (if-not (nil? grid-loc) grid-loc ;not found 
    (throw (IllegalStateException. (str "NOT a valid list-location:" i))))))
([x y ^clojure.lang.PersistentVector mappings] ;{:post [(not (== % -1))]} 
  (let [list-loc (.indexOf mappings [x y])] ;will translate from 2d to 1d
    (if-not (= list-loc -1) list-loc ;not found
     (throw (IllegalStateException. (str "NOT a valid grid-location: [" x ", " y "]")))))))

(def translate-position (memoize translate))
                    
(defn make-piece 
"Helper function for creating pieces. A piece is simply a record with 5 keys. Better not use this directly!"
 [game c pos rank direction] ;&{:keys [rank direction ]
               ;:or {rank 'zombie direction 1 r-value 1}}]
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
  
 
(definline alive? [p]
`(:alive (meta ~p)))

(defn empty-board 
"Returns an empty board for the game provided - all nils." 
[game] 
(vec (repeat (:board-size game) nil)))

(definline populate-board 
"Builds a new board with nils where dead pieces." 
[board]     
`(into [] (r/map #(if (alive? %) % nil) ~board)))
               

(defn move 
"The function responsible for moving Pieces. Each piece knows how to move itself. Returns the resulting board without making any state changes. " 
[board p coords] 
;{:pre [(satisfies? Piece p)]}  ;safety comes first
;(if  (some #{coords} (:mappings game-map)) ;check that the position exists on the grid
(let [newPiece (update-position p coords) ;the new piece as a result of moving 
      old-pos  (getListPosition p)
      new-pos  (getListPosition newPiece)]            
(-> board 
     (transient)  
     (assoc! old-pos nil) 
     (assoc! new-pos newPiece)
     (persistent!)
     #_(populate-board))) ;replace dead-pieces with nils
#_(throw (IllegalStateException. (str coords " is NOT a valid position according to the mappings provided!"))))

(defn amove 
"Same as 'move' but fast (expects a mutable array for board). Returns the mutated board." 
[board p coords]
(let [old-pos  (getListPosition p)
      mutPiece (update-position p coords) ;the mutated piece 
      new-pos  (getListPosition mutPiece)] 
(do
     (aset  board old-pos nil) ;^"[LClondie24.games.chess.ChessPiece2;"
     (aset  board new-pos mutPiece) board)))

(defrecord Move [p mover ^clojure.lang.PersistentVector end-pos]
 Movable
 (try-move [this] (mover p end-pos))
 (getOrigin [this] (:position p))
 Object
 (toString [this] 
   (println "Move from:" (getOrigin this) "to:" end-pos)))   

(definline dest->Move "Constructor for creating moves." 
[b p dest]  `(Move. ~p #(move ~b %1 %2) ~dest))

(defn execute! [^Move m batom]
 (reset! batom (try-move m)))
 
(definline threatens? "Returns true if p2 is threatened by p1 on board b." 
[p2 p1 b]
`(if (some #{(:position ~p2)} (getMoves ~p1 ~b)) true false)) 

(defn undo! []
(swap! board-history (comp vec butlast)))     
      
;(for [letter "ABCDEFGH" ;strings are seqable
;     number (range 1 9)]
;(format "%c%d" letter number)))

(defn clear-history! []
 (swap! board-history empty)) 

(definline gather-team "Returns all the pieces with same direction dir on this board b." 
[b dir]
`(r/filter #(= ~dir (:direction %)) ~b)) ;all the team-mates (with same direction)
 
(definline team-moves 
[b dir]
`(let [team# (gather-team ~b ~dir) 
       tmvs# (r/mapcat (fn [p#] (r/map #(dest->Move ~b p# %) (getMoves p# ~b))) team#)]
 tmvs# ))

(defn vacant? 
 "Checks if a position [x, y] is vacant on the given board and mappings." 
  [m b pos]
 (let [[x y] pos]
 (nil? 
  (get b (translate-position x y m)))))  
 
(definline bury-dead [c]
 `(filter alive? ~c))  

(definline collides? 
"Returns true if the move from sp to ep collides with any friendly pieces. The move will be walked step by step by the walker fn."
[sp ep walker b m dir]
`(loop [imm-p# (if (nil? ~walker) ~ep (~walker ~sp))] ;if walker is nil make one big step to the end       
(cond  
  (= ~ep imm-p#) ;if reached destination 
       (if (not= ~dir (:direction (get ~b (translate-position (first ~ep) (second ~ep) ~m)))) false true)    
  (not (nil? (get ~b (translate-position (first imm-p#) (second imm-p#) ~m)))) true
:else (recur (~walker imm-p#)))))

(defn acollides? "Same as 'collides?' but deals with an array as b - not a vector."
[[sx sy] [ex ey] walker b m dir]
(loop [[imm-x imm-y] (if (nil? walker) [ex ey] (walker [sx sy]))] ;if walker is nil make one big step to the end       
(cond  
  (= [ex ey] [imm-x imm-y]) ;if reached destination 
       (if (not= dir (:direction (aget b (translate-position ex ey m)))) false true)    
  (not (nil? (aget  b (translate-position imm-x imm-y m)))) true
:else (recur (walker [imm-x imm-y])))))

(defn exposes-king? [move b]
(let [next-b (try-move move)  
     [def-king] (into [] (r/filter #(and (= 'king (:rank %)) 
                                         (= (:direction %) (get-in move [:p :direction]))) next-b)) ;the king we're defending
     opp-dir (unchecked-negate (get-in move [:p :direction])) ;negate direction of moving piece to get opponent
     opp-pieces (gather-team b opp-dir)]
(every? #(threatens? def-king % b) (into [] opp-pieces))))
                 

  
