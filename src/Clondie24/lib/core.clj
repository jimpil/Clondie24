(ns Clondie24.lib.core
   (:require [Clondie24.lib.util :as ut]
             [clojure.core.reducers :as r :only [map filter]]))
;----------------------------------------<SOURCE>--------------------------------------------------------------------
;----------------------------------------<CODE>----------------------------------------------------------------------   
(set! *unchecked-math* true)

(def ^:const mappings-8x8
"A vector of vectors. Outer vector represents the 64 (serial) positions chess-items can position themselves on. 
 Each inner vector represents the coordinates of that position on the 8x8 grid."
[[0 0] [1 0] [2 0] [3 0] [4 0] [5 0] [6 0] [7 0]
 [0 1] [1 1] [2 1] [3 1] [4 1] [5 1] [6 1] [7 1]
 [0 2] [1 2] [2 2] [3 2] [4 2] [5 2] [6 2] [7 2]
 [0 3] [1 3] [2 3] [3 3] [4 3] [5 3] [6 3] [7 3]
 [0 4] [1 4] [2 4] [3 4] [4 4] [5 4] [6 4] [7 4]
 [0 5] [1 5] [2 5] [3 5] [4 5] [5 5] [6 5] [7 5]
 [0 6] [1 6] [2 6] [3 6] [4 6] [5 6] [6 6] [7 6]
 [0 7] [1 7] [2 7] [3 7] [4 7] [5 7] [6 7] [7 7]])
 
 (def ^:const mappings-3x3
"A vector of vectors. Outer vector represents the 9 (serial) positions tic-tac-toe-items can position themselves on. 
 Each inner vector represents the coordinates of that position on the 3x3 grid."
 [[0 0] [1 0] [2 0]
  [0 1] [1 1] [2 1]
  [0 2] [1 2] [2 2]])

(def board-history 
"Log of the state of a game." 
(atom []))

(defn log-board 
"The logging function for the board ref. Will conj every new board-state into a vector." 
[dest k r old n] 
 (when-not (= n old)  
  (swap! dest conj n)))

(defprotocol Piece "The Piece abstraction."
 (update-position [this new-position])
 (mutate-position [this new-position]) ;;optional in case you need mutation
 (getGridPosition [this])
 (getListPosition [this])
 (getPoint [this]) ;for gui?
 (die [this])
 (promote [this np])
 (getMoves [this board with-precious-piece?])) 
 
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
    (if-not (nil? grid-loc)  ;not found 
      grid-loc 
    (throw (IllegalStateException. (str "NOT a valid list-location:" i))))))
([x y ^clojure.lang.PersistentVector mappings] ;{:post [(not= % -1)]} 
  (let [list-loc (.indexOf mappings [x y])] ;will translate from 2d to 1d
    (if-not (= list-loc -1) ;not found 
      list-loc 
     (throw (IllegalStateException. (str "NOT a valid grid-location: [" x ", " y "]")))))))

(def translate-position (memoize translate))
                    
(defn piece 
"Helper function for creating pieces. A piece is simply a record with 5 keys. Better not use this directly (slow)!"
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

(defn game-over? [referee & args]
(apply referee args))

(defn empty-board 
"Returns an empty board for the game provided - all nils." 
[game] 
(vec (repeat (:board-size game) nil)))

(definline populate-board 
"Builds a new board with nils where dead pieces." 
[board]     
`(into [] (r/map #(if (alive? %) % nil) ~board)))         

(defn move 
"The function responsible for moving Pieces. Each piece knows where it can move. 
 Returns the resulting board without making any state changes. " 
[board p coords] 
;{:pre [(satisfies? Piece p)]}  ;safety comes first
;(if  (some #{coords} (:mappings game-map)) ;check that the position exists on the grid
(let [newPiece (update-position p coords) ;the new piece as a result of moving 
      old-pos  (getListPosition p)
      new-pos  (getListPosition newPiece)]            
(-> board 
     ;(transient)  
     (assoc old-pos nil 
            new-pos newPiece) 
     ;(assoc! )
     ;(persistent!)
     #_(populate-board)))) ;replace dead-pieces with nils
#_(throw (IllegalStateException. (str coords " is NOT a valid position according to the mappings provided!")))

(defn amove 
"Same as 'move' but fast (expects a mutable array for board). Returns the mutated board." 
[board p coords]
(let [old-pos  (getListPosition p)
      mutPiece (update-position p coords) ;the mutated piece 
      new-pos  (getListPosition mutPiece)] 
     (aset  board old-pos nil) ;^"[LClondie24.games.chess.ChessPiece2;"
     (aset  board new-pos mutPiece) board))

(defrecord Move [p mover ^clojure.lang.PersistentVector end-pos]
 Movable
 (try-move [this]  (with-meta (mover p end-pos) {:caused-by this})) ;;the board returned was caused by this move
 (getOrigin [this] (:position p))
 Object
 (toString [this] 
   (println "#Move {:from" (:position p) ":to" end-pos)))   

(defn dest->Move 
 "Constructor for creating moves from destinations. 
 It wouldn't make sense to pass more than 1 mover-fns." 
[b p dest mover]  (if (nil? mover) (Move. p (partial move b) dest)
                                   (Move. p (partial mover b) dest)))

(defn execute! [^Move m batom]
 (reset! batom (try-move m)))
 
(definline threatens? "Returns true if p2 is threatened by p1 on board b. This is the only time that we call getMoves with nil." 
[p2 p1 b]
`(some #{(:position ~p2)} (getMoves ~p1 ~b false)))

(defn undo! []
(try (swap! board-history pop)
(catch Exception e @board-history))) ;popping an empty stack throws an error (just return the empty one)    
     

(defn clear-history! []
 (swap! board-history empty)) 

(definline gather-team "Filters all the pieces with same direction dir on this board b. Returns a reducer." 
[b dir]
`(r/filter #(= ~dir (:direction %)) ~b)) ;all the team-mates (with same direction)

 
(definline team-moves "Filters all the moves for the team with direction 'dir' on this board b. Returns a reducer." 
[b dir exposes-check?]
`(let [team# (gather-team ~b ~dir) 
       tmvs# (r/mapcat 
                  (fn [p#] 
                    ;(r/map #(dest->Move ~b p# % ~mover) 
                            (getMoves p# ~b ~exposes-check?)) team#)]
 tmvs# ))


(defn vacant? 
 "Checks if a position [x, y] is vacant on the given board and mappings." 
  [m b pos]
 (let [[x y] pos]
 (nil? 
  (get b (translate-position x y m))))) 
  
(def occupied? (complement vacant?))   
 
(definline bury-dead [c]
 `(filter alive? ~c))  

(definline collides? 
"Returns true if the move collides with any friendly pieces. 
 The move will be walked step by step by the walker fn."
[move walker b m] ;last 2 can be false, nil
`(let [[epx# epy# :as ep#]  (:end-pos ~move)
       dir# (get-in ~move [:p :direction])]                                         
(loop [[imm-px# imm-py# :as imm-p#] (if (nil? ~walker) ep# (~walker (getOrigin ~move)))] ;if walker is nil make one big step to the end       
(cond  
  (=  imm-p# ep#) ;if reached destination there is potential for attack
       (if-not (= dir# (:direction (get ~b (translate-position epx# epy# ~m)))) false true)    
  (not (nil? (get ~b (translate-position imm-px# imm-py# ~m)))) true
:else (recur (~walker imm-p#))))))

(defn acollides? "Same as 'collides?' but deals with an array as b - not a vector."
[[sx sy] [ex ey] walker b m dir]
(loop [[imm-x imm-y] (if (nil? walker) [ex ey] (walker [sx sy]))] ;if walker is nil make one big step to the end       
(cond  
  (= [ex ey] [imm-x imm-y]) ;if reached destination 
       (if (not= dir (:direction (aget b (translate-position ex ey m)))) false true)    
  (not (nil? (aget  b (translate-position imm-x imm-y m)))) true
:else (recur (walker [imm-x imm-y])))))

(definline exposes? [move precious]
`(if-not ~precious false ;skip everything
   (let [next-b# (try-move ~move)
         dir#    (get-in ~move [:p :direction])
         def-prec#  (some #(when (and (= ~precious (:rank %)) 
                                       (= dir# (:direction %))) %) next-b#)]
   (some #(threatens? def-prec# % next-b#) 
     (into [] (gather-team next-b# (- dir#))))))) 


(defn score-chess-naive ^double [b dir]
 (let [hm (gather-team b dir) ;fixed bug
       aw (gather-team b (- dir))]
 (- (r/reduce + (r/map :value hm)) 
    (r/reduce + (r/map :value aw)))))    

(definline remove-illegal [pred ms]
`(into [] (r/remove ~pred ~ms)))
                 

  
