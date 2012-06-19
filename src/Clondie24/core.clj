(ns Clondie24.core
   (:require [Clondie24.util :as ut]))
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
 
 (defprotocol MoveCommand 
 "The Command design pattern in action (allows us to undo commands)."
 (try-move [this]) 
 (execute [this]) 
 (undo    [this]))
 
 
(defn translate-position
"Translates a position from 1d to 2d and vice-versa. 
Mappings should be either 'checkers-board-mappings' or 'chess-board-mappings'." 
([i mappings] ;{:post [(not (nil? %))]}   
(let [grid-loc (get mappings i)] ;will translate from 1d to 2d
(if-not (nil? grid-loc) grid-loc 
(throw (IllegalStateException. (str "NOT a valid list-location: " i))))))
([x y ^clojure.lang.PersistentVector mappings] ;{:post [(not (== % -1))]} 
(let [list-loc (.indexOf mappings [x y])] ;will translate from 2d to 1d
(if-not (= list-loc -1) list-loc 
(throw (IllegalStateException. (str "NOT a valid grid-location: [" x "," y "]")))))))

                    
(defn make-piece 
"The central function for creating pieces. A piece is simply a record with 4 keys."
 [game c pos &{:keys [rank]
               :or {rank 'zombie}}]
 ((ut/record-factory-aux (:record-name game)) c 
  (ut/vector-of-doubles pos) rank 
  (get (game :rel-values) (keyword rank)) {:alive true} ;pieces are born 'alive'             
                                           nil))        ;no extra fields                          
                               
(defn starting-board [game] 
"Returns the initial board for a game with pieces on correct starting positions."
(let [p1 (:north-player-start game)
      p2 (:south-player-start game)
      vacant (- (:board-size game) (:total-pieces game))]
(vec (flatten
     (conj p2 (conj (repeat vacant nil) p1))))))
  
 
(defn alive? [p]
(:alive (meta p)))

(defn empty-board 
"Returns an empty board for the game provided - all nils." 
[game] 
(repeat (game :board-size) nil))

(defn populate-board 
"Builds a new board with nils where dead pieces were." 
[board]     
(into [] (map #(if (alive? %) % nil) board)))
               

(defn move 
"The function responsible for moving Pieces. Each piece knows how to move itself. Returns the resulting board without making any state changes. " 
[game-map p coords] 
{:pre [(satisfies? Piece p)]}  ;safety comes first
(if  (some #{(ut/vector-of-doubles coords)} (:mappings game-map)) ;check that the position exists on the grid
(let [newPiece (update-position p coords)] ;the new piece as a result of moving
(populate-board              ;replace dead-pieces with nils
(-> @(game-map :board-atom)  ;deref the appropriate board atom 
     (assoc (getListPosition p) nil) 
     (assoc (getListPosition newPiece) newPiece))))
(throw (IllegalStateException. (str coords " is NOT a valid position according to the mappings provided!")))))


;(for [letter "ABCDEFGH" ;strings are seqable
;     number (range 1 9)]
;(format "%c%d" letter number)))


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

(defn -main ;lein generated
  "I don't do a whole lot."
  [& args]
  (println "Hello, Clondie24!"))
  
  
