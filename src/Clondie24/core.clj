(ns Clondie24.core)
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
 (undo    [this])
 (getMovingPiece [this])
 (getStartPos [this])
 (getEndPos   [this])
)
 
 
(defn translate-position
"Translates a position from 1d to 2d and vice-versa. 
Mappings should be either 'checkers-board-mappings' or 'chess-board-mappings'." 
([i mappings] ;{:post [(not (nil? %))]}   
(let [grid-loc (get mappings i)] ;will translate from 1d to 2d
(if-not (nil? grid-loc) grid-loc 
(throw (IllegalStateException. (str "NOT a valid location:" i))))))
([x y ^clojure.lang.PersistentVector mappings] ;{:post [(not (== % -1))]} 
(let [list-loc (.indexOf mappings (vector (double x)  (double y)))] ;will translate from 2d to 1d
(if-not (== list-loc -1) list-loc 
(throw (IllegalStateException. (str "NOT a valid location:" x "," y)))))))

                    
(defn make-piece 
"The central function for creating pieces. A piece is simply a record with 3 keys: colour, position [x,y] and rank (optional)."
 [game c pos &{:keys [rank]
               :or {rank 'zombie}}]
(with-meta 
   ((record-factory (game :record-name)) c pos rank 
   (get (game :rel-values) (keyword rank))) 
{:dead false}))   ;pieces are born 'alive'              
             
                               
(defn starting-board [game] 
"Returns the initial board for a game with pieces on correct starting positions."
 ;opponent pieces come first, then 8 nils and our pieces last (conj appends at tail)
(let [p1 (game :north-player-start)
      p2 (game :south-player-start)
      vacant (- (game :board-size) (game :total-pieces))]
(flatten
  (conj p2 (conj (repeat vacant nil) p1)))))
  
 
(defn dead-piece? [p]
((meta p) :dead))

(defn empty-board 
"Returns an empty board - all nils." 
[game-map] 
(repeat (game-map :board-size) nil))
      

(defn populate-board 
"Builds the appropriate board (chess or chekers). Will have nil at vacant positions. Really ugly fn but it does everything in 1 pass!"
 ^clojure.lang.PersistentVector
[game-map board]
(loop [nb (vec (empty-board game-map)) ;building a brand new board after each move
       p  board]
(if (empty? p) nb
  (let [fp (first p)]
    (recur 
    (if (nil? fp) nb ;if encounter nil just carry on recursing with the current board
       (assoc nb  ;else
          (getListPosition fp)    ;the piece's position
        (if (dead-piece? fp)   nil ;if the piece is dead stick nil
                         fp)))  ; else stick the piece in
         (rest p) )))))  ;carry on recursing
               

(defn move 
"The function responsible for moving Pieces. Each piece knows how to move itself. Returns the resulting board without making any state changes. " 
 ^clojure.lang.PersistentVector 
[game-map p coords] 
{:pre [(satisfies? Piece p)]}  ;safety comes first
(if  (some #{(vector-of-doubles coords)} (game-map :mappings)) ;check that the position exists on the grid
(let [newPiece (update-position p coords)] ;the piece that results from the move
(populate-board game-map   ;replace dead-pieces with nils
(-> @(game-map :board-atom)    ;deref the appropriate board atom 
    (assoc (getListPosition p) nil) 
    (assoc (getListPosition newPiece) newPiece))))
(throw (IllegalArgumentException. (str coords " is NOT a valid position according to the mappings provided!")))))


;(for [letter "ABCDEFGH" ;strings are seqable
;     number (range 1 9)]
;(format "%c%d" letter number)))


;EXAMPLEs:
 ;(make-checker    (make-color 'BLUE)  [1 5] :rank 'soldier)
 ;(make-checker    (make-color 'WHITE) [0 0])   ;rank will default to 'zombie
 ;(make-chessItem  (make-image "bishop-icon.png") [2 3] :rank 'bishop)


(defn -main ;lein generated
  "I don't do a whole lot."
  [& args]
  (println "Hello, Clondie24!"))
  
  
