(ns Clondie24.core
   (:use [clojure.pprint :only (pprint, print-table)])
)

(def ^:const board-mappings-chess
"A vector of vectors. Outer vector represents the 64 (serial) positions chess-items can position themselves on. 
 Each inner vector represents the coordinates of that position on the 8x8 grid."
[
[0.0 0.0] [1.0 0.0] [2.0 0.0] [3.0 0.0] [4.0 0.0] [5.0 0.0] [6.0 0.0] [7.0 0.0]
[7.0 1.0] [6.0 1.0] [5.0 1.0] [4.0 1.0] [3.0 1.0] [2.0 1.0] [1.0 1.0] [0.0 1.0]
[0.0 2.0] [1.0 2.0] [2.0 2.0] [3.0 2.0] [4.0 2.0] [5.0 2.0] [6.0 2.0] [7.0 2.0]
[7.0 3.0] [6.0 3.0] [5.0 3.0] [4.0 3.0] [3.0 3.0] [2.0 3.0] [1.0 3.0] [0.0 3.0]
[0.0 4.0] [1.0 4.0] [2.0 4.0] [3.0 4.0] [4.0 4.0] [5.0 4.0] [6.0 4.0] [7.0 4.0]
[7.0 5.0] [6.0 5.0] [5.0 5.0] [4.0 5.0] [3.0 5.0] [2.0 5.0] [1.0 5.0] [0.0 5.0]
[0.0 6.0] [1.0 6.0] [2.0 6.0] [3.0 6.0] [4.0 6.0] [5.0 6.0] [6.0 6.0] [7.0 6.0]
[7.0 7.0] [6.0 7.0] [5.0 7.0] [4.0 7.0] [3.0 7.0] [2.0 7.0] [1.0 7.0] [0.0 7.0]
])

(def ^:const board-mappings-checkers 
"A vector of vectors. Outer vector represents the 32 (serial) positions checkers can position themselves on. 
 Each inner vector represents the coordinates of that position on the 8x8 grid."
[
[1.0 0.0] [3.0 0.0] [5.0 0.0] [7.0 0.0] 
[6.0 1.0] [4.0 1.0] [2.0 1.0] [0.0 1.0]
[1.0 2.0] [3.0 2.0] [5.0 2.0] [7.0 2.0] 
[6.0 3.0] [4.0 3.0] [2.0 3.0] [0.0 3.0]
[1.0 4.0] [3.0 4.0] [5.0 4.0] [7.0 4.0]
[6.0 5.0] [4.0 5.0] [2.0 5.0] [0.0 5.0]
[1.0 6.0] [3.0 6.0] [5.0 6.0] [7.0 6.0]
[6.0 7.0] [4.0 7.0] [2.0 7.0] [0.0 7.0]
])


(def ^:const checkers-rel-values {:soldier 1 :prince 3}) ;only 2 types in checkers

(def ^:const chess-ranks '(:queen :rook :knight :bishop :pawn :king)) ;all the different chess piece-types 
(def ^:const rel-values  '(9 5 3 3 1 100))  ;relative values according to wikipedia
(def ^:const chess-rel-values (zipmap chess-ranks rel-values)) ;=> {:king 100, :pawn 1, :bishop 3, :knight 3, :rook 5, :queen 9}

(def board-history 
"Log of the state of a game." 
(atom []))

(defn log-board 
"The logging function for the board ref. Will conj every new board-state into a vector." 
[dest k s old n] 
(when (not= old n) 
  (swap! dest conj n)))


(defmacro doeach 
"Like doseq but in a map-like manner. Assumes f is side-effecty." 
 [f coll]
`(doseq [x# ~coll] (~f x#)))

;Helper macro for creting Points
(defmacro make-point [p]
`(java.awt.Point. (first ~p) (second ~p)))


(defmacro make-image 
"Returns a buffered-image from the specified file or nil if the file is not there.." 
[path-to-image]
`(try (javax.imageio.ImageIO/read (java.io.File. ~path-to-image))
 (catch java.io.IOException e# ;returning nil here is ok! 
  (println ~path-to-image "does not exist! Reverting to 'nil'..."))))

;Helper macro for creting pre-defined Colours
(defmacro make-color [predefined-name]
`(.get (.getField (Class/forName "java.awt.Color") (str ~predefined-name)) nil))


;RED is machine (north camp), YELLOW is human (south camp)            
(defn checkers-colors [c1 c2] 
[(make-color c1)  
 (make-color c2)]) 

                       
(def chess-images 
(zipmap chess-ranks  [
                      [(make-image "white queen-image")
                       (make-image "black queen-image")]
                      [(make-image "white rook-image")
                       (make-image "black rook-image")]
                      [(make-image "white knight-image")
                       (make-image "black knight-image")]
                      [(make-image "white pawn-image")
                       (make-image "black pawn-image")]
                      [(make-image "white king-image")
                       (make-image "black king-image")]
                      ]))
 

(def chess    {:players 2 
               :images chess-images     
               :board-size 64 
               :pieces 32 
               :mappings board-mappings-chess})
                       
(def checkers {:players 2 
               :colors (checkers-colors 'RED 'YELLOW) 
               :board-size 32 
               :pieces 24 
               :mappings board-mappings-checkers})

(def   current-chessItems nil) ;TODO

(def ^:dynamic black-direction -1)
(def ^:dynamic white-direction 1)

(def valid-checkers-positions    (range 32))
(def invalid-checkers-positions  (repeat 32 -1))

(def checkers-1d ;the checkers-board as a list. -1 represents invalid positions
(interleave invalid-checkers-positions 
            valid-checkers-positions))
                      

(declare make-piece, make-checker, make-chessItem, translate-position) ;will need all this 

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
 (execute [this])
 (undo    [this])
 (getMovingPiece [this])
 (getStartPos [this])
 (getEndPos   [this])
)
 


(defrecord CheckersPiece [^java.awt.Color color 
                          ^java.awt.Point position 
                           rank ^Integer value] 
 Piece 
 (update-position [this np] ;mutable state inside Point!
   (.setLocation position   ;can accept ints or doubles
    ^double (first np) ^double (second np))) ;np should be [x, y]
 (die     [this] (vary-meta this assoc :dead true)) ;communicate death through meta-data 
 (promote [this] (make-checker color position :rank 'prince)) ; a checker is promoted to prince
 (getGridPosition [this] (vector (.getX position) (.getY position)))
 (getListPosition [this] (translate-position  (first  (.getGridPosition this)) 
                                              (second (.getGridPosition this)) (checkers :mappings)))
 (getPoint [this] position)
 (getMoves [this] nil) ;TODO
 Object
 (toString [this] 
   (println "Checker (" rank ") at position:" (.getListPosition this) " ->" (.getGridPosition this))) )
 
(defrecord ChessPiece [^java.awt.Image image 
                       ^java.awt.Point position 
                        rank ^Integer value]
 Piece 
 (update-position [this np] ;mutable state inside Point!
   (.setLocation position   ;can accept ints or doubles
    ^double (first np) ^double (second np))) ;np should be [x, y]
 (die [this]     (vary-meta this assoc :dead true)) ;communicate death through meta-data 
 (promote [this] (make-chessItem image position :chess? true :rank 'queen)) ;a pawn is promoted to a queen
 (getGridPosition [this] (vector (.getX position) (.getY position)))
 (getListPosition [this] (translate-position (first  (.getGridPosition this)) 
                                             (second (.getGridPosition this)) (chess :mappings)))
 (getPoint [this] position)
 (getMoves [this] nil) ;TODO 
 Object
 (toString [this] 
   (println "ChessItem (" rank ") at position:" (.getListPosition this) " ->" (.getGridPosition this))) )
   

(defn translate-position
"Translates a position from 1d to 2d and vice-versa. 
Mappings should be either 'checkers-board-mappings' or 'chess-board-mappings'." 
([i mappings] {:post [(not (nil? %))]}   ;will translate from 1d to 2d
      (get mappings i)) 
([x y ^clojure.lang.PersistentVector mappings] {:post [(not (== % -1))]} ;will translate from 2d to 1d
(.indexOf mappings (vector (double x)  (double y)))))



(defmacro in? ;handy macro to test if some element exists in some collection 
 "Returns true if colle contains elm, false otherwise."
 [colle elm]  
`(if (some #{~elm} ~colle) true false)) 

(defmacro vector-of-doubles [v] 
`(vec (map double ~v))) 


(defn make-piece 
"The central function for creating pieces. A piece is simply a record with 3 keys: colour, position [x,y] and rank (optional)."
 [game c pos &{:keys [rank]
               :or {rank 'zombie}}]
(condp = game
    'chess    (with-meta (ChessPiece. c pos rank (chess-rel-values (keyword rank))) 
                         {:dead false})  ;pieces are born 'alive'
    'checkers (with-meta (CheckersPiece. c pos rank (checkers-rel-values (keyword rank))) 
                         {:dead false})))               
             

(def make-checker   (partial make-piece 'checkers)) 
(def make-chessItem (partial make-piece 'chess))                                                 
            
(defn starting-checkers
"Will construct a set of initial checkers (12). opponent? specifies the side of the board where the pieces should be placed (true for north false for south).   " 
[opponent?]
(let [[red yellow] (checkers-colors 'RED 'YELLOW)]                                   
(if opponent?  
(map #(make-checker red
      (make-point (translate-position % (checkers :mappings))) :rank 'soldier) (range 12))
(map #(make-checker yellow 
      (make-point (translate-position % (checkers :mappings))) :rank 'soldier) (range 20 32))      
)))

(def chessPos->rank 
(flatten ['rook 'knight 'bishop 'queen 'king 'bishop 'knight 'rook (repeat 8 'pawn)]))

(defn starting-chessItems
"Will construct a set of initial checkers (12). opponent? specifies the side of the board where the pieces should be placed (true for north false for south)."
[opponent?]
(if opponent?  
(map #(make-chessItem (second (chess-images (keyword %2))) 
      (make-point (translate-position % (chess :mappings))) :rank %2) (range 16) chessPos->rank)
(map #(make-chessItem (first (chess-images (keyword %2))) 
      (make-point (translate-position % (chess :mappings))) :rank %2) (range 48 64) (reverse chessPos->rank))      
))



(def current-checkers 
"This is list that keeps track of moving checkers. Is governed by an atom and it changes after every move. All changes are being logged to 'board-history'."
(add-watch 
(atom (concat (starting-checkers true) 
              (starting-checkers false)) 
      :validator #(== 24 (count %))) 
  :log (partial log-board board-history)))
              
(def current-chessItems
"This is list that keeps track of moving checkers. Is governed by an atom and it changes after every move. All changes are being logged to 'board-history'."
(add-watch 
(atom (concat (starting-chessItems true) 
              (starting-chessItems false)) 
      :validator #(== 32 (count %))) 
  :log (partial log-board board-history)))              

(defn starting-board [game] 
"Returns the initial board for a game with correct starting positions."
 ;opponent pieces come first, then 8 nils and our pieces last (conj appends at tail)
(condp = game
 'chess
       (flatten 
          (conj (starting-chessItems false) 
          (conj (repeat 32 nil) (starting-chessItems true))))
 'checkers 
       (flatten
          (conj (starting-checkers false) 
          (conj (repeat 8 nil) (starting-checkers true))))
))

(defmacro dead-piece? [p]
`((meta ~p) :dead))

(defmacro empty-board 
"This is a macro for performance reasons. I want to keep build-board fn as tight as possible." 
[game] 
`(condp = ~game 
       (symbol "chess")    (repeat 64 nil) 
       (symbol "checkers") (repeat 32 nil)
))       
     
(defmacro current-items 
"This is a macro for performance reasons. I want to keep build-board fn as tight as possible." 
[game atom?]
`(condp = ~game 
       (symbol "chess")    (if ~atom? current-chessItems @current-chessItems) 
       (symbol "checkers") (if ~atom? current-checkers   @current-checkers)
))
      

(defn build-board 
"Builds the appropriate board (chess or chekers). Will have nil at vacant positions."
 ^clojure.lang.PersistentVector$ChunkedSeq 
[game]
(loop [nb (vec (empty-board game)) ;building a brand new board after each move
       p  (current-items game false)]
(if (empty? p) (seq nb)
  (let [fp (first p)]
    (recur (assoc nb (getListPosition fp)    ;the piece's position
      (if (dead-piece? fp)  nil ;if the piece is dead stick nil
                      fp))  ; else stick the piece in
         (rest p))))))  ;carry on recursing
               
 (defmacro clean "Filter out nils from a collection." 
  [c] 
 `(filter #(not (nil? %)) ~c))              
 
 (defn vacant? 
 "Checks if a position [x, y] is vacant on the given board and mappings. Is a macro for performance reasons." 
 ^Boolean [m b pos]
 (let [[x y] pos]
 (nil? 
  (nth b (translate-position x y m)))))
  
 (def vacant-checker-tile?  (partial vacant? board-mappings-checkers))
 (def vacant-chess-tile?    (partial vacant? board-mappings-chess))                   

(def chess-1d (range 64)) ;;the chess board as a list

(defn to-2d [b] ; the checkers or chess board as 8 rows of 8 columns
(map vec        ;apply 'reverse' to every second item in 1d-board
(map #(%1 %2) (cycle [identity reverse]) (partition 8 b))))


(defn move 
"The function responsible for moving Pieces. Each piece knows how to move itself. Returns the new board." 
 ^clojure.lang.LazySeq 
[game mappings  p coords] 
{:pre [(satisfies? Piece p)]}  ;safety comes first
(if (in? mappings (vector-of-doubles coords)) ;check that position exists on the grid
(do  (update-position p coords) ;coords should be of the form [x, y]
(reset! (current-items game true) ;replace the board atom - log new state
        (clean (build-board game)))) ;;replace the old board with the new
(throw (IllegalArgumentException. (str coords " is NOT a valid position according to the mappings provided!")))))


;partially apply move with game and checker-mappings locked in as 1st & 2nd args     
(def move-checker   (partial move 'checkers board-mappings-checkers))
;partially apply move with game and chess-mappings locked in as 1st & 2nd args
(def move-chessItem (partial move 'chess board-mappings-chess))  

 (defrecord ChessMove [^ChessPiece p
                       ^clojure.lang.PersistentVector start-pos 
                       ^clojure.lang.PersistentVector end-pos]
 MoveCommand
 (execute [this] (move-chessItem p (.getEndPos this)))
 (undo    [this] (move-chessItem p (.getStartPos this)))
 (getMovingPiece [_] p)
 (getStartPos [_] (vector-of-doubles start-pos))
 (getEndPos   [_] (vector-of-doubles end-pos))
 Object
 (toString [this] 
   (println "Checkers-move originating from" (.getStartPos this) "to" (.getEndPos this))))
 
 (defrecord CheckersMove [^CheckersPiece p
                          ^clojure.lang.PersistentVector start-pos 
                          ^clojure.lang.PersistentVector end-pos]
 MoveCommand
 (execute [this] (move-checker p (getEndPos this)))
 (undo    [this] (move-checker p (getStartPos this)))
 (getMovingPiece [_] p)
 (getStartPos [_] (vector-of-doubles start-pos))
 (getEndPos   [_] (vector-of-doubles end-pos))
 Object
 (toString [this] 
   (println "Chess-move originating from" (.getStartPos this) "to" (.getEndPos this))))
 

(defn printBoard 
"Will print the detailed board with nils where vacant. Calls build-board without 'cleaning' it." 
[game] 
(print-table (build-board game)))


;(for [letter "ABCDEFGH" ;strings are seqable
;     number (range 1 9)]
;(format "%c%d" letter number)))


;EXAMPLEs:
 ;(make-checker    (make-color 'BLUE)  (make-point [1 5]) :rank 'soldier)
 ;(make-checker    (make-color 'WHITE) (make-point [0 0]))   ;rank will default to 'zombie
 ;(make-chessItem  (make-image "bishop-icon.png") (make-point [2 3]) :rank 'bishop)





(defn -main ;lein generated
  "I don't do a whole lot."
  [& args]
  (println "Hello, Clondie24!"))
  
  
