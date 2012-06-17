(ns Clondie24.chess 
    (:require [Clondie24.util :as ut] 
              [Clondie24.core :as core])
)

;----------------------------------------<SOURCE>--------------------------------------------------------------------
;----------------------------------------<CODE>----------------------------------------------------------------------

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



(def chessPos->rank 
(flatten ['rook 'knight 'bishop 'queen 'king 'bishop 'knight 'rook (repeat 8 'pawn)]))

(defn starting-chessItems
"Will construct a set of initial checkers (12). opponent? specifies the side of the board where the pieces should be placed (true for north false for south)."
[opponent?]
(if opponent?  
(map #(make-chessItem (second (chess-images (keyword %2))) 
      (translate-position % (get (details) :mappings)) :rank %2) (range 16) chessPos->rank)
(map #(make-chessItem (first (chess-images (keyword %2))) 
      (translate-position % (get (details) :mappings)) :rank %2) (range 48 64) (reverse chessPos->rank))      
))

(def chess-images 
(zipmap (get (details) :ranks)[
                      [(ut/make-image "white queen-image")
                       (ut/make-image "black queen-image")]
                      [(ut/make-image "white rook-image")
                       (ut/make-image "black rook-image")]
                      [(ut/make-image "white knight-image")
                       (ut/make-image "black knight-image")]
                      [(ut/make-image "white pawn-image")
                       (ut/make-image "black pawn-image")]
                      [(ut/make-image "white king-image")
                       (ut/make-image "black king-image")]
                      ]))
                      
(defn details "Returns a map that describes the game of chess."
^clojure.lang.PersistentArrayMap []
              {:name 'chess
               :players 2 
               :images chess-images
               :characteristics [:image :position :rank :value]      
               :board-size 64 
               :total-pieces 32

               :rel-values (zipmap '(:queen :rook :knight :bishop :pawn :king) 
                                   '(  9      5     3       3       1     100))
               :board-atom current-chessItems
               :record-name "ChessPiece" 
               :mappings board-mappings-chess
               :north-player-start  (starting-chessItems true)
               :south-player-start  (starting-chessItems false)})
               
(def current-chessItems
"This is list that keeps track of moving checkers. Is governed by an atom and it changes after every move. All changes are being logged to 'board-history'."
(add-watch 
(atom (vec (core/starting-board (details))) ) 
      ;:validator #(== 32 (count %))   
  :log (partial core/log-board core/board-history))) 
  
;partially apply move with game and chess-mappings locked in as 1st & 2nd args
(def move-chessItem (partial core/move  (details)))  
(def make-chessItem (partial core/make-piece (details))) 
(def vacant-chess-tile? (partial ut/vacant? (get (details) mappings)))

(defrecord ChessPiece [^java.awt.Image image 
                       ^clojure.lang.PersistentVector position 
                        rank ^Integer value]
 Piece 
 (update-position [this np] (make-chessItem image position rank))
 (die [this]     (vary-meta this assoc :dead true)) ;communicate death through meta-data 
 (promote [this] (make-chessItem image position :rank 'queen)) ;a pawn is promoted to a queen
 (getGridPosition [this] position)
 (getListPosition [this] (core/translate-position (first  position) 
                                                  (second position) (get (chess) :mappings)))
 (getPoint [this] (make-point position))
 (getMoves [this] nil) ;TODO 
 Object
 (toString [this] 
   (println "ChessItem (" rank ") at position:" (getListPosition this) " ->" (getGridPosition this))) )

 (defrecord ChessMove [^ChessPiece p
                       ^clojure.lang.PersistentVector start-pos 
                       ^clojure.lang.PersistentVector end-pos]
 MoveCommand
 (try-move [this] (move-chessItem p (getEndPos this)))
 (execute [this]  (reset! (get (details) :board-atom) (try-move this))) ;STATE CHANGE!
 (undo    [this]  (move-chessItem p (getStartPos this)))
 (getMovingPiece [_] p)
 (getStartPos [_] (utvector-of-doubles start-pos))
 (getEndPos   [_] (utvector-of-doubles end-pos))
 Object
 (toString [this] 
   (println "Checkers-move originating from" (getStartPos this) "to" (getEndPos this))))                
;---------------------------------------------------------------------------------------------               
(def chess-1d (range 64)) ;;the chess board as a list 
(def ^:dynamic black-direction -1)
(def ^:dynamic white-direction 1)              
               
