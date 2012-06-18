(ns Clondie24.chess 
    (:require [Clondie24.util :as ut] 
              [Clondie24.core :as core]
              [Clondie24.rules :as rul])
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


(def chess-images 
(zipmap '(:queen :rook :knight :bishop :pawn :king)
                     [
                      [(ut/make-image "white queen-image")
                       (ut/make-image "black queen-image")]
                      [(ut/make-image "white rook-image")
                       (ut/make-image "black rook-image")]
                      [(ut/make-image "white knight-image")
                       (ut/make-image "black knight-image")]
                      [(ut/make-image "white bishop-image")
                       (ut/make-image "black bishop-image")]
                      [(ut/make-image "white pawn-image")
                       (ut/make-image "black pawn-image")]
                      [(ut/make-image "white king-image")
                       (ut/make-image "black king-image")]
                      ]))

(def chessPos->rank 
(flatten ['rook 'knight 'bishop 'queen 'king 'bishop 'knight 'rook (repeat 8 'pawn)]))

(declare make-chessItem, details) ;;will need these

(defn starting-chessItems
"Will construct a set of initial chess items (16). opponent? specifies the side of the board where the pieces should be placed (true for north false for south)."
[opponent?]
(if opponent?  
(map #(make-chessItem (second (chess-images (keyword %2))) 
      (core/translate-position % board-mappings-chess) :rank %2) (range 16) chessPos->rank)
(map #(make-chessItem (first (chess-images (keyword %2))) 
      (core/translate-position % board-mappings-chess) :rank %2) (range 48 64) (reverse chessPos->rank))      
))

(def chess-moves {:pawn     nil ;TODO
                  :rook     nil ;TODO
                  :bishop   nil ;TODO
                  :knight   #(rul/knight-moves %1 %2)
                  :queen    nil   ;TODO
                  :king     nil}) ;TODO
                  
(defn rank->moves 
"Returns all legal moves of p depending on rank of p." 
[p] 
(let [gpos (core/getGridPosition p)]
((chess-moves (keyword (:rank p))) ;will return a fn
 (first gpos) (second gpos))))

                      
(def current-chessItems
"This is list that keeps track of moving checkers. Is governed by an atom and it changes after every move. All changes are being logged to 'board-history'. Starts off as nil but we can always get the initial arrangement from core."
(add-watch 
(atom nil #_(vec (core/starting-board (details))) ) 
      ;:validator #(== 32 (count %))   
  :log (partial core/log-board core/board-history)))                       
                      
(defn details 
"Returns a map that describes the game of chess." []
              {:name 'chess
               :players 2 
               :images chess-images
               :characteristics [:image :position :rank :value]      
               :board-size 64 
               :total-pieces 32
               :rel-values (zipmap '(:queen :rook :knight :bishop :pawn :king) 
                                   '(  9      5     3       3       1     100))
               :board-atom current-chessItems
               :record-name "Clondie24.chess.ChessPiece" 
               :mappings board-mappings-chess
               :north-player-start  (starting-chessItems true)    ;opponent
               :south-player-start  (starting-chessItems false)}) ;human
               
  
;partially apply move with game and chess-mappings locked in as 1st & 2nd args
(def move-chessItem (partial core/move  (details)))  
(def make-chessItem (partial core/make-piece (details))) 
(def vacant-chess-tile? (partial core/vacant? (get (details) :mappings)))

(defrecord ChessPiece [^java.awt.Image image 
                       ^clojure.lang.PersistentVector position 
                        rank ^Integer value]
 core/Piece 
 (update-position [this np] (make-chessItem image position rank))
 (die [this]     (vary-meta this assoc :dead true)) ;communicate death through meta-data 
 (promote [this] (make-chessItem image position :rank 'queen)) ;a pawn is promoted to a queen
 (getGridPosition [this] position)
 (getListPosition [this] (core/translate-position (first  position) 
                                                  (second position) (get (details) :mappings)))
 (getPoint [this] (ut/make-point position))
 (getMoves [this] (rank->moves this)) ;returns a list of points [x y]
 Object
 (toString [this] 
   (println "ChessItem (" rank ") at position:" (core/getListPosition this) " ->" (core/getGridPosition this))) )

 (defrecord ChessMove [^ChessPiece p
                       ^clojure.lang.PersistentVector start-pos 
                       ^clojure.lang.PersistentVector end-pos]
 core/MoveCommand
 (try-move [this] (move-chessItem p (core/getEndPos this)))
 (execute [this]  (reset! (get (details) :board-atom) (core/try-move this))) ;STATE CHANGE!
 (undo    [this]  (move-chessItem p (core/getStartPos this)))
 (getStartPos [_] (ut/vector-of-doubles start-pos))
 (getEndPos   [_] (ut/vector-of-doubles end-pos))
 Object
 (toString [this] 
   (println "Checkers-move originating from" (core/getStartPos this) "to" (core/getEndPos this))))                
;---------------------------------------------------------------------------------------------               
(def chess-1d (range 64)) ;;the chess board as a list 
(def ^:dynamic black-direction -1)
(def ^:dynamic white-direction 1)              
               
