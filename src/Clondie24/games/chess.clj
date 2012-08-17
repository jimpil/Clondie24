(ns Clondie24.games.chess 
    (:require [Clondie24.lib.util :as ut] 
              [Clondie24.lib.core :as core]
              [Clondie24.lib.rules :as rul]
              [Clondie24.lib.search :as s]
              [Clondie24.lib.gui :as gui] :verbose :reload)
)

;----------------------------------------<SOURCE>--------------------------------------------------------------------
;----------------------------------------<CODE>----------------------------------------------------------------------

(def ^:const board-mappings-chess
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
 ;[7 1] [6 1] [5 1] [4 1] [3 1] [2 1] [1 1] [0 1]
 ;[0 2] [1 2] [2 2] [3 2] [4 2] [5 2] [6 2] [7 2]
 ;[7 3] [6 3] [5 3] [4 3] [3 3] [2 3] [1 3] [0 3]
 ;[0 4] [1 4] [2 4] [3 4] [4 4] [5 4] [6 4] [7 4]
 ;[7 5] [6 5] [5 5] [4 5] [3 5] [2 5] [1 5] [0 5]
 ;[0 6] [1 6] [2 6] [3 6] [4 6] [5 6] [6 6] [7 6]
; [7 7] [6 7] [5 7] [4 7] [3 7] [2 7] [1 7] [0 7]])

;(def game-on? (atom false))

(def chess-images "All the chess images paired up according to rank."
(zipmap '(:queen :rook :knight :bishop :pawn :king)
                     [[(ut/make-image "images/50px/png/Yellow Q.png")
                       (ut/make-image "images/50px/png/Black Q.png")]
                      [(ut/make-image "images/50px/png/Yellow R.png")
                       (ut/make-image "images/50px/png/Black R.png")]
                      [(ut/make-image "images/50px/png/Yellow N.png")
                       (ut/make-image "images/50px/png/Black N.png")]
                      [(ut/make-image "images/50px/png/Yellow B.png")
                       (ut/make-image "images/50px/png/Black B.png")]
                      [(ut/make-image "images/50px/png/Yellow P.png")
                       (ut/make-image "images/50px/png/Black P.png")]
                      [(ut/make-image "images/50px/png/Yellow K.png")
                       (ut/make-image "images/50px/png/Black K.png")]]))

(def chessPos->rank 
(flatten ['rook 'knight 'bishop 'queen 'king 'bishop 'knight 'rook (repeat 8 'pawn)]))

(declare make-chessItem, details, start-chess!) ;;will need these

(defn chess-best-move [dir b n] 
(s/start-search dir b n))

(defn starting-chessItems
"Will construct a set of initial chess items (16). black? specifies the side of the board where the pieces should be placed (true for north false for south)."
[black?]
(if black?  
(map #(make-chessItem (second (chess-images (keyword %2))) 
      (core/translate-position % board-mappings-chess) :rank %2 :direction 1) (range 16) chessPos->rank)
(map #(make-chessItem (first (chess-images (keyword %2))) 
      (core/translate-position % board-mappings-chess) :rank %2 :direction -1) (range 48 64) (reverse chessPos->rank))))

(def current-chessItems
"This is list that keeps track of moving checkers. Is governed by an atom and it changes after every move. All changes are being logged to 'board-history'. Starts off as nil but we can always get the initial arrangement from core."
(add-watch (atom nil) 
 :log (partial core/log-board core/board-history))) 
 
(def chess-moves {:pawn     (partial rul/pawn-moves board-mappings-chess) 
                  :rook     (partial rul/rook-moves board-mappings-chess)
                  :bishop   (partial rul/bishop-moves board-mappings-chess) 
                  :knight   (partial rul/knight-moves board-mappings-chess) 
                  :queen    (partial rul/queen-moves board-mappings-chess)
                  :king     (partial rul/king-moves board-mappings-chess)})
                                   
(defn rank->moves 
"Returns all legal moves of piece p depending on rank of p." 
[p] 
(let [[x y] (:position p) 
      r (keyword (:rank p))
      d (:direction p)]
(apply (r chess-moves) ;will return a fn which is called with current x and y 
         (list @current-chessItems x y d))))                                              

 ;partially apply move with game details locked in as 1st arg 
(def make-chessItem  (partial core/make-piece details)) 
(def vacant-chess-tile? (partial core/vacant? board-mappings-chess))

(defn start-chess! [] 
"Start a chess-game. Returns the starting-board."
(do (core/clear-history!) ;empty board-history
    (deliver s/curr-game details)
    (reset! current-chessItems
            (core/starting-board details))
    ));mandatory before game starts 
                      
(def details "The map that describes the game of chess."
              {:name 'Chess
               :players 2 
               :images chess-images
               :characteristics [:image :position :rank :value]      
               :board-size 64 
               :total-pieces 32
               :rel-values (zipmap '(:queen :rook :knight :bishop :pawn :king) 
                                   '(  9      5     3       3       1    100))
               :board-atom current-chessItems
               :game-starter start-chess!
               :hinter chess-best-move 
               :record-name "Clondie24.games.chess.ChessPiece" 
               :mappings board-mappings-chess
               :north-player-start  (starting-chessItems true)    ;opponent
               :south-player-start  (starting-chessItems false)}) ;human
               
               

 ;partially apply move with game details locked in as 1st arg
;(defn chess-mover [] (partial core/move details))  
(def make-chessItem  (partial core/make-piece details)) 
(def vacant-chess-tile? (partial core/vacant? board-mappings-chess))              

(defrecord ChessPiece [^java.awt.Image image 
                       ^clojure.lang.PersistentVector position 
                        rank ^Integer value direction]
 core/Piece 
 (update-position [this np] (make-chessItem image np :rank rank))
 (die [this]     (vary-meta this assoc :alive false)) ;communicate death through meta-data 
 (promote [this] (make-chessItem image position :rank 'queen)) ;a pawn is promoted to a queen
 (getListPosition [this] (core/translate-position (first  position) 
                                                  (second position) board-mappings-chess))
 (getPoint [this] (ut/make-point position))
 (getMoves [this] (rank->moves this)) ;returns a list of points [x y]
 Object
 (toString [this] 
   (println "ChessItem (" rank ") at position:" (core/getListPosition this) " ->" position)) )                
;---------------------------------------------------------------------------------------------               
(def chess-1d (range 64)) ;;the chess board as a list 
(def ^:dynamic black-direction -1)
(def ^:dynamic white-direction 1)   


;(deliver s/curr-game details) ;temporary
;(defmethod gui/new-game! 'Chess [_] (start-chess!) (reset! s/curr-game details)) ;hook on to the gui and search




(defn -main 
"Starts a swing Chess game." 
[& args]  (gui/show-gui! details)
#_(s/game-tree true 1 (start-chess!) s/next-level 4)
#(s/start-search 1 (start-chess!) 4)
)
         
               
