(ns Clondie24.games.chess 
    (:require [Clondie24.lib.util :as ut] 
              [Clondie24.lib.core :as core]
              [Clondie24.lib.search :as s]
              [Clondie24.lib.rules :as rul]
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

(def rel-values (zipmap '(:queen :rook :knight :bishop :pawn :king) 
                        '(  9      5     3       3       1    100)))

(declare details, start-chess!, buffered-moves) ;;will need these

(defn chess-best-move [dir b n] 
(s/fake dir b n))

(def current-chessItems
"This is list that keeps track of moving checkers. Is governed by an atom and it changes after every move. All changes are being logged to 'board-history'. Starts off as nil but we can always get the initial arrangement from core."
(add-watch (atom nil) 
 :log (partial core/log-board core/board-history))) 
 
(def chess-moves {:pawn     (partial rul/pawn-moves board-mappings-chess) 
                  :rook      rul/rook-moves
                  :bishop    rul/bishop-moves
                  :knight    rul/knight-moves
                  :queen     rul/queen-moves
                  :king      rul/king-moves})
                                   
(defn rank->moves 
"Returns all legal moves of piece p depending on rank of p (excluding pawn)." 
[p]
(let [pos (:position p)            ;[[x y] (:position p) 
      r (keyword (:rank p))
      d (:direction p)]      
(apply (r chess-moves) pos))) ;will return a fn which is called with current x and y 
;(if (= (class p) (Class/forName "Clondie24.games.chess.ChessPiece2"))
;(ut/Point->Vec pos) pos))))  
                                              

(defn start-chess! [fast?] 
"Start a chess-game. Returns the starting-board."
(do (core/clear-history!) ;empty board-history
    (deliver s/curr-game details)
    (reset! current-chessItems
  (if fast? (into-array  (core/starting-board details))
                         (core/starting-board details)))
    ));mandatory before game starts 
               
;(def make-chessItem  (partial core/make-piece details))  (->ChessPiece)
(def vacant-chess-tile? (partial core/vacant? board-mappings-chess))                         

(defrecord ChessPiece [^java.awt.Image image 
                       ^clojure.lang.PersistentVector position 
                        rank ^long value direction]
 core/Piece 
 (update-position [this np] (ChessPiece. image np rank value direction))
 (die [this]     (vary-meta this assoc :alive false)) ;communicate death through meta-data 
 (promote [this] (ChessPiece. image position rank value direction)) ;a pawn is promoted to a queen
 (getListPosition [this] (core/translate-position (first  position) (second position) board-mappings-chess))
 (getPoint [this] (ut/make-point position))
 (getMoves [this b] (let [[x y] position]
               (remove 
                              ;#(or 
                              #(core/collides? position % (ut/make-walker 
                                                   (ut/resolve-direction position %) rank) 
                                                    b board-mappings-chess direction)
                               ;(core/exposes-king? (core/dest->Move b this %) b)
                                
                  (if-not (= rank 'pawn)                                
                    (get-in buffered-moves [(core/translate-position 
                                                  x y board-mappings-chess) (keyword rank)]) ;returns a list of points [x y]
                   (apply ((keyword rank) chess-moves) (list b x y direction))))))  
 Object
 (toString [this] 
   (println "ChessItem (" rank ") at position:" (core/getListPosition this) " ->" position)) )
   
(defrecord ChessPiece2 [^java.awt.Image image 
                        ^java.awt.Point position 
                         rank ^long value direction]
 core/Piece 
 (update-position [this np] (do (set! (.x position) (first np)) 
                                (set! (.y position) (second np)) this))
 (die [this]     (vary-meta this assoc :alive false)) ;communicate death through meta-data 
 (promote [this] (ChessPiece2. image position rank value direction)) ;a pawn is promoted to a queen
 (getListPosition [this] (core/translate-position (.x position) (.y position) board-mappings-chess))
 (getPoint [this] position)
 (getMoves [this b] (let [[x y] (ut/Point->Vec position)]
                  (remove #(core/acollides? [x y] % (ut/make-walker  (ut/resolve-direction [x y] %) rank) 
                                                     b board-mappings-chess direction) 
                  (if-not (= rank 'pawn)                                
                    (get-in buffered-moves [(core/translate-position 
                                                  x y board-mappings-chess) (keyword rank)]) ;returns a list of points [x y]
                   (apply ((keyword rank) chess-moves) (list (vec b) x y direction))))))  
 Object
 (toString [this] 
   (println "ChessItem (" rank ") at position:" (core/getListPosition this) " = " position)) )   
   
(defn starting-chessItems
"Will construct a set of initial chess items (16). black? specifies the side of the board where the pieces should be placed (true for north false for south)."
[black?]
(if black?  
(map #(ChessPiece. (second (chess-images (keyword %2))) 
      (core/translate-position % board-mappings-chess) %2
                       ((keyword %2) rel-values)  1 {:alive true} nil) (range 16) chessPos->rank)
(map #(ChessPiece. (first (chess-images (keyword %2))) 
      (core/translate-position % board-mappings-chess) %2
                      ((keyword %2) rel-values)  -1 {:alive true} nil) (range 48 64) (reverse chessPos->rank))))
                      
(def details "The map that describes the game of chess."
              {:name 'Chess
               :players 2 
               :images chess-images
               :characteristics [:image :position :rank :value]      
               :board-size 64 
               :total-pieces 32
               :rel-values rel-values
               :board-atom current-chessItems
               :game-starter start-chess!
               :hinter chess-best-move 
               :record-name "Clondie24.games.chess.ChessPiece" ;;fully qualified name
               :mappings board-mappings-chess
               :north-player-start  (starting-chessItems true)   ;opponent (black)
               :south-player-start  (starting-chessItems false)});human (white or yellow)
                   
;---------------------------------------------------------------------------------------------               

#_(def buffered-moves "Precalculate the logical moves of chess-pieces for better performance. Does not apply to pawn."           
(loop [k 0 m (hash-map)]
(if (= 64 k) m
(recur (inc k) 
 (assoc m k 
    (zipmap [:rook :knight :bishop :queen :king] 
     (map #(rank->moves (ChessPiece. nil (core/translate-position k board-mappings-chess) % 
               ((keyword %) (:rel-values details)) nil)) 
      ['rook 'knight 'bishop 'queen 'king])))))))

;(ut/persist buffered-moves "performance.cheat") 
(def buffered-moves (ut/unpersist "performance.cheat"))          

(defn -main 
"Starts a graphical (swing) Chess game." 
[& args]  
(gui/show-gui! details)
#_(time (s/fake -1 (start-chess! false) 4) #_(println @s/mmm))
)





         
               
