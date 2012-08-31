(ns Clondie24.games.chess 
    (:require [Clondie24.lib.util :as ut] 
              [Clondie24.lib.core :as core]
              [Clondie24.lib.search :as s]
              [Clondie24.lib.rules :as rul]
              [Clondie24.lib.gui :as gui] :verbose :reload)
)

;----------------------------------------<SOURCE>--------------------------------------------------------------------
;----------------------------------------<CODE>----------------------------------------------------------------------

(def board-mappings-chess core/mappings-8x8)


(def chess-images "All the chess images paired up according to rank."
(zipmap '(:queen :rook :knight :bishop :pawn :king)
                     [{-1 (ut/make-image "images/50px/png/Yellow Q.png")
                        1 (ut/make-image "images/50px/png/Black Q.png")}
                      {-1 (ut/make-image "images/50px/png/Yellow R.png")
                        1 (ut/make-image "images/50px/png/Black R.png")}
                      {-1 (ut/make-image "images/50px/png/Yellow N.png")
                        1 (ut/make-image "images/50px/png/Black N.png")}
                      {-1 (ut/make-image "images/50px/png/Yellow B.png")
                        1 (ut/make-image "images/50px/png/Black B.png")}
                      {-1 (ut/make-image "images/50px/png/Yellow P.png")
                        1 (ut/make-image "images/50px/png/Black P.png")}
                      {-1 (ut/make-image "images/50px/png/Yellow K.png")
                        1 (ut/make-image "images/50px/png/Black K.png")}]))
  [{1 (ut/make-image "images/50px/png/Black Q.png")
   -1 (ut/make-image "images/50px/png/Yellow Q.png")}]                     
                       
(def promotion-row {1 7 -1 0})                       

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
"Returns all legal moves of piece p depending on rank of p (excluding pawn). Will be called only once to buffer the moves." 
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
               
(def chess-piece  (partial core/piece details))
(def vacant-chess-tile? (partial core/vacant? board-mappings-chess))                         

(defrecord ChessPiece [^java.awt.Image image 
                       ^clojure.lang.PersistentVector position 
                        rank ^long value direction]
 core/Piece 
 (update-position [this np] (if (and (= (second np) (get promotion-row direction))
                                     (= rank 'pawn)) (core/promote this np) 
                                (ChessPiece. image np rank value direction {:alive true} nil)))
 (die [this]     (vary-meta this assoc :alive false)) ;communicate death through meta-data 
 (promote [this np] (ChessPiece. (get-in chess-images [:queen direction]) np 'queen 9 direction)) ;a pawn is promoted to a queen
 (getListPosition [this] (core/translate-position (first  position) (second position) board-mappings-chess))
 (getPoint [this] (ut/make-point position))
 (getMoves [this b with-precious?]
  (let [[x y] position]
    (core/remove-illegal #(or 
                             (core/collides? (core/dest->Move b this % (:mover details)) 
                                 (ut/make-walker 
                                 (ut/resolve-direction position %) rank) b board-mappings-chess)
                            (core/exposes? (core/dest->Move b this % (:mover details)) (if with-precious? 'king nil)))    
                  (if (= rank 'pawn) (apply ((keyword rank) chess-moves) (list b x y direction))                               
                    (get-in buffered-moves [(core/translate-position x y board-mappings-chess) 
                                            (keyword rank)]))))) ;returns a list of points [x y]
 Object
 (toString [this] 
   (println "ChessItem (" rank ") at position:" (core/getListPosition this) " ->" position)) )
   
#_(defrecord ChessPiece2 [^java.awt.Image image 
                        ^java.awt.Point position 
                         rank ^long value direction]
 core/Piece 
 (update-position [this np] (do (set! (.x position) (first np)) 
                                (set! (.y position) (second np)) this))
 (die [this]     (vary-meta this assoc :alive false)) ;communicate death through meta-data 
 (promote [this] (ChessPiece2. image position rank value direction)) ;a pawn is promoted to a queen
 (getListPosition [this] (core/translate-position (.x position) (.y position) board-mappings-chess))
 (getPoint [this] position)
 (getMoves [this b safe?] (let [[x y] (ut/Point->Vec position)]
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
(map #(ChessPiece. (get-in chess-images [(keyword %2) 1])
      (core/translate-position % board-mappings-chess) %2
                       ((keyword %2) rel-values)  1 {:alive true} nil) (range 16) chessPos->rank)
(map #(ChessPiece. (get-in chess-images [(keyword %2) -1]) 
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
               :mover core/move
               :scorer core/score-chess-naive
               :pref-depth 4
               :board-atom current-chessItems
               :game-starter start-chess!
               :hinter chess-best-move 
               :record-name "Clondie24.games.chess.ChessPiece" ;;fully qualified name
               :mappings board-mappings-chess
               :north-player-start  (starting-chessItems true)   ;opponent (black)
               :south-player-start  (starting-chessItems false)});human (white or yellow)
               
               
                   
;---------------------------------------------------------------------------------------------               

#_(def buffered-moves 
"Precalculate the logical moves of chess-pieces for better performance. Does not apply to pawn."           
(loop [k 0 m (hash-map)]
(if (= 64 k) m
(recur (inc k) 
 (assoc m k 
    (zipmap [:rook :knight :bishop :queen :king] 
     (map #(rank->moves (ChessPiece. nil (core/translate-position k board-mappings-chess) % 
               ((keyword %) (:rel-values details)) nil)) 
      ['rook 'knight 'bishop 'queen 'king])))))))

;(ut/data->string buffered-moves "performance.cheat") 
(def buffered-moves (ut/string->data "performance.cheat"))  ;it's faster to read them from file than recalculate       

(defn -main 
"Starts a graphical (swing) Chess game." 
[& args]  
(gui/show-gui! details)
#_(time (s/fake -1 (start-chess! false) 4) #_(println @s/mmm))
#_(time (do (s/fake -1 (start-chess! false) 4) (println @s/mmm))) 
)





         
               
