(ns Clondie24.checkers
     (:require [Clondie24.util :as ut] 
               [Clondie24.core :as core]
               [Clondie24.rules :as rul])
)
;----------------------------------------<SOURCE>--------------------------------------------------------------------
;----------------------------------------<CODE>----------------------------------------------------------------------

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


;RED is machine (north camp), YELLOW is human (south camp)            
(defn checkers-colors [c1 c2] 
(vec (map ut/make-color (list c1 c2))))

(declare make-checker, details)

(defn starting-checkers
"Will construct a set of initial checkers (12). opponent? specifies the side of the board where the pieces should be placed (true for north false for south).   " 
[opponent?]
(let [[red yellow] (checkers-colors 'RED 'YELLOW)]                                   
(if opponent?  
(map #(make-checker red
      (core/translate-position % board-mappings-checkers) :rank 'soldier) (range 12))
(map #(make-checker yellow 
      (core/translate-position % board-mappings-checkers) :rank 'soldier) (range 20 32))      
)))               

(def current-checkers 
"This is list that keeps track of moving checkers. Is governed by an atom and it changes after every move. All changes are being logged to 'board-history'. Starts off as nil but we can always get the initial arrangement from core."
(add-watch 
(atom nil #_(vec (core/starting-board (details)))) 
      ;:validator #(== 24 (count %)) 
  :log (partial core/log-board core/board-history)))
  
(defn details "Returns a map that describes the game of checkers."
^clojure.lang.PersistentArrayMap []
              {:name 'checkers
               :players 2 
               :colors (checkers-colors 'RED 'YELLOW)
               :characteristics [:color :position :rank :value]  
               :board-size 32 
               :total-pieces 24
               :rel-values {:soldier 1 :prince 3}
               :board-atom  current-checkers
               :record-name "Clondie24.checkers.CheckersPiece"
               :mappings board-mappings-checkers
               :north-player-start  (starting-checkers true)
               :south-player-start  (starting-checkers false)})

;partially apply move with game locked in as 1st
(def move-checker   (partial core/move  (details)))
(def make-checker   (partial core/make-piece (details)))                
(def vacant-checker-tile?  (partial core/vacant? board-mappings-checkers))  
                

(defrecord CheckersPiece [^java.awt.Color color 
                          ^clojure.lang.PersistentVector position 
                           rank ^Integer value] 
 core/Piece 
 (update-position [this np]  (make-checker color np :rank rank))
 (die     [this] (vary-meta this assoc :dead true)) ;communicate death through meta-data 
 (promote [this] (make-checker color position :rank 'prince)) ; a checker is promoted to prince
 (getGridPosition [this] position)
 (getListPosition [this] (core/translate-position  (first  position) 
                                                   (second position) board-mappings-checkers))
 (getPoint [this] (ut/make-point position))
 (getRank [this] rank)
 (getMoves [this] nil) ;TODO
 Object
 (toString [this] 
   (println "Checker (" rank ") at position:" (.getListPosition this) " ->" (.getGridPosition this))) )
 
(defrecord CheckersMove [ ^CheckersPiece p
                          ^clojure.lang.PersistentVector start-pos 
                          ^clojure.lang.PersistentVector end-pos]
 core/MoveCommand
 (try-move [this] (move-checker p (core/getEndPos this)))
 (execute [this]  (reset! (get (details) :board-atom) (core/try-move this)))  ;STATE CHANGE!
 (undo    [this]  (move-checker p (core/getStartPos this)))
 (getMovingPiece [_] p)
 (getStartPos [_] (ut/vector-of-doubles start-pos))
 (getEndPos   [_] (ut/vector-of-doubles end-pos))
 Object
 (toString [this] 
   (println "Chess-move originating from" (core/getStartPos this) "to" (core/getEndPos this)))) 
   
(defn to-2d [b] ; the checkers or chess board as 8 rows of 8 columns
(map vec        ;apply 'reverse' to every second item in 1d-board
(map #(%1 %2) (cycle [identity reverse]) (partition 8 b)))) 
;-----------------------------------------------------------------------------------
(def ^:dynamic red-direction 1)
(def ^:dynamic yellow-direction -1)
(def valid-checkers-positions    (range 32))
(def invalid-checkers-positions  (repeat 32 -1))

(def checkers-1d ;the checkers-board as a list. -1 represents invalid positions
(interleave invalid-checkers-positions 
            valid-checkers-positions))
               