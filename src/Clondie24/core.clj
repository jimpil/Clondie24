(ns Clondie24.core
   (:use [clojure.pprint :only (pprint, print-table)]
         [clojure.inspector :only (inspect-table)])
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

(defn record-factory [recordname]
  (let [recordclass ^Class (resolve (symbol recordname))
        max-arg-count (apply max (map #(count (.getParameterTypes %))
                                      (.getConstructors recordclass)))
        args (map #(symbol (str "x" %)) (range (- max-arg-count 2)))]
    (eval `(fn [~@args] (new ~(symbol recordname) ~@args)))))
    
;(defn ChessPieceFactory [s] (record-factory "ChessPiece"))
;(defn CheckersPieceFactory [s] (record-factory "CheckersPiece"))    
    
;(def ^:const checkers-rel-values {:soldier 1 :prince 3}) ;only 2 types in checkers

;(def ^:const chess-ranks ) ;all the different chess piece-types 
;(def ^:const rel-values  '(9 5 3 3 1 100))  ;relative values according to wikipedia
;(def ^:const chess-rel-values (zipmap chess-ranks rel-values)) ;=> {:king 100, :pawn 1, :bishop 3, :knight 3, :rook 5, :queen 9}

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
(defn make-point 
^java.awt.Point [p]
(let [[x y] p]
(java.awt.Point. x y)))


(defn make-image 
"Returns a buffered-image from the specified file or nil if the file is not there.." 
[path-to-image]
(try 
  (javax.imageio.ImageIO/read (java.io.File. path-to-image))
(catch java.io.IOException e ;returning nil here is ok! 
  (println path-to-image "does not exist! Reverting to 'nil'..."))))

;Helper macro for creting pre-defined Colours
(defn make-color 
^java.awt.Color [^String predefined-name]
(.get (.getField (Class/forName "java.awt.Color") 
       (str predefined-name)) nil))


;RED is machine (north camp), YELLOW is human (south camp)            
(defn checkers-colors [c1 c2] 
(vec (map make-color (list c1 c2))))
;[(make-color c1)  
; (make-color c2)]) 


(declare make-piece, make-checker, make-chessItem, translate-position, current-chessItems, current-checkers, 
 chess checkers) ;will need all this 
                       

 




(def ^:dynamic black-direction -1)
(def ^:dynamic white-direction 1)

(def valid-checkers-positions    (range 32))
(def invalid-checkers-positions  (repeat 32 -1))

(def checkers-1d ;the checkers-board as a list. -1 represents invalid positions
(interleave invalid-checkers-positions 
            valid-checkers-positions))
                      

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
 

(defrecord CheckersPiece [^java.awt.Color color 
                          ^clojure.lang.PersistentVector position 
                           rank ^Integer value] 
 Piece 
 (update-position [this np]  (make-checker color np :rank rank))
 (die     [this] (vary-meta this assoc :dead true)) ;communicate death through meta-data 
 (promote [this] (make-checker color position :rank 'prince)) ; a checker is promoted to prince
 (getGridPosition [this] position)
 (getListPosition [this] (translate-position  (first  position) 
                                              (second position) (get (checkers) :mappings)))
 (getPoint [this] (make-point position))
 (getMoves [this] nil) ;TODO
 Object
 (toString [this] 
   (println "Checker (" rank ") at position:" (getListPosition this) " ->" (getGridPosition this))) )
 
(defrecord ChessPiece [^java.awt.Image image 
                       ^clojure.lang.PersistentVector position 
                        rank ^Integer value]
 Piece 
 (update-position [this np] (make-chessItem image position rank))
 (die [this]     (vary-meta this assoc :dead true)) ;communicate death through meta-data 
 (promote [this] (make-chessItem image position :rank 'queen)) ;a pawn is promoted to a queen
 (getGridPosition [this] position)
 (getListPosition [this] (translate-position (first  position) 
                                             (second position) (get (chess) :mappings)))
 (getPoint [this] (make-point position))
 (getMoves [this] nil) ;TODO 
 Object
 (toString [this] 
   (println "ChessItem (" rank ") at position:" (getListPosition this) " ->" (getGridPosition this))) )
   

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

#_(defn piece->point [p] ;needs to be in the gui namespace
 (let [[x y] (getGridPosition p)]
 (java.awt.Point. x y)))

(defn vector-of-doubles 
^clojure.lang.PersistentVector [v] 
(vec (map double v))) 



(defn starting-checkers
"Will construct a set of initial checkers (12). opponent? specifies the side of the board where the pieces should be placed (true for north false for south).   " 
[opponent?]
(let [[red yellow] (checkers-colors 'RED 'YELLOW)]                                   
(if opponent?  
(map #(make-checker red
      (translate-position % (get (checkers) :mappings)) :rank 'soldier) (range 12))
(map #(make-checker yellow 
      (translate-position % (get (checkers) :mappings)) :rank 'soldier) (range 20 32))      
)))

(def chessPos->rank 
(flatten ['rook 'knight 'bishop 'queen 'king 'bishop 'knight 'rook (repeat 8 'pawn)]))

(declare chess-images)
(defn starting-chessItems
"Will construct a set of initial checkers (12). opponent? specifies the side of the board where the pieces should be placed (true for north false for south)."
[opponent?]
(if opponent?  
(map #(make-chessItem (second (chess-images (keyword %2))) 
      (translate-position % (get (chess) :mappings)) :rank %2) (range 16) chessPos->rank)
(map #(make-chessItem (first (chess-images (keyword %2))) 
      (translate-position % (get (chess) :mappings)) :rank %2) (range 48 64) (reverse chessPos->rank))      
))


(defn chess 
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
                       
(defn checkers 
^clojure.lang.PersistentArrayMap []
              {:name 'checkers
               :players 2 
               :colors (checkers-colors 'RED 'YELLOW)
               :characteristics [:color :position :rank :value]  
               :board-size 32 
               :total-pieces 24
               :rel-values {:soldier 1 :prince 3}
               :board-atom  current-checkers
               :record-name "CheckersPiece"
               :mappings board-mappings-checkers
               :north-player-start  (starting-checkers true)
               :south-player-start  (starting-checkers false)})
               
(def chess-images 
(zipmap (get (chess) :ranks)[
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
                      
(defn make-piece 
"The central function for creating pieces. A piece is simply a record with 3 keys: colour, position [x,y] and rank (optional)."
 [game c pos &{:keys [rank]
               :or {rank 'zombie}}]
(with-meta 
   ((record-factory (game :record-name)) c pos rank 
   (get (game :rel-values) (keyword rank))) 
{:dead false}))   ;pieces are born 'alive'              
             

(def make-checker   (partial make-piece (checkers))) 
(def make-chessItem (partial make-piece (chess)))                                                 
                                  
                      
            
(defn starting-board [game] 
"Returns the initial board for a game with pieces on correct starting positions."
 ;opponent pieces come first, then 8 nils and our pieces last (conj appends at tail)
(let [p1 (game :north-player-start)
      p2 (game :south-player-start)
      vacant (- (game :board-size) (game :total-pieces))]
(flatten
  (conj p2 (conj (repeat vacant nil) p1)))))
  

(def current-checkers 
"This is list that keeps track of moving checkers. Is governed by an atom and it changes after every move. All changes are being logged to 'board-history'."
(add-watch 
(atom (vec (starting-board (checkers))) 
      ;:validator #(== 24 (count %))
      ) 
  :log (partial log-board board-history)))
              
(def current-chessItems
"This is list that keeps track of moving checkers. Is governed by an atom and it changes after every move. All changes are being logged to 'board-history'."
(add-watch 
(atom (vec (starting-board (chess))) 
      ;:validator #(== 32 (count %))
      ) 
  :log (partial log-board board-history))) 
  
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
               
 (defmacro no-nils "Filter out nils from a collection." 
  [c] 
 `(filter (complement nil?) ~c)) 
 
 (defmacro bury-dead "Will filter out dead-pieces from a collection"
  [c]
 `(filter (complement dead-piece?) ~c)) 
 
 (defn clean 
 ^clojure.lang.PersistentVector [c]
 (filter #(and (not (nil? %)) 
               (not (dead-piece? %)))
  c))           
 
 (defn vacant? 
 "Checks if a position [x, y] is vacant on the given board and mappings." 
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


;partially apply move with game and checker-mappings locked in as 1st & 2nd args     
(def move-checker   (partial move  (checkers)))
;partially apply move with game and chess-mappings locked in as 1st & 2nd args
(def move-chessItem (partial move  (chess)))  

 (defrecord ChessMove [^ChessPiece p
                       ^clojure.lang.PersistentVector start-pos 
                       ^clojure.lang.PersistentVector end-pos]
 MoveCommand
 (try-move [this] (move-chessItem p (getEndPos this)))
 (execute [this]  (reset! (get (chess) :board-atom) (try-move this))) ;STATE CHANGE!
 (undo    [this]  (move-chessItem p (getStartPos this)))
 (getMovingPiece [_] p)
 (getStartPos [_] (vector-of-doubles start-pos))
 (getEndPos   [_] (vector-of-doubles end-pos))
 Object
 (toString [this] 
   (println "Checkers-move originating from" (getStartPos this) "to" (getEndPos this))))
 
 (defrecord CheckersMove [^CheckersPiece p
                          ^clojure.lang.PersistentVector start-pos 
                          ^clojure.lang.PersistentVector end-pos]
 MoveCommand
 (try-move [this] (move-checker p (getEndPos this)))
 (execute [this]  (reset! (get (checkers) :board-atom) (try-move this)))  ;STATE CHANGE!
 (undo    [this]  (move-checker p (getStartPos this)))
 (getMovingPiece [_] p)
 (getStartPos [_] (vector-of-doubles start-pos))
 (getEndPos   [_] (vector-of-doubles end-pos))
 Object
 (toString [this] 
   (println "Chess-move originating from" (getStartPos this) "to" (getEndPos this))))
 

(defmacro print-board 
"Will print the detailed board with nils where vacant. Calls build-board without 'cleaning' it." 
[game] 
`(print-table (get (~game) :characteristics);the columns
       (deref (get (~game) :board-atom))))  ;the rows
       
(defmacro inspect-board [game] ;fails if the first element is nil
`(inspect-table ;(get (~game) :characteristics)  
         (deref (get (~game) :board-atom))))

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
  
  
