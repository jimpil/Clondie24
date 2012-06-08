(ns Clondie24.core
   (:use [clojure.pprint])

)


(def ^:const board-mappings-checkers 
"A vector of vectors. Outer vector represents the 32 (serial) positions checkers can position themselves on. 
Each inner vector represents the coordinates of that position on the 8x8 grid."
[ ;;
[1.0 0.0] [3.0 0.0] [5.0 0.0] [7.0 0.0] 
[6.0 1.0] [4.0 1.0] [2.0 1.0] [0.0 1.0]
[1.0 2.0] [3.0 2.0] [5.0 2.0] [7.0 2.0] 
[6.0 3.0] [4.0 3.0] [2.0 3.0] [0.0 3.0]
[1.0 4.0] [3.0 4.0] [5.0 4.0] [7.0 4.0]
[6.0 5.0] [4.0 5.0] [2.0 5.0] [0.0 5.0]
[1.0 6.0] [3.0 6.0] [5.0 6.0] [7.0 6.0]
[6.0 7.0] [4.0 7.0] [2.0 7.0] [0.0 7.0]
])

(declare make-piece, make-checker, make-chessItem, translate-position) ;will need this fn 

;Helper macro for creting Points
(defmacro make-point [p]
`(java.awt.Point. (first ~p) (second ~p)))

;Helper macro for creting pre-defined Colours
(defmacro make-color [predefined-name]
`(.get (.getField (Class/forName "java.awt.Color") (str ~predefined-name)) nil))

(def ^:const board-mappings-chess nil) ;TODO

(def ^:dynamic black-direction -1)
(def ^:dynamic white-direction 1)

(def valid-checkers-positions    (range 32))
(def invalid-checkers-positions  (repeat 32 -1))

(def checkers-1d ;the checkers-board as a list. -1 represents invalid positions
(interleave invalid-checkers-positions 
            valid-checkers-positions))

;RED is machine (north camp), YELLOW is human (south camp)            
(def checkers-colors [(make-color 'RED)  
                      (make-color 'YELLOW)]) 

;BLACK is machine (north camp), WHITE is human (south camp)                       
(def chess-colors [(make-color 'BLACK)  
                   (make-color 'WHITE)])                       
 
(declare all-chessItems) ;TODO                                                 
            
(defn starting-checkers
"Will construct a set of initial pieces (12 for checkers or 16 for chess). opponent? specifies the side of the board where the pieces should be placed (true for north false for south). Optional arguments include board mappings and chess? (they default to checkers-mappings and false). If one is changed the other one should be changed as well.  " 
[opponent? &{:keys [mappings]
             :or {mappings board-mappings-checkers}}]                                   
(if opponent? 
   (for [p (range 0 12)]  
        (make-checker (first checkers-colors)  
        (make-point (translate-position p mappings)) :rank 'soldier))
   (for [p (range 20 32)] 
        (make-checker (second checkers-colors) 
        (make-point (translate-position p mappings)) :rank 'soldier))
))


(def all-checkers
(concat (starting-checkers true) 
        (starting-checkers false)))

(defn starting-board-checkers 
"Creates the initial board for checkers with correct starting positions."
[] ;opponent pieces come first, then 8 nils and our pieces last (conj appends at tail)
(flatten
  (conj (starting-checkers false) 
  (conj (repeat 8 nil) (starting-checkers true)))))

(defn dead-piece? [p]
((meta p) :dead))


(defn build-board [chess?]
(loop [nb (vec (if chess? (repeat 64 nil) (repeat 32 nil))) ;building a new board after each move
      p (if chess? all-chessItems all-checkers)]
(if (empty? p) (seq nb)
(recur (assoc nb (.getListPosition (first p))    ;the piece's position
(if (dead-piece? (first p))  nil ;if the piece is dead stick nil
                (first p)))  ; else stick the piece in
               (rest p)))))  ;carry on recursing
                    

(def chess-1d (range 64)) ;;the chess board as a list

(defn to-2d [b] ; the checkers or chess board as 8 rows of 8 columns
(map vec        ;apply 'reverse' to every second item in 1d-board
(map #(%1 %2) (cycle [identity reverse]) (partition 8 b))))

(def checkers-rel-values {:soldier 1 :prince 3}) ;only 2 types in checkers

(def chess-ranks '(:queen :rook :knight :bishop :pawn :king)) ;all the different chess piece-types 
(def rel-values  '(9 5 3 3 1 100))  ;relative values according to wikipedia
(def chess-rel-values (zipmap chess-ranks rel-values)) ;=> {:king 100, :pawn 1, :bishop 3, :knight 3, :rook 5, :queen 9}

(defn translate-position
"Translates a position from 1d to 2d and vice-versa. 
Mappings should be either 'checkers-board-mappings' or 'chess-board-mappings'." 
([i mappings] {:post [(not (nil? %))]}   ;will translate from 1d to 2d
(get mappings i)) 
([x y mappings] {:post [(not (== % -1))]} ;will translate from 2d to 1d
(.indexOf mappings (vector x y))
)) 

(defmacro in? ;handy function to test if some element exists in some collection 
 "Returns true if colle contains elm, false otherwise."
 [colle elm]  
`(if (some #{~elm} ~colle) true false))  

(defprotocol Piece "The Piece abstraction."
 (update-position [this new-position])
 (getGridPosition [this])
 (getListPosition [this])
 (getPoint [this])
 (die [this])
 (promote [this])
 (getMoves [this]) ;pretends there is only ONE piece on the board - will need filtering for validity later
 (toString [this])) 

(defrecord CheckersPiece [^java.awt.Color color 
                          ^java.awt.Point position 
                           rank] 
 Piece 
 (update-position [this np] ;mutable state inside Point!
   (. position setLocation  ;can accept ints or doubles
     (first np) (second np))) ;np should be [x, y]
 (die     [this] (vary-meta this assoc :dead true)) ;communicate death through meta-data 
 (promote [this] (make-checker color position :rank 'prince)) ; a checker is promoted to prince
 (getGridPosition [this] (vector (.getX position) (.getY position)))
 (getListPosition [this] (translate-position (.getX position) (.getY position) board-mappings-checkers))
 (getPoint [this] position)
 (getMoves [this] nil) ;TODO
 (toString [this] 
   (println rank "at position:" (.getListPosition this) " ->" (.getGridPosition this))) )
 
(defrecord ChessPiece [^java.awt.Image image 
                       ^java.awt.Point position 
                        rank]
 Piece 
 (update-position [this np] ;mutable state inside Point!
   (. position setLocation  ;can accept ints or doubles
     (first np) (second np))) ;np should be [x, y]
 (die [this]     (vary-meta this assoc :dead true)) ;communicate death through meta-data 
 (promote [this] (make-chessItem image position :chess? true :rank 'queen)) ;a pawn is promoted to a queen
 (getGridPosition [this] (vector (.getX position) (.getY position)))
 (getListPosition [this] (translate-position (.getX position) (.getY position) board-mappings-chess))
 (getPoint [this] position)
 (getMoves [this] nil) ;TODO 
 (toString [this] 
   (println rank "at position:" (.getListPosition this) " ->" (.getGridPosition this))) )


(defn make-piece 
"The central function for creating pieces. A piece is simply a record with 3 keys: colour, position [x,y] and rank (optional)."
 [chess? c p &{:keys [rank]
               :or {rank 'zombie}}]
(if chess?
(with-meta (ChessPiece. c p rank)    {:dead false})  ;pieces are born 'alive'
(with-meta (CheckersPiece. c p rank) {:dead false})))

(def make-checker   (partial make-piece false))
(def make-chessItem (partial make-piece true))

;EXAMPLEs:
 ;(make-checker java.awt.Color/BLUE (make-point [1 5]) :rank 'soldier)
 ;(make-chessItem (java.awt.Image. bishop-icon.png) (make-point[2 3]) :chess? true :rank 'bishop)
 ;(make-checker java.awt.Color/WHITE (make-point [0 0])) ;rank will default to :zombie

(defn move 
"The function responsible for moving Pieces. Each piece knows how to move itself." 
[chess? mappings p coords] 
{:pre [(== 2 (count coords))]}   ;safety comes first
(if (in? mappings (vec (map double coords)))
(do (. p update-position coords) ;coords should be of the form [x, y]
    (build-board chess?)) 
(throw (Exception. (str coords " is NOT a valid position!")))))
;to be called only on the very first move of the game
;([mappings p coords] (move all-checkers mappings p coords))
;)

;partially apply move with all-checkers and checker-mappings locked in as 1st & 2nd args     
(def move-checker   (partial move false board-mappings-checkers))
;partially apply move with all-chessItems and chess-mappings locked in as 1st & 2nd args
(def move-chessItem (partial move true board-mappings-chess))  

(defn print-board [] 
(for [letter "ABCDEFGH" ;strings are seqable
      number (range 1 9)]
(format "%c%d" letter number)))


(defn -main ;lein generated
  "I don't do a whole lot."
  [& args]
  (println "Hello, Clondie24!"))
