(ns Clondie24.core
   (:use [clojure.pprint])

)

(def valid-checkers-positions (range 32))
(def invalid-checkers-positions (take 32 (repeat -1)))

(def checkers-1d ;the checkers-board as a list. -1 represents invalid positions
(interleave invalid-checkers-positions 
            valid-checkers-positions))

(def chess-1d (range 64)) ;;the chess board as a list

(defn to-2d [b] ; the checkers or chess board as 8 rows of 8 columns
(map vec        ;apply 'reverse' to every second item in 1d-board
(map #(%1 %2) (cycle [identity reverse]) (partition 8 b))))

(def checkers-rel-values {:soldier 1 :prince 3}) ;only 2 types in checkers

(def chess-ranks '(:queen :rook :knight :bishop :pawn :king)) ;all the different chess piece-types 
(def rel-values  '(9 5 3 3 1 500))  ;relative values according to wikipedia
(def chess-rel-values (zipmap chess-ranks rel-values)) ;=> {:king 500, :pawn 1, :bishop 3, :knight 3, :rook 5, :queen 9}

 
(declare make-piece) ;will need this fn in here (when promoting)
(defprotocol Piece "The Piece abstraction. "
 (update-position [this new-position])
 (getPosition [this])
 (die [this])
 (promote [this])
 (getMoves [this]) ;pretends there is only ONE piece on the board - will need filtering for validity later
 (toString [this])) 

(defrecord CheckersPiece [^java.awt.Color color 
                          ^java.awt.Point position 
                           rank] 
 Piece 
 (update-position [this np] ;mutable state inside Point!
   (.setLocation position ;can accept ints or doubles
     (first np) (second np))) ;np should be [x, y]
 (die [this]     (vary-meta this assoc :dead true)) ;communicate death through meta-data 
 (promote [this] (make-piece false color position :rank 'prince)) ; a checker is promoted to prince
 (getPosition [this] (vector (.getX position) (.getY position))) 
 (getMoves [this] nil) ;TODO
 (toString [this] 
   (println (str color) "piece at" (str (.getLocation position)))))
 
(defrecord ChessPiece [^java.awt.Image image 
                       ^java.awt.Point position 
                        rank]
 Piece 
 (update-position [this np] ;mutable state inside Point!
   (.setLocation position ;can accept ints or doubles
     (first np) (second np))) ;np should be [x, y]
 (die [this]     (vary-meta this assoc :dead true)) ;communicate death through meta-data 
 (promote [this] (make-piece true image position :rank 'queen)) ;a pawn is promoted to a queen
 (getPosition [this] (vector (.getX position) (.getY position)))
 (getMoves [this] nil) ;TODO 
 (toString [this] 
   (println rank "at position" (str (.getLocation position)))))

;Helper macro for creting Points
(defmacro make-point [x y]
`(new java.awt.Point ~x ~y))

;Helper macro for creting predefined Colours
(defmacro make-color [predefined-name]
`(.get (.getField (Class/forName "java.awt.Color") (str ~predefined-name)) nil))

(defn make-piece 
"The central function for creating pieces. A piece is simply a record with 3 keys: colour, position [x,y] and rank (optional)."
 [chess? c p &{:keys [rank]
               :or {rank 'zombie}}]
(if chess?
(with-meta (ChessPiece. c p rank)    {:dead false})  ;pieces are born 'alive'
(with-meta (CheckersPiece. c p rank) {:dead false})))

;EXAMPLEs:
 ;(make-piece false java.awt.Color/BLUE (make-point 1 5) :rank 'soldier)
 ;(make-piece true java.awt.Color/WHITE (make-point 2 3) :rank 'bishop)
 ;(make-piece false java.awt.Color/WHITE (make-point 0 0)) ;rank will default to :zombie

(defn move 
"The function responsible for moving Pieces. Each piece knows how to move itself." 
[p coords] 
{:pre [(== 2 (count coords))]} ;safety comes first
(. p update-position coords))  ;coords should be of the form [x, y]


(defn print-board [] 
(for [letter "ABCDEFGH" ;strings are seqable
      number (range 1 9)]
(format "%c%d" letter number)))


(defn -main ;lein generated
  "I don't do a whole lot."
  [& args]
  (println "Hello, Clondie24!"))
