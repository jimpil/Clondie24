(ns Clondie24.games.chess 
    (:require [Clondie24.lib.util :as ut] 
              [Clondie24.lib.core :as core]
              [Clondie24.lib.search :as s]
              [Clondie24.lib.rules :as rul]
              [Clondie24.lib.gui :as gui]
              [enclog.nnets :as ai]
              [enclog.training :as evol]
              [enclog.normalization :as norm] :verbose :reload)
    (:import  [encog_java.customGA CustomNeuralGeneticAlgorithm 
                                   CustomGeneticScoreAdapter Referee])
)

;----------------------------------------<SOURCE>--------------------------------------------------------------------
;----------------------------------------<CODE>----------------------------------------------------------------------

(def board-mappings-chess core/mappings-8x8)
(def previous-move (atom nil))  ;will need this for en-passant
(def state-dependent-moves (atom {:castling nil 
                                  :en-passant nil})) 
(defrecord Player [brain ^int direction searcher])
                                 

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

(definline chess-best-move [dir b n scorer] 
`(s/go ~dir ~b ~n ~scorer))

(defn chess-rand-move [dir b _ _] ;;need same arity as 'chess-best-move'
(let [all-moves (into [] (core/team-moves b dir core/move true))]
{:move (get all-moves (rand-int (count all-moves)))})) 

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
(let [[x y] (:position p)            ;[[x y] (:position p) 
      r (keyword (:rank p))
      d (:direction p)]      
((r chess-moves) x y))) ;will return a fn which is called with current x and y 
;(if (= (class p) (Class/forName "Clondie24.games.chess.ChessPiece2"))
;(ut/Point->Vec pos) pos))))  
                                              

(defn start-chess! [fast?] 
"Start a chess-game. Returns the starting-board."
 (core/clear-history!) ;empty board-history
    (deliver s/curr-game details)
    (reset! current-chessItems
  (if fast? (into-array  (core/starting-board details))
                         (core/starting-board details)))
  );mandatory before game starts 
    
(definline jit-referee ;just-in-time referee
"Inspects the board for missing kings. If no team is missing its king returns nil (no winner),
 otherwise returns the direction of the team which has a king still standing (winner)." 
 [b]
 `(let [kings# (filter #(= (:rank %) 'king) ~b)]
   (if (= 2 (count kings#)) nil ;;no winners
     (:direction (first kings#))))) ;;return the direction of the king still standing 
     
(definline gui-referee [b] ;referee a-la checkmate for the gui
`(let [t1-moves# (into [] (core/team-moves ~b  1 core/move true))
       t2-moves# (into [] (core/team-moves ~b -1 core/move true))]
 (cond 
    (empty? t1-moves#) "Yellow wins!"  
    (empty? t2-moves#) "Black wins!"   
 :else nil)))         ;;no winner    
               
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
                            (core/exposes? (core/dest->Move b this % (:mover details)) (when with-precious? 'king)))    
                  (if (= rank 'pawn) (((keyword rank) chess-moves)  b x y direction)                               
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
                      
(def brain (ai/network (ai/neural-pattern :feed-forward) 
                        :activation :sigmoid
                        :input 64 ;the entire board for input
                        :output 1 ;the score
                        :hidden [80 40 10])) ; 3 hidden layers                      
                      
(def details "The map that describes the game of chess."
              {:name 'Chess
               :players 2 
               :chunking 2
               :images chess-images
               :characteristics [:image :position :rank :value :direction]      
               :board-size 64
               :arena-size [421 :by 506]
               :tile-size 50
               :alternating-colours [(ut/hex->color '0xffdead) ;funny colour name!
                                     (ut/hsb->color 0.931 0.863 0.545)] 
               :total-pieces 32
               :rel-values rel-values
               :obligatory-move nil
               :team-moves core/team-moves
               :mover core/move
               :referee-gui gui-referee
               :referee-jit jit-referee
               :naive-scorer core/score-chess-naive
               :pref-depth 4
               :board-atom current-chessItems
               :game-starter start-chess!
               :hinter chess-best-move 
               :record-name "Clondie24.games.chess.ChessPiece" ;;fully qualified name
               :mappings board-mappings-chess
               :north-player-start  (starting-chessItems true)   ;opponent (black)
               :south-player-start  (starting-chessItems false)});human (white or yellow)
               
                      
(defmacro definvokable
  [type fields & deftype-tail]
  (let [f        (fields 0)
        args     (repeatedly 20 gensym)
        arity    (fn [n]
                   (let [args (take n args)]
                     `(invoke [this# ~@args] ((. this# ~f) ~@args))))
        vararg   `(invoke [this# ~@args more#]
                    (apply (. this# ~f) ~@args more#))
        apply-to `(applyTo [this# args#] (apply (. this# ~f) args#))]
    `(deftype ~type
       ~fields
       clojure.lang.IFn
       ~@(map arity (range (inc 20)))
       ~vararg
       ~apply-to
       ~@deftype-tail))) 
       
;(definvokable brain )                                                           
;---------------------------------------------------------------------------------------------               

(def buffered-moves 
"Precalculate the logical moves of chess-pieces for better performance. Does not apply to pawn."           
(loop [k 0 m (hash-map)]
(if (= 64 k) m
(recur (inc k) 
 (assoc m k 
    (zipmap [:rook :knight :bishop :queen :king] 
     (map #(rank->moves (ChessPiece. nil (core/translate-position k board-mappings-chess) % 
               ((keyword %) (:rel-values details)) nil)) 
      ['rook 'knight 'bishop 'queen 'king])))))))
      
      
(defn neural-input "Returns 64 inputs for the neural net." 
[b dir fields?]
((if fields? norm/input identity) 
  (for [t b] 
  (if (nil? t) 0 
   (* dir (:direction t) (:value t)))))) ;or maybe :direction instead of :value? 
  
(definline neural-output "Creates output-field based on this InputField." 
[inputs] 
`(norm/output ~inputs))

(defn normalize-fields [ins outs]
((norm/prepare :range [ins] [outs]) false 
  (norm/target-storage :norm-array [64 nil])))          
  
(definline anormalise "Deals directly with seqs (input) and arrays (output)." 
[ins]
`(norm/prepare :array-range nil nil :raw-seq ~ins))  

(defn tournament
"Starts a tournament between the 2 players (p1, p2). If there is no winner, returns the entire history (vector) of 
 the tournament after 100 moves. If there is a winner, a 2d vector will be returned containing both the history(1st item) 
 and the winner (2nd item)." 
[sb depth p1 p2 & {:keys [limit]
                   :or   {limit 100}}]
(reduce 
 (fn [history player] 
  (let [cb (peek history) 
        win-dir (jit-referee cb)]
    (if win-dir (reduced (vector history (if (= win-dir (:direction p1)) p1 p2)))
    (conj history (->> player
                      :brain
                      ((:searcher player) (:direction player) cb depth) 
                      :move
                      core/try-move))))) 
 [sb] (take limit (cycle [p1 p2])))) ;;100 moves each should be enough
  
 
(defn fast-tournament 
"Same as tournament but without keeping history. If there is a winner, returns the winning direction
otherwise returns the last board. Intended to be used with genetic training." 
[sb d p1 p2 & {:keys [limit]
               :or   {limit 100}}]
(reduce 
  (fn [board player]
    (if-let [win-dir (jit-referee board)] (reduced (if (= win-dir (:direction p1)) p1 p2))
    (->> player
          :brain
          ((:searcher player) (:direction player) board d)
          :move
           core/try-move))) 
 sb (take limit (cycle [p1 p2])))) ;;50 moves each should be enough
  
(defn ga-fitness*
"Scores p1 after competing with p2 starting with board b." 
([b d fast? p1 p2]
(let [winner ((if fast? fast-tournament tournament) b d p1 p2)]
(condp #(= winner %)
       (:direction p1)  1 ;reward p1 with 1 point
       (:direction p2) -2 ;penalise p1 with -2 points
:else 0))))               ;give 0 points - noone won
         

(defn ga-fitness [] 
(partial ga-fitness* (start-chess! false) (:pref-depth details) true)) 

(defn ga 
[brain pop-size & {:keys [randomizer to-mate to-mutate thread-no]
                   :or {randomizer (evol/randomizer :nguyen-widrow)
                        to-mate   0.2
                        to-mutate 0.1
                        thread-no 10}}]
(doto 
 (CustomNeuralGeneticAlgorithm. brain randomizer  (Referee.) pop-size to-mate to-mutate)
 (.setThreadCount thread-no)))

(defn neural-player 
"Constructs a Player object given a brain b (a neural-net) and a direction dir." 
 [^org.encog.neural.networks.BasicNetwork brain dir]
(Player. 
   (fn [leaf _] ;;ignore 2nd arg - we already have direction
     (let [normals (anormalise (neural-input leaf dir false))
           output  (double-array 1)] 
     (.compute brain normals output)
     (aget output 0)))
 dir chess-best-move)) 
     
(defn random-player [dir]
(Player. nil dir chess-rand-move)) 

(defn naive-player [dir]
(Player. core/score-chess-naive dir chess-best-move))        

;(ut/data->string buffered-moves "performance.cheat") 
(def buffered-moves (ut/string->data "performance.cheat"))  ;it's faster to read them from file than recalculate       

(defn -main 
"Starts a graphical (swing) Chess game." 
[& args]
#_(ga brain 10)
#_(fast-tournament (start-chess! false) 4 
  (neural-player brain -1)
  ;(random-player -1) 
  (naive-player 1) :limit 20)
#_(let [ni (neural-input (start-chess! false) -1 true)
      no (neural-output ni)]  
(normalize-fields ni no))
;(anormalise (neural-input (start-chess! false) -1 false)) ;;quick and easy way is preferred
(gui/show-gui! details)
)





         
               
