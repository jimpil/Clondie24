(ns Clondie24.lib.core
   (:require [Clondie24.lib.util :as ut]
             [clojure.core.reducers :as r]
             [enclog.training :as evo]
             [enclog.normalization :refer [prepare input output target-storage]]
   )
   (:import  [encog_java.customGA CustomNeuralGeneticAlgorithm Referee] 
             [org.encog.ml MLRegression]))
;----------------------------------------<SOURCE>--------------------------------------------------------------------
;----------------------------------------<CODE>----------------------------------------------------------------------   
(set! *unchecked-math* true)
(set! *warn-on-reflection* true)

(def ^:const mappings-8x8
"A vector of vectors. Outer vector represents the 64 (serial) positions chess-items can position themselves on. 
 Each inner vector represents the coordinates of that position on the 8x8 grid."
;(mapv #(apply vector-of :int %)
[[0 0] [1 0] [2 0] [3 0] [4 0] [5 0] [6 0] [7 0]
 [0 1] [1 1] [2 1] [3 1] [4 1] [5 1] [6 1] [7 1]
 [0 2] [1 2] [2 2] [3 2] [4 2] [5 2] [6 2] [7 2]
 [0 3] [1 3] [2 3] [3 3] [4 3] [5 3] [6 3] [7 3]
 [0 4] [1 4] [2 4] [3 4] [4 4] [5 4] [6 4] [7 4]
 [0 5] [1 5] [2 5] [3 5] [4 5] [5 5] [6 5] [7 5]
 [0 6] [1 6] [2 6] [3 6] [4 6] [5 6] [6 6] [7 6]
 [0 7] [1 7] [2 7] [3 7] [4 7] [5 7] [6 7] [7 7]]);)
 
 
 (def ^:const mappings-3x3
"A vector of vectors. Outer vector represents the 9 (serial) positions tic-tac-toe-items can position themselves on. 
 Each inner vector represents the coordinates of that position on the 3x3 grid."
;(mapv #(apply vector-of :int %)
 [[0 0] [1 0] [2 0]
  [0 1] [1 1] [2 1]
  [0 2] [1 2] [2 2]]);)

(def board-history 
"Log of the state of a game." 
(atom []))

(defn log-board 
"The logging function for the board ref. Will conj every new board-state into a vector." 
[dest k r old n] 
 (when (not= n old)  
  (swap! dest conj n)))
  
(defrecord Player [brain ^long direction searcher])  

(defprotocol Piece "The Piece abstraction."
 (update-position [this new-position])
 (mutate-position [this new-position]) ;;optional in case you need mutation
 (getGridPosition [this])
 (getListPosition [this])
 (getPoint [this]) ;for gui?
 (die [this])
 (promote [this np])
 (getMoves [this board with-precious-piece?])) 
 
 (defprotocol Movable 
 "The Command design pattern in action (allows us to do/undo moves)."
 (try-move [this]) 
 (undo [this]) ;;optional in case you need mutation
 (getOrigin [this]))
 
(extend-type nil
Movable
(try-move [_] nil)) 
 
(defn- translate
"Translates a position from 1d to 2d and vice-versa. 
Mappings should be either 'checkers-board-mappings' or 'chess-board-mappings'." 
([^long i mappings] ;{:post [(not (nil? %))]}   
  (let [grid-loc (get mappings i)] ;will translate from 1d to 2d
    (if (nil? grid-loc)  ;not found 
     (throw (IllegalStateException. (str "NOT a valid list-location:" i))) 
     grid-loc) ))
([x y ^clojure.lang.PersistentVector mappings] ;{:post [(not= % -1)]} 
  (let [list-loc (.indexOf mappings [x y])] ;will translate from 2d to 1d
    (if (= list-loc -1) 
     (throw (IllegalStateException. (str "NOT a valid grid-location: [" x ", " y "]"))) 
     list-loc))))

(def translate-position (memoize translate))
                    
(defn piece 
"Helper function for creating pieces. A piece is simply a record with 5 keys. Better not use this directly (slow)!"
 [game c pos rank direction] ;&{:keys [rank direction ]
               ;:or {rank 'zombie direction 1 r-value 1}}]
 ((ut/record-factory-aux (:record-name game)) c pos rank 
  ((keyword rank) (:rel-values game)) direction  
  {:alive true}   ;pieces are born 'alive'             
           nil))  ;no extra fields                          
                               
(defn starting-board [game] 
"Returns the initial board for a game with pieces on correct starting positions for the particular game."
(let [p1 (:north-player-start game)
      p2 (:south-player-start game)
      tps (:total-pieces game)
      vacant (- (:board-size game)  tps)]
(if (= 0 tps) 
  (vec (repeat vacant nil))
  (vec (flatten (conj p2 (conj (repeat vacant nil) p1)))) )))  ;;(into p2 (into (repeat vacant nil) p1))  
     
(definline gather-team "Filters all the pieces with same direction dir on this board b." 
[b dir]
`(filter
   #(= ~dir (:direction %)) ~b)) ;all the team-mates (with same direction)
   
 
(definline team-moves 
"Returns all the moves (a reducer) for the team with direction 'dir' on this board b." 
[b dir exposes-check?]
`(r/mapcat  
   (fn [p#] 
     (getMoves p# ~b ~exposes-check?)) 
  (gather-team ~b ~dir)))
  
(definline full-board? [b]
 `(not-any? nil? ~b))         
  
(defn vacant? 
 "Checks if a position [x, y] is vacant on the given board and mappings." 
  [m b [x y :as pos]]
 (nil? 
  (get b (translate-position x y m)))) 
  
(def occupied? (complement vacant?))    
 
(definline alive? [p]
`(:alive (meta ~p)))

(defn game-over? [referee & args]
(apply referee args))

(defn empty-board 
"Returns an empty board for the game provided - all nils." 
[game] 
(vec (repeat (:board-size game) nil)))

(definline populate-board 
"Builds a new board with nils where dead pieces." 
[board]     
`(into [] (r/map #(if (alive? %) % nil) ~board)))

(defn rand-move [piece b precious?]
 {:move (rand-nth (getMoves piece b precious?))})
 
(defn rand-team-move [dir b]
(let [all-moves (into [] (team-moves b dir true))] ;;looking for safe moves
 {:move (rand-nth all-moves)}))          

(defn move 
"A generic function for moving Pieces. Each piece knows where it can move. 
 Returns the resulting board without side-effects. " 
[board p coords] 
;{:pre [(satisfies? Piece p)]}  ;safety comes first
;(if  (some #{coords} (:mappings game-map)) ;check that the position exists on the grid
(let [newPiece (update-position p coords) ;the new piece as a result of moving 
      old-pos  (getListPosition p)
      new-pos  (getListPosition newPiece)]            
(-> board 
     ;(transient)  
     (assoc old-pos nil 
            new-pos newPiece) 
     ;(assoc! )
     ;(persistent!)
     #_(populate-board)))) ;replace dead-pieces with nils
#_(throw (IllegalStateException. (str coords " is NOT a valid position according to the mappings provided!")))

#_(defn amove 
"Same as 'move' but fast (expects a mutable array for board). Returns the mutated board." 
[board p coords]
(let [old-pos  (getListPosition p)
      mutPiece (update-position p coords) ;the mutated piece 
      new-pos  (getListPosition mutPiece)] 
     (aset  board old-pos nil) ;^"[LClondie24.games.chess.ChessPiece2;"
     (aset  board new-pos mutPiece) board))

(defrecord Move [p mover end-pos]
 Movable
 (try-move [this]  (with-meta (mover p end-pos) {:caused-by this})) ;;the board returned was caused by this move
 (getOrigin [this] (if (map? p) (:position p)
                                (:position (first p))))
 Object
 (toString [this] 
   (println "#Move {:from" (:position p) ":to" end-pos)))   

(defn dest->Move 
 "Constructor for creating moves from destinations. Prefer this to the direct constructor. 
 It wouldn't make sense to pass more than 1 mover-fns." 
^Move [b p dest mover]  
 (Move. p #((or mover move) b %1 %2) dest))

(defn execute! [^Move m batom]
;(when (not= (:end-pos m) (-> m :p :position))
 (reset! batom (try-move m)))

(defn unchunk
  "Returns a fully unchunked lazy sequence. Might need it for massive boards." 
  [s] 
  (when (seq s)
    (lazy-seq
      (cons (first s)
            (unchunk (next s))))))
 
(definline threatens? "Returns true if p2 is threatened by p1 on board b. This is the only time that we call getMoves with a falsy last arg." 
[p2 p1 b]
`(some #{(:position ~p2)} 
      (map :end-pos (getMoves ~p1 ~b false)))) 

(defn undo! []
  (try (swap! board-history pop)
  (catch Exception e @board-history))) ;popping an empty stack throws an error (just return the empty one)    
     

(defn clear-history! []
 (swap! board-history empty)) 
   
 
(definline bury-dead [c]
 `(filter alive? ~c))  

(definline collides? 
"Returns true if the move collides with any friendly pieces. 
 The move will be walked step by step by the walker fn."
[move walker b m] ;last 2 can be false, nil
`(let [ppp#  (:end-pos ~move)
       multi-move?# (if (sequential? (first ppp#)) true false)
       [epx# epy# :as ep#]  (if multi-move?# (first ppp#) ppp#)
       dir# (if multi-move?# (get-in ~move [:p 0 :direction])
                             (get-in ~move [:p :direction]))]                                        
(loop [[imm-px# imm-py# :as imm-p#] (if (nil? ~walker) ep#  ;;if walker is nil make one big step to the end       
                                        (~walker (getOrigin ~move)))]
(cond  
  (=  imm-p# ep#) ;if reached destination there is potential for attack
       (if-not (= dir# (:direction (get ~b (translate-position epx# epy# ~m)))) false true)   
  (not (nil? (get ~b (translate-position imm-px# imm-py# ~m)))) true
:else (recur (~walker imm-p#))))))

#_(defn acollides? "Same as 'collides?' but deals with an array as b - not a vector."
[[sx sy] [ex ey] walker b m dir]
(loop [[imm-x imm-y] (if (nil? walker) [ex ey] (walker [sx sy]))] ;if walker is nil make one big step to the end       
(cond  
  (= [ex ey] [imm-x imm-y]) ;if reached destination 
       (if (not= dir (:direction (aget b (translate-position ex ey m)))) false true)    
  (not (nil? (aget  b (translate-position imm-x imm-y m)))) true
:else (recur (walker [imm-x imm-y])))))

(definline exposes? [move precious]
`(if-not ~precious false ;skip everything
   (let [next-b# (try-move ~move)
         ipiece#  (first (:p ~move)) ;;assuming multi-move temporarily
         dir#     (if (map? ipiece#) (:direction ipiece#) 
                    (get-in ~move [:p :direction]))
         def-prec#  (some #(when (and (= ~precious (:rank %)) 
                                       (= dir# (:direction %))) %) next-b#)]
   (some #(threatens? def-prec# % next-b#) 
      (gather-team next-b# (- dir#))))))


(defn score-chess-naive ^double [b dir]
 (let [hm (gather-team b dir) ;fixed bug
       aw (gather-team b (- dir))]
 (- (r/reduce + (r/map :value hm)) 
    (r/reduce + (r/map :value aw)))))    

(definline remove-illegal [pred ms]
`(into [] (r/remove ~pred ~ms)))

;(comment

(definline anormalise "Deals directly with seqs (input) and arrays (output)." 
[ins]
`(prepare nil nil nil :how :array-range :raw-seq ~ins))

(defn normalize-fields [ins outs game-map]
 (prepare ins outs (target-storage :norm-array [(:board-size game-map) nil])))

(defn neural-input "Returns appropriate number of inputs for the neural-net according to how big the board is." 
[b dir fields?]
((if fields? input identity) 
  (for [t b] 
  (if (nil? t) 0 
   (* dir (:direction t)
          (get t :value 1)))))) 
  
(definline neural-output "Creates output-field based on this InputField." 
[input] 
`(output ~input))  

(defn neural-player 
"Constructs a Player object for a particular game, given a brain b (a neural-net), a direction dir and a game-specific searching fn [:best or :random]."  
([game ^MLRegression brain dir searcher extract-response]  
(Player. 
   (fn [leaf _] ;;ignore 2nd arg - we already have direction
     (let [ins (neural-input leaf dir false)
           normals (if (get :normalise-neural-input? game false) 
                     (->> ins anormalise  (evo/data :basic)) 
                     (evo/data :basic ins))  
           output (-> brain (.compute normals) .getData)]  
    (extract-response output leaf)  ))
 dir (get-in game [:searchers searcher] 
       (get-in game [:searchers :minmax]))))  
([game ^MLRegression brain dir searcher] 
   (neural-player game brain dir searcher (fn [a _] (aget ^doubles a 0)))) 
 ([game ^MLRegression brain dir] 
   (neural-player game brain dir :limited)) ) 
 
               
(defn tournament
"Starts a tournament between the 2 Players (p1, p2). If there is no winner, returns the entire history (vector) of 
 the tournament after 100 moves. If there is a winner, a 2d vector will be returned containing both the history (1st item) and the winner (2nd item).
 For games that can end without a winner, [history, nil] will be returned." 
([game-details p1 p2 sb]
(reduce 
 (fn [history player] 
  (let [cb (peek history)]
    (if-let [win-dir ((:game-over?-fn game-details) cb)] 
       (reduced (vector history (condp = win-dir (:direction p1) p1 (:direction p2) p2 nil)))
    (conj history (-> player
                      ((:searcher player) cb (get game-details :pruning?)) 
                      :move
                      try-move))))) 
 [sb] (take (:max-moves game-details) (cycle [p1 p2])))) 
 ([game-details p1 p2] 
   (tournament game-details p1 p2 (starting-board game-details))) )
   
(defn fast-tournament
"Same as tournament but without keeping history. If there is a winner, returns the winning direction
 otherwise returns the last board. Intended to be used with genetic training."  
([game-details p1 p2 sb]
(reduce 
 (fn [board player] 
  (if-let [win-dir ((:game-over?-fn game-details) board)] 
    (reduced (condp = win-dir (:direction p1) p1 (:direction p2) p2 board))
         (-> player
               ((:searcher player) board (:pruning? game-details)) 
               :move
               try-move))) 
     sb (take (:max-moves game-details) (cycle [p1 p2])))) 
 ([game-details p1 p2] 
   (fast-tournament game-details p1 p2 (starting-board game-details))) )  
              
  
(defn ga-fitness
"Scores p1 after competing with p2 using tournament-fn." 
[tournament-fn p1 p2 &{:keys[reward penalty] :or {reward 1 penalty -1}}] 
(let [winner (tournament-fn p1 p2)]  
(condp = (:direction winner) 
       (:direction p1) reward ;reward p1 with 1 point
       (:direction p2) penalty ;penalise p1 with -1 points
       0)))               ;give 0 points - noone won 

  
  
(defn GA 
[game brain pop-size & {:keys [randomizer to-mate to-mutate thread-no total-tournaments ga-type fitness] 
                   :or {randomizer (evo/randomizer :nguyen-widrow)
                        to-mate   0.2
                        to-mutate 0.1
                        total-tournaments (int 5)
                        ga-type :custom
                        thread-no (+ 2 (.. Runtime getRuntime availableProcessors))}}]
(case ga-type
   :custom                        
     (doto 
       (CustomNeuralGeneticAlgorithm. brain randomizer (Referee. game fitness total-tournaments) pop-size to-mutate to-mate)
       (.setThreadCount thread-no))  
   :default (doto (evo/trainer :genetic 
                               :network brain 
                               :randomizer randomizer 
                               :minimize? false 
                               :population-size pop-size 
                               :fitness-fn fitness 
                               :mutation-percent to-mutate 
                               :mate-percent to-mate)  
                  (.setThreadCount thread-no))
   (throw (IllegalArgumentException. "Only :custom and :default GAs are supported!"))) )
   
(definline train [trainer iterations strategies]
  `(evo/train ~trainer Double/NEGATIVE_INFINITY ~iterations ~strategies))                                        
                 
 
