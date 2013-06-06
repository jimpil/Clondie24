(ns Clondie24.games.chess 
    (:require [Clondie24.lib.util :as ut] 
              [Clondie24.lib.core :as core]
              [Clondie24.lib.search :as s]
              [Clondie24.lib.rules :as rul]
              [Clondie24.lib.gui :as gui]
              [enclog.nnets :as ai]
              ;[enclog.training :as evo]
              ;[enclog.normalization :as norm] 
    )
    (:import  #_[encog_java.customGA CustomNeuralGeneticAlgorithm 
                                   CustomGeneticScoreAdapter Referee] 
              [Clondie24.lib.core Player])
)

;----------------------------------------<SOURCE>--------------------------------------------------------------------
;----------------------------------------<CODE>----------------------------------------------------------------------

(def board-mappings-chess core/mappings-8x8) 
(def chess-board-colours [(ut/hex->color '0xffdead) ;funny colour name!
                          (ut/hsb->color 0.931 0.863 0.545)])

;(defrecord Player [brain ^int direction searcher])
                                 
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
(def pos-groups [['rook 'knight 'bishop 'queen 'king 'bishop 'knight 'rook] (repeat 8 'pawn)])

(def chessPos->rank 
(flatten pos-groups))
         
(def rel-values (zipmap '(:queen :rook :knight :bishop :pawn :king) 
                        '(  9      5     3       3       1    100)))

(declare details, start-chess!, buffered-moves, castling-moves, en-passant-move) ;;will need these

(definline valid-move? [m b] 
 `(some #{~m} (core/getMoves (:p ~m) ~b false) )) 

(defn chess-best-move 
 ([dir b] (chess-best-move dir b false))
 ([dir b pruning?] (s/go dir b pruning?)))
  

(def chess-rand-move core/rand-team-move)

(def current-chessItems
"This is list that keeps track of moving chess pieces. Is governed by an atom and it changes after every move. 
 All changes are being logged to 'board-history'. Starts off as nil but we can always get the initial arrangement from core."
 (-> (atom nil)
   (add-watch  :log (partial core/log-board core/board-history))
   #_(add-watch  :last-move (fn [k r old n] (swap! r conj n)))))

(def chess-moves {:pawn     #(rul/pawn-moves board-mappings-chess %1 %2 %3 %4) 
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
                         

(defn start-chess! ;mandatory before GUI game starts 
"Start a proper chess-game (with history, searching and GUI capabilities). 
 Returns the starting-board, a vector by default (no args), or optionally a Java array."
  ([] (start-chess! false))
  ([fast?] 
    (core/clear-history!) ;clear board-history first
    (deliver s/curr-game details) ;;hook on to tree-searching
    (reset! current-chessItems ;;duh!
       (if fast? (into-array  (core/starting-board details))
                              (core/starting-board details)))))
    
(definline jit-referee ;just-in-time referee
"Inspects the board for missing kings. If no team is missing its king returns nil (no winner),
 otherwise returns the direction of the team which has a king still standing (winner)." 
 [b]
 `(let [kings# (filter #(= (:rank %) 'king) ~b)]
   (if (= 2 (count kings#)) nil ;;no winners
     (:direction (first kings#))))) ;;return the direction of the king still standing 
     
(definline gui-referee [b] ;referee a-la checkmate for the gui
`(let [t1-moves# (into [] (core/team-moves ~b  1 true))
       t2-moves# (into [] (core/team-moves ~b -1 true))]
 (cond 
    (empty? t1-moves#) "Yellow wins!"  
    (empty? t2-moves#) "Black wins!"   
 :else nil)))         ;;no winner    
               
(def chess-piece  (partial core/piece details))

(def vacant-chess-tile? (partial core/vacant? board-mappings-chess))                         

(defrecord ChessPiece [ image ;;not really an image but a fn that fetches the image
                       ^clojure.lang.PersistentVector position 
                        rank ^long value direction]
 core/Piece 
 (update-position [this np] (if (and (= (second np) (get promotion-row direction))
                                     (= rank 'pawn)) (core/promote this np) 
                                (ChessPiece. image np rank value direction {:alive true 
                                                                            :has-moved? true} nil)))
 (die [this]     (vary-meta this assoc :alive false)) ;communicate death through meta-data 
 (promote [this np] (ChessPiece. #(get-in chess-images [:queen direction]) np 'queen 9 direction {:alive true 
                                                                                                  :has-moved? true} nil)) ;a pawn is promoted to a queen
 (getListPosition [this] (core/translate-position (first position) (second position) board-mappings-chess))
 (getPoint [this] (ut/make-point position))
 ;(getMoves [this b with-precious?] (core/getMoves this b with-precious? true))
 (getMoves [this b with-precious?]
  (let [[x y] position
        move-creator #(core/dest->Move b this % nil)] 
       (core/remove-illegal #(or 
                             (core/collides? % 
                               (ut/make-walker 
                                 (ut/resolve-direction position  
                                                       (let [ep (:end-pos %)] ;;the first move is what matters in multi-moves
                                                        (if (sequential? (first ep)) (first ep) ep))) 
                                 rank) b board-mappings-chess)
                            (core/exposes? % (when with-precious? 'king)))    
                  (case rank 
                    pawn
                    (if-let [en-pmv (en-passant-move b this)]
                     (-> 
                       (mapv move-creator ((:pawn chess-moves)  b x y direction))
                        (conj en-pmv))
                     (mapv move-creator ((:pawn chess-moves)  b x y direction)))
                    king (into 
                           (->> (get-in buffered-moves 
                                    [(core/translate-position x y board-mappings-chess) :king])
                            (mapv move-creator))
                            (when-not (and with-precious? 
                                           (some #(core/threatens? this % b) 
                                            (core/gather-team b (- direction)))) 
                                    (castling-moves b this))) ;;castling is a move of the king's
                          (->> (get-in buffered-moves
                                    [(core/translate-position x y board-mappings-chess) (keyword rank)])
                             (mapv move-creator))))))  
 Object
 (toString [this] 
   (println "Chess-item (" rank ") at position:" (core/getListPosition this) " ->" position)) )
   
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
"Will construct a set of initial chess items (16). 
black? specifies the side of the board where the pieces should be placed (true for north false for south)."
[black?]         
(if black?  
(map #(ChessPiece. (fn [] (get-in chess-images [(keyword %2) 1]))
      (core/translate-position % board-mappings-chess) %2
                       ((keyword %2) rel-values)  1 {:alive true :has-moved? false} nil) (range 16) chessPos->rank)
(map #(ChessPiece. (fn [] (get-in chess-images [(keyword %2) -1])) 
      (core/translate-position % board-mappings-chess) %2
                      ((keyword %2) rel-values)  -1 {:alive true :has-moved? false} nil) (range 48 64) (flatten (reverse pos-groups)))))
                      
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
               :arena-size [421 :by 530]
               :tile-size 50
               :alternating-colours chess-board-colours
               :color-names ["Yellow" "Black"]
               :tiles (mapv vector (for [x (range 0 421 50) 
                                         y (range 0 530 50)] [x y]) 
                                   (cycle chess-board-colours))
               :total-pieces 32
               :rel-values rel-values
               :obligatory-move nil
               :team-moves core/team-moves
               :mover core/move
               :referee-gui gui-referee
               :referee-jit jit-referee
               :game-over?-fn  jit-referee
               :scorer core/score-chess-naive ;;don't have any other scorers
               :naive-scorer core/score-chess-naive
               :searchers {:random chess-rand-move 
                           :best chess-best-move}
               :pref-depth 4
               :max-moves 100
               :board-atom current-chessItems
               :game-starter start-chess!
               :hinter chess-best-move 
               :record-name "Clondie24.games.chess.ChessPiece" ;;fully qualified name
               :mappings board-mappings-chess
               :north-player-start  (starting-chessItems true)   ;opponent (black)
               :south-player-start  (starting-chessItems false)});human (white or yellow)
               
(def starting-board "The empty starting board of chess." (core/empty-board details))                

(defn castling-mover [board ps new-positions]
 (reduce-kv (fn [b piece coo]
             (let [newPiece (core/update-position piece coo) ;the new piece as a result of moving 
                   old-pos  (core/getListPosition piece)
                   new-pos  (core/getListPosition newPiece)]            
                   (assoc b old-pos nil 
                            new-pos newPiece))) board (zipmap ps new-positions)))

(defn castling-moves [b king]
  (if  (:has-moved? (meta king)) nil ;;if king has moved, don't bother looking
    (let [[kx ky] (:position king)
          krook (get b (core/translate-position (+ 3 kx) ky core/mappings-8x8))
          qrook (get b (core/translate-position (- 4 kx) ky core/mappings-8x8))
          castlings (make-array java.util.Map 2)] ;;the two sides (left or right)
     (cond 
       (:has-moved? (meta king)) nil
        (and (nil? (get b (core/translate-position (inc kx) ky core/mappings-8x8)))
             (nil? (get b (core/translate-position (+ 2 kx) ky core/mappings-8x8)))
             (not (:has-moved? (meta krook))))
      (aset castlings 0 (core/dest->Move b [king, krook]  [[(+ 2 kx) ky], [(inc kx) ky]] castling-mover)) ;;kingside castling-move (2 moves)
        (and (nil? (get b (core/translate-position (dec kx) ky core/mappings-8x8)))
             (nil? (get b (core/translate-position (- kx 2) ky core/mappings-8x8)))
             (nil? (get b (core/translate-position (- kx 3) ky core/mappings-8x8)))
             (not (:has-moved?  (meta qrook))))
     (aset castlings 1 ;;a move with double-impact (the mover supplied must be able to hanle it)
       (core/dest->Move b [king, qrook]  [[(- kx 2) ky], [(dec kx) ky]] castling-mover))) ;;queenside castling-move
  (remove nil? castlings))))

(defn en-passant-mover [b p [nx ny :as np]]
   (let [newPiece (core/update-position p np) ;the new piece as a result of moving 
         old-pos  (core/getListPosition p)
         new-pos  (core/getListPosition newPiece)
         mov-dir  (ut/resolve-direction (:position p) np)]
      (when-let [en-passant-capture  (cond
                                         (or (= mov-dir :south-east) 
                                             (= mov-dir :south-west)) (core/translate-position nx (dec ny) core/mappings-8x8)
                                         (or (= mov-dir :north-east) 
                                             (= mov-dir :north-west)) (core/translate-position nx (inc ny) core/mappings-8x8)
                               :else nil)]
      (assoc b old-pos nil 
               new-pos newPiece
               en-passant-capture nil))))


(defn en-passant-move [b pawn]
  (when-let [last-move (-> b meta :caused-by)] 
      (let [last-moving-piece (:p last-move)]  
        
  (when  (and 
         (and (= 'pawn (:rank last-moving-piece))
              (not= (:direction pawn) (:direction last-moving-piece)))
         (= 1 (Math/abs (- (first (:position pawn)) ;;side by side (x +/- 1)
                                 (first (:end-pos last-move)))))
         (= (second (:position pawn)) (second(:end-pos last-move))) ;;same y
         (= 2 (Math/abs (- (second (:position last-moving-piece)) 
                                 (second (:end-pos last-move)))))) ;;previous pawn  jumped 2 squares
         (core/dest->Move b pawn 
                          (if (neg? (:direction pawn)) 
                             [(first (:end-pos last-move)) 
                              (dec (second (:end-pos last-move)))]
                             [(first (:end-pos last-move)) 
                              (inc (second (:end-pos last-move)))])  en-passant-mover)))))
                      
#_(defmacro definvokable
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
      

#_(defn normalize-fields [ins outs]
((norm/prepare :range [ins] [outs]) false 
  (norm/target-storage :norm-array [64 nil])))          
  

(def chess-tournament (partial core/tournament details))
(def chess-tournament-fast (partial core/fast-tournament details))

#_(defn tournament
"Starts a tournament between the 2 players (p1, p2). If there is no winner, returns the entire history (vector) of 
 the tournament after 100 moves. If there is a winner, a map will be returned containing :history and the :winner." 
[sb p1 p2 & {:keys [limit]
             :or   {limit 100}}]
(reduce 
 (fn [history player] 
  (let [cb (peek history) 
        win-dir (jit-referee cb)]
    (if win-dir (reduced {:history (persistent! history) 
                          :winner (if (= win-dir (:direction p1)) p1 p2)})
    (conj! history (->> player
                      :brain
                      ((:searcher player) (:direction player) cb) 
                      :move
                      core/try-move))))) 
 (transient [sb]) (take limit (cycle [p1 p2])))) ;;100 moves should be enough
  
 
#_(defn fast-tournament 
"Same as tournament but without keeping history. If there is a winner, returns the winning direction
otherwise returns the last board. Intended to be used with genetic training." 
[sb p1 p2 & {:keys [limit]
             :or   {limit 100}}]
(reduce 
  (fn [board player]
    (if-let [win-dir (jit-referee board)] (reduced (if (= win-dir (:direction p1)) p1 p2))
    (->> player
          :brain
          ((:searcher player) (:direction player) board)
          :move
           core/try-move))) 
 sb (take limit (cycle [p1 p2])))) ;;100 moves should be enough
  
  
(defn random-player [dir]
(Player. nil dir chess-rand-move)) 


(defn naive-player [dir]
(Player. core/score-chess-naive dir chess-best-move)) 

(defn chess-GA [pop-size]
 (deliver s/curr-game details)
 (core/GA brain pop-size))       

#_(ut/data->string buffered-moves "machine-performance.cheat") 
(def buffered-moves (ut/string->data "machine-performance.cheat"))  ;it's faster to read them from file than recalculate       

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


      
               
