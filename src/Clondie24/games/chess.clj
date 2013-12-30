(ns Clondie24.games.chess 
    (:require [Clondie24.lib.util :as ut] 
              [Clondie24.lib.core :as core]
              [Clondie24.lib.search :as s]
              [Clondie24.lib.fmoves :as fmov]
              [Clondie24.lib.gui :as gui]
              [enclog.nnets :as ai]
              ;[clojure.core.memoize :as memo] 
              #_[clj-tuple :refer [tuple]] :reload)
    (:import [Clondie24.lib.core Player])
)

;----------------------------------------<SOURCE>--------------------------------------------------------------------
;----------------------------------------<CODE>----------------------------------------------------------------------

(set! *warn-on-reflection* true)
(set! *unchecked-math*     true)

;(defonce board-mappings-chess core/mappings-8x8)
(defonce board-mappings-chess core/mappings-8x8 #_(mapv #(tuple (first %) (second %)) core/mappings-8x8))  
(defonce chess-board-colours [(ut/hex->color '0xffdead) ;funny colour name!
                              (ut/hsb->color 0.931 0.863 0.545)])
                                 
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
                       
(defonce promotion-row {1 7 -1 0})
(defonce pos-groups [['rook 'knight 'bishop 'queen 'king 'bishop 'knight 'rook] (repeat 8 'pawn)])

(defonce chessPos->rank (flatten pos-groups))
        
(defonce rel-values (zipmap '(:queen :rook :knight :bishop :pawn :king) 
                            '(  9      5     3       3       1    100)))

(declare score-chess-naive, details, start-chess!, buffered-moves, castling-moves, en-passant-move, starting-board, chess-player) ;;will need these

(definline valid-move? [m b] 
 `(some #{~m} (core/getMoves (:p ~m) ~b false) )) 

(defn chess-best-move 
 ([^Player p b] (chess-best-move p b false))
 ([^Player p  b pruning?] (s/go p b pruning?)))
  

(def chess-rand-move core/rand-team-move)

(def current-chessItems
"This is list that keeps track of moving chess pieces. Is governed by an atom and it changes after every move. 
 All changes are being logged to 'board-history'. Starts off as nil but we can always get the initial arrangement from core."
 (-> (atom nil)
   (add-watch  :log #(core/log-board core/board-history %1 %2 %3 %4))
   #_(add-watch  :last-move (fn [k r old n] (swap! r conj n)))))
   
(definline pawn-area [b x y direction]
 `(if (neg? ~direction) 
   (vector (try (get ~b (core/translate-position ~x (dec ~y) board-mappings-chess)) 
           (catch Exception ~'e :out)) ;;returning :out because nil means vacant
           (try (get ~b (core/translate-position ~x (- ~y 2) board-mappings-chess))
           (catch Exception ~'e :out))
           (try (get ~b (core/translate-position (dec ~x) (dec ~y) board-mappings-chess))
           (catch Exception ~'e :out)) 
 	   (try (get ~b (core/translate-position (inc ~x) (dec ~y) board-mappings-chess))
 	   (catch Exception ~'e :out)) )  
   (vector (try (get ~b (core/translate-position ~x (inc ~y) board-mappings-chess))
           (catch Exception ~'e :out))
           (try  (get ~b (core/translate-position ~x (+ ~y 2) board-mappings-chess))
           (catch Exception ~'e :out))
           (try (get ~b (core/translate-position (dec ~x) (inc ~y) board-mappings-chess))  
 	   (catch Exception ~'e :out))
           (try (get ~b (core/translate-position (inc ~x) (inc ~y) board-mappings-chess))  
 	   (catch Exception ~'e :out)) )) )

           
#_(defn pawn-moves** [b [x y :as pos] dir]
  (let [infrnt (ut/vertical-neighbours pos 2)
        in-front (if (neg? dir) (filter #(> y (second %)) infrnt)
                                (filter #(< y (second %)) infrnt))
        frd (ut/diagonal-neighbours pos 1)
        front-diag (if (neg? dir) (filter #(> y (second %)) frd) 
                                  (filter #(< y (second %)) frd)) ]
   (if 
      (or (= y 6) 
          (= y 1)) (concat (filter #(core/vacant? board-mappings-chess b %) in-front)
                           (filter #(core/occupied? board-mappings-chess b %) front-diag))   
     (concat (filter #(core/occupied? board-mappings-chess b %) front-diag) 
             (filter #(= 1 (* dir (- (second %) y))) in-front)))) )           
           
;(def pawn-moves (memo/lru #(fmov/pawn-moves board-mappings-chess %1 [%2 %3] %4) :lru/threshold 1024))             

(def chess-moves {:pawn     #(fmov/pawn-moves  board-mappings-chess %1 [%2 %3] %4) 
                             ;;#(pawn-moves #_(pawn-area %1 %2 %3 %4) %1 [%2 %3] %4) 
                  :rook      fmov/rook-moves
                  :bishop    fmov/bishop-moves
                  :knight    fmov/knight-moves
                  :queen     fmov/queen-moves
                  :king      fmov/king-moves})
                  
                  
                                   
(defn rank->moves 
"Returns all legal moves of piece p depending on rank of p (excluding pawn). Will be called only once to buffer the moves." 
[p]
(let [[x y] (:position p)            ;[[x y] (:position p) 
      r (keyword (:rank p))
      d (:direction p)]      
((r chess-moves) [x y]))) ;will return a fn which is called with current x and y 
;(if (= (class p) (Class/forName "Clondie24.games.chess.ChessPiece2"))
;(ut/Point->Vec pos) pos))))  
                         

(defn start-chess! ;mandatory before GUI game starts 
"Start a proper chess-game (with history, searching and GUI capabilities). 
 Returns the starting-board, a vector by default (no args), or optionally a Java array."
([fast? p1 p2] 
  (core/clear-history!) ;clear board-history first
  (deliver s/curr-game details) ;;hook on to tree-searching
  {:players [p1 p2]
   :board (reset! current-chessItems ;;duh!
            (if fast? (into-array starting-board)
              starting-board))})
([fast?] 
  (start-chess! fast? (chess-player :naive 1)
                      (chess-player :naive -1)))
([] (start-chess! false)))
    
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

(defrecord ChessPiece [ ;image ;;not really an image but a fn that fetches the image
                        position 
                        rank ^long value direction]
 core/Piece 
 (update-position [this np] (if (and (= (second np) (get promotion-row direction))
                                     (= rank 'pawn)) (core/promote this np) 
                                (ChessPiece. np rank value direction {:alive true 
                                                                            :has-moved? true} nil)))
 (die [this]     (vary-meta this assoc :alive false)) ;communicate death through meta-data 
 (promote [this np] (ChessPiece. np 'queen 9 direction {:alive true :has-moved? true} nil)) ;a pawn is promoted to a queen
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
                     (-> move-creator
                       (mapv ((:pawn chess-moves)  b x y direction))
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
   #_(println "Chess-item (" rank ") at position:" (core/getListPosition this) " ->" position)
   (str "#" (:record-name details) " " (assoc (into {} this) :meta (:meta this)))) )
 
 
(defmethod print-method ChessPiece
  [cp ^java.io.Writer w]
  (.write w (str "#" (:record-name details)))  (-> cp class .getName)  
  (print-method {:position (:position cp) 
                 :rank  (:rank cp) 
                 :value (:value cp)
                 :direction (:direction cp)  
                 :states (meta cp)} w))

(defn chesspiece-reader [piece] ;;edn reader for ChessPiece that re-attaches the metadata
 (-> piece
   (dissoc :states)
    map->ChessPiece
   (with-meta (:states piece)))) 
      
   
(defn starting-chessItems
"Will construct a set of initial chess items (16). 
black? specifies the side of the board where the pieces should be placed (true for north false for south)."
[black?]         
(if black?  
(map #(ChessPiece.  #_(fn [] (get-in chess-images [(keyword %2) 1]))
      (core/translate-position % board-mappings-chess) %2
                       ((keyword %2) rel-values)  1 {:alive true :has-moved? false} nil) (range 16) chessPos->rank)
(map #(ChessPiece.  #_(fn [] (get-in chess-images [(keyword %2) -1])) 
      (core/translate-position % board-mappings-chess) %2
                      ((keyword %2) rel-values)  -1 {:alive true :has-moved? false} nil) (range 48 64) (flatten (reverse pos-groups)))))
                      
(def brain (ai/network (ai/neural-pattern :feed-forward) 
               :activation :tanh
               :input 64 ;the entire board for input
               :output 1 ;the score
               :hidden [80 40 10])) ; 3 hidden layers                      
                      
(def details "The map that describes the game of chess."
              {:name 'Chess
               :players 2 
               :chunking 2
               :images chess-images
               :characteristics [:position :rank :value :direction]      
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
                           :minmax chess-best-move}
               :pref-depth 4
               :pruning? false
               :max-moves 100
               :board-atom current-chessItems
               :readers {'Clondie24.games.chess.ChessPiece chesspiece-reader}
               :game-starter start-chess!
               :hinter chess-best-move 
               :record-name (.getName ChessPiece) ;;fully qualified name 
               :mappings board-mappings-chess
               :north-player-start  (starting-chessItems true)   ;opponent (black)
               :south-player-start  (starting-chessItems false)});human (white or yellow)
               
(def starting-board "The empty starting board of chess." (core/starting-board details))                

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
        (or (nil? krook) 
            (:has-moved? (meta krook))) nil
        (and (nil? (get b (core/translate-position (inc kx) ky core/mappings-8x8)))
             (nil? (get b (core/translate-position (+ 2 kx) ky core/mappings-8x8)))
             (not (:has-moved? (meta krook))) )
      (aset castlings 0 (core/dest->Move b [king, krook]  [[(+ 2 kx) ky], [(inc kx) ky]] castling-mover))) ;;kingside castling-move (2 moves)
      (cond  
        (or (nil? qrook) 
            (:has-moved?  (meta qrook))) nil
        (and (nil? (get b (core/translate-position (dec kx) ky core/mappings-8x8)))
             (nil? (get b (core/translate-position (- kx 2) ky core/mappings-8x8)))
             (nil? (get b (core/translate-position (- kx 3) ky core/mappings-8x8))))
     (aset castlings 1 ;;a move with double-impact (the mover supplied must be able to hanle it)
       (core/dest->Move b [king, qrook]  [[(- kx 2) ky], [(dec kx) ky]] castling-mover))) ;;queenside castling-move
  (ut/no-nils castlings))))

(defn en-passant-mover [b p [nx ny :as np]]
   (let [newPiece (core/update-position p np) ;the new piece as a result of moving 
         old-pos  (core/getListPosition p)
         new-pos  (core/getListPosition newPiece)
         mov-dir  (ut/resolve-direction (:position p) np)]
      (when-let [en-passant-capture (cond
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
         (= 1 (Math/abs ^long (- (first (:position pawn)) ;;side by side (x +/- 1)
                                 (first (:end-pos last-move)))))
         (= (second (:position pawn)) (second(:end-pos last-move))) ;;same y
         (= 2 (Math/abs ^long (- (second (:position last-moving-piece)) 
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
     (map #(rank->moves (ChessPiece. (core/translate-position k board-mappings-chess) % 
               ((keyword %) (:rel-values details)) nil)) 
      ['rook 'knight 'bishop 'queen 'king])))))))
            
  
(def chess-neural-player   (partial core/neural-player details))
(def chess-tournament      #(core/tournament details %1 %2 starting-board))
(def chess-tournament-fast #(core/fast-tournament details %1 %2 starting-board))
(def chess-ga-fitness      #(core/ga-fitness chess-tournament-fast %1 %2 :penalty -2))

(defn chess-player 
([character dir n-brain]
(case character
   :random    (Player. nil dir chess-rand-move) ;don't need a brain to pick a random move
   :naive     (Player. core/score-chess-naive dir chess-best-move) 
   :neural    (chess-neural-player n-brain dir chess-best-move)))
([character dir]
  (if-not (= character :neural) (chess-player character dir nil)
    (throw (IllegalArgumentException. "Cannot have a neural player without a brain (network)!")))) ) 
      
#_(defn chess-fitness 
([net oppo tournaments n-player]
(/
  (->> #(chess-ga-fitness (chess-player n-player 1 net) (chess-player oppo -1))
    (repeatedly tournaments)
    (apply +)) tournaments))
([net oppo tournaments]
  (chess-fitness net oppo tournaments :neural))
([net oppo] 
  (chess-fitness net oppo 5)) )      

(defn CHESS-GA [brain pop-size]
 (deliver s/curr-game details)
 (core/GA details brain pop-size :fitness chess-tournament-fast)) 
 
 
;(ut/data->string buffered-moves "machine-performanceNEW.cheat") 

(defonce buffered-moves (ut/string->data "machine-performanceNEW.cheat" details))  ;it's faster to read them from file than recalculate       

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


      
               
