(ns Clondie24.games.tictactoe
    (:require [Clondie24.lib.util :as ut] 
              [Clondie24.lib.core :as core]
              [Clondie24.lib.search :as s]
              [Clondie24.lib.gui :as gui]
              [enclog.nnets :as ai] [enclog.util :as persi] [enclog.training :as evo]
              ;[clojure.core.match :refer [match]]
              [clojure.set :as sets]
              :reload)
    (:import [Clondie24.lib.core Player]
             ;[org.encog.ml MLRegression]
   )
)

;----------------------------------------<SOURCE>--------------------------------------------------------------------
;----------------------------------------<CODE>----------------------------------------------------------------------
(set! *unchecked-math* true)
(set! *warn-on-reflection* true)

(def board-mappings-tic-tac-toe core/mappings-3x3)
(def winning-sets [[0 1 2] [3 4 5] [6 7 8] [0 3 6] [1 4 7] [2 5 8] [0 4 8] [2 4 6]])
(def shape->dir {'X 1 'O -1}) ;;we use direction to represent shape
(def dir->shape {1 'X -1 'O})
(def turns (atom (cycle ['X 'O])))
(declare details starting-board game-over? move score-ttt-naive team-moves)


(def current-tictactoeItems 
(add-watch (atom nil) 
 :log (partial core/log-board core/board-history))) 
             
(defrecord TicTacToePiece [direction rank position]
 core/Piece   
 (update-position [this np] (TicTacToePiece. direction rank np)) 
 (die [this]        (throw (IllegalStateException. "Tic-tac-toe pieces cannot die!"))) 
 (promote [this np] (throw (IllegalStateException. "Tic-tac-toe pieces cannot be promoted!"))) 
 (getListPosition [this] (core/translate-position (first position) (second position) board-mappings-tic-tac-toe))
 (getPoint [this] (ut/make-point position)) 
 (getMoves [this b _] 
   (when-not (game-over? b)  
    (->> (for [i (range (count b)) :let [t (get b i)] :when (nil? t)]  
          (core/translate-position i board-mappings-tic-tac-toe))
     (map #(core/dest->Move b this % move)) )))
 Object
 (toString [this] 
   (println (str (get dir->shape direction)" -> " position))) ) 

(defmethod print-method TicTacToePiece
  [tp ^java.io.Writer w]
  (.write w (str "#" (:record-name details) (into {} tp))) ) 
   
(definline ttt-best-move 
  [^Player p b _] 
`(s/go-lazy ~p ~b))

(defn ttt-rand-move [player b _] 
(let [d (:direction player)]
  (core/rand-move (TicTacToePiece. d (dir->shape d) nil) b nil))) ;same arity as ttt-best-move
  
(defn ttt-agressive-move [^Player p board _]
(let [iboard (map-indexed vector board)
      opp-indices (reduce (fn [v [i piece]] (if (= (:direction piece) (- (:direction p))) (conj v i) v)) [] iboard)
      winning-sets (remove (fn [ws] (some (set opp-indices) ws)) winning-sets)
      mate-indices (reduce (fn [v [i piece]] (if (= (:direction piece) (:direction p)) (conj v i) v)) [] iboard)
      proximities (for [w winning-sets] 
                    (reduce #(if-let [i (some #{%2} w)] 
                                (assoc %1 i w) %1) {} mate-indices) )
      potential   (apply max-key count (if (empty? proximities) [{}] proximities))
      ks          (-> potential keys set)
      potential-win-pos  (-> potential vals first set)
      list-poss (sets/difference potential-win-pos ks)
      tmvs (team-moves board (:direction p))]
 (if-let [gp (first list-poss)] 
     {:move (some #(when (= (core/translate-position gp board-mappings-tic-tac-toe)  (:end-pos %)) %) tmvs)} 
  (ttt-rand-move p board nil))) ) 
  
(defn ttt-next-move [player b _]
  (let [dir (:direction player)
        eval-fn (:brain player)
        tmvs (team-moves b dir)
        zm (zipmap tmvs (map #(-> % core/try-move (eval-fn dir)) tmvs))   
        best-move (->> zm (apply max-key second) first)] 
   {:move best-move}) )
   
(defn ttt-neat-move [player b _]
(let [dir (:direction player)
      eval-fn (:brain player)
      tmvs (team-moves b dir)
      neat-dest (eval-fn b dir)
      neat-move (some #(when (= (:end-pos %) neat-dest) %) tmvs)] 
  {:move neat-move}) )                 
               
(defn move 
"The function responsible for placing tic-tac-toe pieces. 
 Returns the resulting board without making any state changes. " 
[board p coords]
  (let [np (core/update-position p coords)]     
    (assoc board (core/getListPosition np) np))) 
                     

#_(defn match-win "Returns the winning direction [1 , -1] or nil. " 
[b]
(for [[x y z :as rows] winning-sets] 
     (match (mapv #(get dir->shape (:direction (get b %))) rows) ;;[(:shape (get b x)) (:shape (get b y)) (:shape (get b z))]         
        ['X 'X 'X]  1
        ['O 'O 'O] -1
       :else        0)))  ;;(0 0 0 1  0 0 0 0) 
       
(defn match-win [b]
 (for [a-win  winning-sets]       
   (condp = (mapv #(->> % (get b) :direction dir->shape)  a-win)               
      ['X 'X 'X]  1
      ['O 'O 'O] -1  0)))
 
(defn winner "Returns the winning shape [X , O]" 
  [b]
  (let [winning-combinations (match-win b)]
   (cond 
     (some #{-1}  winning-combinations) 'O 
     (some #{1} winning-combinations) 'X
    :else nil)))

(defn score-ttt-naive [b dir] 
 (if-let [w (winner b)] 
   (* dir (get shape->dir w)) 0))     
 
(defn game-over? 
"Checks whetehr we have a winner or a full-board. 
 If you get a direction (1, -1) then there is a winner. If you get true then the board is full." 
 [b]
  (or (get shape->dir (winner b)) 
      (core/full-board? b)))
      
(declare ttt-neural-player)      
 
(defn start-tictactoe!  
"Start a tic-tac-toe game. Returns the starting-board."
([_ p1 p2]
(core/clear-history!) ;empty board-history
(gui/knob! :selection p1) ;; X plays first
(reset! turns (cycle ['X 'O]))
 (deliver s/curr-game details)  
 {:players [p1 p2]
  :board (reset! current-tictactoeItems starting-board)})
([_] 
  (start-tictactoe! _ (Player. score-ttt-naive 1 ttt-agressive-move) (ttt-neural-player (persi/eg-load "tttNets/iter20_layer3_agressive") -1 :limited))) 
 )
  
(defn team-moves  
([b d]
  (let [dummy (TicTacToePiece. d (dir->shape d) nil)] 
    (core/getMoves dummy b nil)))
([b d _] ;;we need this in order to fit with the minimax implementation
  (team-moves b d)) )
  


                
(def details "The map that describes the game of tic-tac-toe."
              {:name 'Tic-Tac-Toe
               :players 2 
               :chunking 10
               :images {:X {1  (ut/make-image "images/133px/tic-tac-toe-X.png")} 
                        :O {-1 (ut/make-image "images/133px/tic-tac-toe-O.png")}}
               :characteristics [:direction :position]      
               :board-size 9 ;;0-8
               :arena-size [420 :by 530]
               :tiles (map vector (for [x (range 0 420 133) 
                                        y (range 0 530 133)] [x y]) 
                                  (cycle nil)) ;;there are no colours 
               :tile-size 133
               :total-pieces 0
               :mover move
               :scorer score-ttt-naive
               :team-moves team-moves
               :searchers {:random ttt-rand-move 
                           :exhaustive ttt-best-move
                           :limited ttt-next-move
                           :neat ttt-neat-move}            
               :max-moves 10
               :pref-depth 9
               ;:referee-gui gui-referee
               :referee-jit winner
               :game-over?-fn  game-over?
               :board-atom current-tictactoeItems
               :game-starter start-tictactoe!
               :hinter ttt-next-move  ;ttt-best-move 
               :record-name "Clondie24.games.tictactoe.TicTacToePiece" ;;fully qualified name
               :mappings board-mappings-tic-tac-toe
               :readers {'Clondie24.games.tictactoe.TicTacToePiece map->TicTacToePiece}})
                
(def starting-board "The empty starting board of tic-tac-toe." (core/empty-board details)) 
       

;(def tictactoe-tournament-fast (partial core/fast-tournament details))   
 
(defmethod gui/canva-react 'Tic-Tac-Toe [_ ^java.awt.event.MouseEvent e]
(let [spot  (vector (.getX e) (.getY e))
      bspot (mapv (ut/balance :down (:tile-size details)) spot)]
(when (core/vacant? board-mappings-tic-tac-toe @current-tictactoeItems bspot)
(let [n-turn (first @turns)
      turn  (get shape->dir n-turn)
      ;image #(get (:images details) nturn)
      ] 
(core/execute! 
  (core/dest->Move @current-tictactoeItems (TicTacToePiece. turn n-turn nil) bspot move) current-tictactoeItems)
  (swap! turns rest)
  (gui/refresh :hint nil :highlighting? nil
               :selection (TicTacToePiece. (- turn) (dir->shape (- turn)) nil)) 
  (gui/request-canva-repaint)
(if (core/full-board? @current-tictactoeItems) (gui/alert! "GAME OVER!")
  (when-let [win-shape (winner @current-tictactoeItems)] 
    (do (gui/knob! :block? true) (gui/alert! (str win-shape " WON!")))))))))
    

(def ttt-neural-player    (partial core/neural-player details))
(def ttt-tournament      #(core/tournament details %1 %2 starting-board)) ;one more move to realise winner
(def ttt-tournament-fast #(core/fast-tournament details %1 %2 starting-board))
(def ttt-ga-fitness      #(core/ga-fitness ttt-tournament-fast %1 %2 :penalty -2)) 


(defn ttt-player 
([character shape n-brain n-searcher]
(case character
   :random    (Player. nil shape ttt-rand-move) ;don't need a brain to pick a random move
   :agressive (Player. nil shape ttt-agressive-move) ;don't need a brain to pick an agressive move
   :neural    (ttt-neural-player n-brain shape n-searcher)
   :neat      (core/neural-player details n-brain shape n-searcher (fn [arr ^clojure.lang.PersistentVector b] 
 								    (let [indexed (->> arr (map-indexed vector) (into {}))  
      									  indices (keys indexed)
      									  dests   (map #(vector (long (Math/floor (inc (/ % 3)))) (inc (mod % 3))) indices)
      									  valid-dest (some #(when (nil? (get (.indexOf b %) b)) %) dests)]
   								      valid-dest)))))
([character shape]
  (if-not (= character :neural) (ttt-player character shape nil nil)
    (throw (IllegalArgumentException. "Cannot have a neural player without a brain (network)!")))) )    
 

(defn ttt-fitness 
([net oppo tournaments n-player n-searcher]
(/
  (->> #(ttt-ga-fitness (ttt-player n-player 1 net n-searcher) (ttt-player oppo -1))
    (repeatedly tournaments)
    (apply +)) tournaments))
([net oppo tournaments n-player]
  (ttt-fitness net oppo tournaments n-player :limited))
([net oppo tournaments]
  (ttt-fitness net oppo tournaments :neural))
([net oppo] 
  (ttt-fitness net oppo 100)) )       

(def brain "A traditional feed-forward brain." 
(ai/network (ai/neural-pattern :feed-forward) 
         :activation :sigmoid
         :input 9 ;the entire board for input
         :output 1 ;the score
         :hidden [27])) ; 1 hidden layers
                        
(defn TTT-NEAT "A NEAT population ready for training." [oppo-variant pop-size] 
(let [tr (evo/trainer :neat :fitness-fn #(ttt-fitness % oppo-variant 100 :neat :neat) :minimize? false :input 9 :output 9 :population-size pop-size)]
 (-> tr .getPopulation (.setNeatActivationFunction (ai/activation :sigmoid)))
 tr))
                       
 
(defn TTT-GA 
([oppo-variant brain pop-size]
  (case oppo-variant
    :rand-player     (core/GA details brain pop-size :ga-type :default :fitness #(ttt-fitness % :random)) ;;total-tournaments is baked in the fitness fn
    :agressive       (core/GA details brain pop-size :ga-type :default :fitness #(ttt-fitness % :agressive))
    :rand-contestant (core/GA details brain pop-size :total-games 100  :fitness ttt-tournament-fast)))
([brain pop-size]
  (TTT-GA :rand-contestant brain pop-size)) )     
                        


;(tournament starting-board 9 (random-player 'X) (random-player 'O))
               
               
(defn -main [& args]
(gui/show-gui! details)
#_(ttt-best-move 1 (start-tictactoe! nil) 10))               
               
                         
               
