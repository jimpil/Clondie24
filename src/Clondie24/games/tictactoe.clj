(ns Clondie24.games.tictactoe
    (:require [Clondie24.lib.util :as ut] 
              [Clondie24.lib.core :as core]
              [Clondie24.lib.search :as s]
              [Clondie24.lib.gui :as gui]
              [enclog.nnets :as ai]
              [clojure.core.match :refer [match]]
              ;[enclog.training :as evo]
              ;[enclog.normalization :as norm] 
              )
    (:import [Clondie24.lib.core Player] 
             ;[encog_java.customGA CalculateScore CustomNeuralGeneticAlgorithm CustomGeneticScoreAdapter Referee ] 
             ;[org.encog.ml MLRegression]
   )
)

;----------------------------------------<SOURCE>--------------------------------------------------------------------
;----------------------------------------<CODE>----------------------------------------------------------------------


(def board-mappings-tic-tac-toe core/mappings-3x3)
(def winning-sets [[0 1 2] [3 4 5] [6 7 8] [0 3 6] [1 4 7] [2 5 8] [0 4 8] [2 4 6]])
(def shape->dir {'X -1 'O 1}) ;;we use direction to represent shape
(def dir->shape {-1 'X 1 'O})
(def turns (atom (cycle ['X 'O])))
;(defrecord Player [brain shape searcher])
(declare details starting-board game-over? move score-ttt-naive)


(definline tic-tac-toe-best-move 
  [dir b _] 
`(s/go-lazy ~dir ~b))

(def current-tictactoeItems 
(add-watch (atom nil) 
 :log (partial core/log-board core/board-history))) 
             
(defrecord TicTacToePiece [direction image position]
 core/Piece   
 (update-position [this np] (TicTacToePiece. direction image np)) 
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
               
(defn move 
"The function responsible for placing tic-tac-toe pieces. 
 Returns the resulting board without making any state changes. " 
[board p coords]
  (let [np (core/update-position p coords)]     
    (assoc board (core/getListPosition np) np))) 
                     

(defn match-win "Returns the winning direction [1 , -1] or nil. " 
[b]
(for [[x y z :as rows] winning-sets]
     (match   (mapv #(get dir->shape (:direction (get b %))) rows) ;;[(:shape (get b x)) (:shape (get b y)) (:shape (get b z))]        
        ['X 'X 'X] -1
        ['O 'O 'O]  1
       :else       0)))  ;;(0 0 0 1  0 0 0 0)            
 

(defn winner "Returns the winning shape [X , O]" 
  [b]
  (let [winning-combinations (match-win b)]
   (cond 
     (some #{1}  winning-combinations) 'O 
     (some #{-1} winning-combinations) 'X
    :else nil)))
 
(defn game-over? [b]
  (or (winner b)
      (core/full-board? b)))
 
(defn start-tictactoe! [_] 
"Start a tic-tac-toe game. Returns the starting-board."
(core/clear-history!) ;empty board-history
(gui/knob! :selection (TicTacToePiece. -1 nil nil)) ;; X plays first\
(reset! turns (cycle ['X 'O]))
 (deliver s/curr-game details)
  (reset! current-tictactoeItems starting-board)) 
  
(defn team-moves 
([b d]
  (let [dummy (TicTacToePiece. d nil nil)] 
    (core/getMoves dummy b nil)))
([b d _] ;;we need this in order to fit with the minimax implementation
  (team-moves b d)) )

(defn score-ttt-naive [b dir] 
 (if-let [w (winner b)] 
   (* (get shape->dir w) dir) 0))  
                
(def details "The map that describes the game of tic-tac-toe."
              {:name 'Tic-Tac-Toe
               :players 2 
               :chunking 10
               :images {'X (ut/make-image "images/133px/tic-tac-toe-X.png") 
                        'O (ut/make-image "images/133px/tic-tac-toe-O.png")}
               :characteristics [:direction :image :position]      
               :board-size 8 ;;0-8
               :arena-size [420 :by 530]
               :tiles (map vector (for [x (range 0 420 133) 
                                        y (range 0 530 133)] [x y]) 
                                  (cycle nil)) ;;there are no colours 
               :tile-size 133
               :total-pieces 0
               :mover move
               :scorer score-ttt-naive
               :team-moves team-moves
               :max-moves 9
               :pref-depth 9
               ;:referee-gui gui-referee
               :referee-jit winner
               :game-over?-fn  game-over?
               :board-atom current-tictactoeItems
               :game-starter start-tictactoe!
               :hinter tic-tac-toe-best-move 
               :record-name "Clondie24.games.tictactoe.TicTacToePiece" ;;fully qualified name
               :mappings board-mappings-tic-tac-toe})
                
(def starting-board "The empty starting board of tic-tac-toe." (core/empty-board details)) 

(defn ttt-rand-move [direction b _ ] 
  (let [p (TicTacToePiece. direction nil nil)]
    {:move (rand-nth (core/getMoves p b nil))})) 
    
(def tictactoe-tournament (partial core/tournament details))
(def tictactoe-tournament-fast (partial core/fast-tournament details))   

#_(defn tournament
"Starts a tournament between the 2 players (p1, p2). If there is no winner, returns the entire history (vector) of 
 the tournament after 100 moves. If there is a winner, a 2d vector will be returned containing both the history(1st item) 
 and the winner (2nd item)." 
[sb depth p1 p2 & {:keys [limit]
                   :or   {limit 20}}]
(reduce 
 (fn [history player] 
  (let [cb (peek history) 
        win-dir (winner cb)]
    (if (core/full-board? cb) (reduced (vector history (when win-dir 
                                                   (if (= win-dir (:direction p1)) p1 p2))))
    (conj history (->> player
                      :brain
                      ((:searcher player) (:direction player) cb depth) 
                      :move
                      core/try-move))))) 
 [sb] (take limit (cycle [p1 p2])))) ;;20 moves each should be enough
 
#_(defn fast-tournament 
"Same as tournament but without keeping history. If there is a winner, returns the winning direction
otherwise returns the last board. Intended to be used with genetic training." 
[sb p1 p2 & {:keys [limit]
             :or   {limit 20}}]
(reduce 
  (fn [board player]
   (let [win-dir (winner board)]
    (if (core/full-board? board) (reduced (when win-dir 
                                     (if (= win-dir (:direction p1)) p1 p2) ))
    (->> player
          :brain
          ((:searcher player) (:direction player) board)
          :move
           core/try-move)))) 
 sb (take limit (cycle [p1 p2])))) ;;100 moves should be enough 

(defmethod gui/canva-react 'Tic-Tac-Toe [_ ^java.awt.event.MouseEvent e]
(let [spot  (vector (.getX e) (.getY e))
      bspot (mapv (ut/balance :down (:tile-size details)) spot)]
(when (core/vacant? board-mappings-tic-tac-toe @current-tictactoeItems bspot)
(let [turn  (get shape->dir (first @turns))
      image #(get (:images details) (get dir->shape turn))] 
(core/execute! 
  (core/dest->Move @current-tictactoeItems (TicTacToePiece. turn image nil) bspot move) current-tictactoeItems)
  (swap! turns rest)
  (gui/refresh :hint nil :highlighting? nil
               :selection (TicTacToePiece. (- turn) nil nil)) 
  (gui/request-canva-repaint)
(if (core/full-board? @current-tictactoeItems) (gui/alert! "GAME OVER!")
  (when-let [win-shape (winner @current-tictactoeItems)] 
    (do (gui/knob! :block? true) (gui/alert! (str win-shape " WON!")))))))))

(defn random-player [shape]
(Player. nil shape ttt-rand-move)) 

(def brain (ai/network (ai/neural-pattern :feed-forward) 
                        :activation :sigmoid
                        :input 9 ;the entire board for input
                        :output 1 ;the score
                        :hidden [3])) ; 1 hidden layer
(defn TTT-GA [pop-size]
 (deliver s/curr-game details)
 (core/GA brain pop-size))
                        


;(tournament starting-board 9 (random-player 'X) (random-player 'O))
               
               
(defn -main [& args]
(gui/show-gui! details)
#_(tic-tac-toe-best-move 1 (start-tictactoe! nil) 10))               
               
                         
               
