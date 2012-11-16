(ns Clondie24.games.tictactoe
    (:use [clojure.core.match :only (match)]) 
    (:require [Clondie24.lib.util :as ut] 
              [Clondie24.lib.core :as core]
              [Clondie24.lib.search :as s]
              ;[Clondie24.lib.rules :as rul]
              [Clondie24.lib.gui :as gui]
              ;[enclog.nnets :as ai]
              ;[enclog.training :as evol]
              ;[enclog.normalization :as norm] :verbose :reload
              )
    #_(:import  [encog_java.customGA CustomNeuralGeneticAlgorithm 
                                   CustomGeneticScoreAdapter Referee])
)

;----------------------------------------<SOURCE>--------------------------------------------------------------------
;----------------------------------------<CODE>----------------------------------------------------------------------


(def board-mappings-tic-tac-toe core/mappings-3x3)
(def winning-sets [[0 1 2] [3 4 5] [6 7 8] [0 3 6] [1 4 7] [2 5 8] [0 4 8] [2 4 6]])
(def dir->shape {'X -1 'O 1}) ;;we use direction to represent shape {-1 'X 1 'O}
(defn shape->dir [s] (get s dir->shape))
(def turns (->> (cycle ['X 'O]) 
                (take 20)
                atom))
(defrecord Player [brain shape searcher])
(declare details starting-board)


(definline tic-tac-toe-best-move [dir b n scorer] 
`(s/go ~dir ~b ~n ~scorer))

(def current-tictactoeItems 
(add-watch (atom nil) 
 :log (partial core/log-board core/board-history))) 
             
(defrecord TicTacToePiece [shape ^java.awt.Image image position]
 core/Piece   
 (update-position [this np] (TicTacToePiece. shape image np)) 
 (die [this]    (throw (IllegalStateException. "Tic-tac-toe pieces cannot die!"))) 
 (promote [this np] (throw (IllegalStateException. "Tic-tac-toe pieces cannot be promoted!"))) 
 (getListPosition [this] (core/translate-position (first position) (second position) board-mappings-tic-tac-toe))
 (getPoint [this] (ut/make-point position))
 (getMoves [this b _] (loop [i 0 curr b
                             vacant (list)]
                       (if (empty? curr) vacant   
                       (recur (inc i) (rest curr) 
                        (if (nil? (first curr)) 
                            (conj vacant (core/translate-position i board-mappings-tic-tac-toe)) vacant)))))
 Object
 (toString [this] 
   (println "TicTacToe (" (str shape) ") at position:" (core/getListPosition this) " ->" position)))            
               
(defn move 
"The function responsible for placing tic-tac-toe pieces. 
 Returns the resulting board without making any state changes. " 
[board p coords]
  (let [np (core/update-position p coords)]     
    (assoc board (core/getListPosition np) np))) 
       
    
(defn full-board? [b]
 (not-any? nil? b))                 

(defn winner 
"Returns the winning shape ['O , 'X] or nil. " 
[b]
(let [combs (for [[x y z :as rows] winning-sets]
              (match (into [] (clojure.core.reducers/map #(:shape (get b %)) rows)) ;[(:shape (get b x)) (:shape (get b y)) (:shape (get b z))]
                ['X 'X 'X] -1
                ['O 'O 'O]  1
              :else       0))]  ;;(0 0 0 1  0 0 0 0)             
(if (some #{1} combs) 'O
 (when (some #{-1} combs) 'X)))) 
 
(defn game-over? [b]
 (if (or (winner b)
         (full-board? b)) 
 true false))
 
(defn start-tictactoe! [_] 
"Start a tic-tac-toe game. Returns the starting-board."
(core/clear-history!) ;empty board-history
 (deliver s/curr-game details)
  (reset! current-tictactoeItems starting-board)) 
  
 (defn team-moves [b s _ _] 
 (clojure.core.reducers/map 
     #(core/dest->Move b nil % move) 
 (-> (TicTacToePiece. s nil nil) 
     (core/getMoves b nil))))  
                
(def details "The map that describes the game of tic-tac-toe."
              {:name 'Tic-Tac-Toe
               :players 2 
               :chunking 5
               :images {'X (ut/make-image "images/133px/tic-tac-toe-X.png") 
                        'O (ut/make-image "images/133px/tic-tac-toe-O.png")}
               :characteristics [:shape :image :position]      
               :board-size 9
               :arena-size [420 :by 505]
               :tiles (map vector (for [x (range 0 420 133) 
                                        y (range 0 505 133)] [x y]) 
                                  (cycle nil)) ;;there are no colours 
               :tile-size 133
               :total-pieces 0
               :mover move
               :team-moves team-moves
               ;:referee-gui gui-referee
               :referee-jit winner
               ;:naive-scorer core/score-chess-naive
               :pref-depth 10
               :board-atom current-tictactoeItems
               :game-starter start-tictactoe!
               :hinter tic-tac-toe-best-move 
               :record-name "Clondie24.games.tic-tac-toe.TicTacToePiece" ;;fully qualified name
               :mappings board-mappings-tic-tac-toe})
                
(def starting-board (core/empty-board details)) 

(defn ttt-rand-move [shape b _ _] ;;need same arity as 'chess-best-move'
  (let [p (TicTacToePiece. shape nil nil) 
        all-moves (vec (core/getMoves p b nil))]
    {:move (core/dest->Move b p (get all-moves (rand-int (count all-moves))) move)})) 

(defn tournament
"Starts a tournament between the 2 players (p1, p2). If there is no winner, returns the entire history (vector) of 
 the tournament after 100 moves. If there is a winner, a 2d vector will be returned containing both the history(1st item) 
 and the winner (2nd item)." 
[sb depth p1 p2 & {:keys [limit]
                   :or   {limit 20}}]
(reduce 
 (fn [history player] 
  (let [cb (peek history) 
        win-dir (winner cb)]
    (if (game-over? cb) (reduced (vector history (when win-dir 
                                                   (if (= win-dir (:shape p1)) p1 p2))))
    (conj history (->> player
                      :brain
                      ((:searcher player) (:shape player) cb depth) 
                      :move
                      core/try-move))))) 
 [sb] (take limit (cycle [p1 p2])))) ;;100 moves each should be enough

(defmethod gui/canva-react 'Tic-Tac-Toe [^java.awt.event.MouseEvent e]
(let [spot  (vector (.getX e) (.getY e))
      bspot (vec (map (ut/balance :down (:tile-size details)) spot))]
(when (core/vacant? board-mappings-tic-tac-toe @current-tictactoeItems bspot)
(let [turn  (first @turns)
      image (get (:images details) turn)] 
(core/execute! 
  (core/dest->Move @current-tictactoeItems (TicTacToePiece. turn image nil) bspot move) current-tictactoeItems)
  (swap! turns next)
  (gui/request-canva-repaint)
(if (full-board? @current-tictactoeItems) (gui/alert! "GAME OVER!")
  (when-let [win-dir (winner @current-tictactoeItems)] 
    (do (gui/knob! :block? true) (gui/alert! (str win-dir " WON!")))))))))

(defn random-player [shape]
(Player. nil shape ttt-rand-move))            
               
(defn score-ttt-naive [s b]
 (if-let [w (winner b)] 
    (if (= w s) 1 -1)
 0))
;(tournament starting-board 9 (random-player 'X) (random-player 'O))               
               
(defn -main [& args]
(gui/show-gui! details)
#_(tic-tac-toe-best-move 1 (start-tictactoe! nil) 10 score-ttt-naive))               
               
                         
               
