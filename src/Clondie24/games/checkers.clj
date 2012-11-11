(ns Clondie24.games.checkers
     (:require [Clondie24.lib.util :as ut] 
               [Clondie24.lib.core :as core]
               [Clondie24.lib.search :as s]
               [Clondie24.lib.rules :as rul]
               [Clondie24.lib.gui :as gui] :verbose :reload)
)
;----------------------------------------<SOURCE>--------------------------------------------------------------------
;----------------------------------------<CODE>----------------------------------------------------------------------

(def board-mappings-checkers core/mappings-8x8)

(declare details)
            
(def checkers-images "All the checkers images paired up according to rank."
(zipmap '(:soldier :prince)
                     [{1 (ut/make-image "images/50px/png/checker-black.png")
                      -1 (ut/make-image "images/50px/png/checker-white.png")}
                      {1 (ut/make-image "images/50px/png/prince-black.png")
                      -1 (ut/make-image "images/50px/png/prince-white.png")}]))
                       
(def checkers-moves {:soldier  (partial rul/checker-moves board-mappings-checkers) 
                     :prince   (partial rul/prince-moves board-mappings-checkers)})
(def prince-row {1 7 
                -1 0})                     
                     
#_(defn rank->moves 
"Returns all legal moves of piece p depending on rank of p (excluding pawn). Will be called only once to buffer the moves." 
[p]
(let [pos (:position p)           
      r (keyword (:rank p))
      d (:direction p)]      
(apply (r checker-moves) pos))) ;will return a fn which is called with current x and y

(definline jump? [s e]
`(-> (Math/abs (- (first ~e) 
                  (first ~s))) 
      (rem  2)
      (= 0))) 
                                 
                                                            
#_(defn between [sp ep]
(ut/walk (ut/resolve-direction sp ep) sp))

(defn move [board p coords]
(if (jump? (:position p) coords)
(let [newPiece (core/update-position p coords)
      old-pos  (core/getListPosition p)
      new-pos  (core/getListPosition newPiece)]
(-> board 
     (transient)  
     (assoc! old-pos nil) 
     (assoc! new-pos newPiece) 
     (assoc! (let [[bx by] (ut/walk (ut/resolve-direction (:position p) coords) (:position p))]  ;the piece in between
                (core/translate-position bx by board-mappings-checkers)) nil)
     (persistent!)))
(core/move board p coords))) ;;use the one from core if there is no kill

     
     
(definline team-moves "Jumps have priority." [b dir & more]
`(let [all# (into [] (core/team-moves ~b ~dir move nil)) ;;our specific move-fn
      jumps# (filter #(jump? (get-in % [:p :position]) (:end-pos %))  all#)]
 (if (seq jumps#) jumps# all#)))      

(defrecord CheckersPiece [^java.awt.Image image
                          ^clojure.lang.PersistentVector position 
                           rank value direction] 
 core/Piece 
 (update-position [this np] (if (and (= (second np) (get prince-row direction))
                                     (= rank 'soldier)) (core/promote this np) 
                                (->CheckersPiece image np rank value direction {:alive true} nil)))
 (die     [this] (vary-meta this assoc :alive false)) ;communicate death through meta-data 
 (promote [this np] (->CheckersPiece (get-in checkers-images [:prince direction]) np 'prince 3 direction {:alive true} nil)) ; a checker is promoted to prince (king)
 (getListPosition [this] (core/translate-position  (first  position) 
                                                   (second position) board-mappings-checkers))
 (getPoint [this] (ut/make-point position))
 (getMoves [this b _] 
                  (let [[x y] position]
                  ;(map #(with-meta % {:jump? (jump? position %)}) 
                   (((keyword rank) checkers-moves) b x y direction)))
 Object
 (toString [this] 
   (println "Checker (" rank ") at position:" (core/getListPosition this) " ->" position)) )

(defn starting-checkers
"Will construct a set of initial checkers (12). opponent? specifies the side of the board where the pieces should be placed (true for north false for south)." 
[opponent?]           
(if opponent?               
(map #(when-not (nil? %) 
         (CheckersPiece. (get-in checkers-images [:soldier 1])
         (core/translate-position % board-mappings-checkers) 'soldier 1 1 {:alive true} nil)) 
          '(nil 1 nil 3 nil 5 nil 7 8 nil 10 nil 12 nil 14 nil nil 17 nil 19 nil 21 nil 23))
(map #(when-not (nil? %)  
          (CheckersPiece. (get-in checkers-images [:soldier -1]) 
          (core/translate-position % board-mappings-checkers) 'soldier 1 -1 {:alive true} nil))
          '(40 nil 42 nil 44 nil 46 nil nil 49 nil 51 nil 53 nil 55 56 nil 58 nil 60 nil 62 nil)))) 
                    

(def current-checkers 
"This is list that keeps track of moving checkers. Is governed by an atom and it changes after every move. All changes are being logged to 'board-history'. Starts off as nil but we can always get the initial arrangement of any game from core."
(add-watch (atom nil) 
 :log (partial core/log-board core/board-history)))
 
 
(defn start-checkers! [fast?] 
"Start a chess-game. Returns the starting-board."
(core/clear-history!) ;empty board-history
  (deliver s/curr-game details)
  (reset! current-checkers
  (if fast? (into-array  (core/starting-board details))
                         (core/starting-board details)))
 );mandatory before game starts 
                      
(defn score-by-count  [b dir] 
(let [ hm (into [] (core/gather-team b dir))
       aw (into [] (core/gather-team b (unchecked-negate dir)))]
 (unchecked-subtract (count hm) 
                     (count aw))))
                     
(defn checkers-best-move [dir b n] 
(s/go dir b n))                    
                       
(def details "Returns a map that describes the game of checkers."
              {:name 'Checkers
               :players 2
               :chunking 1
               :images checkers-images
               :characteristics [:image :position :rank :value :direction]  
               :board-size 64
               :tile-size 50 
               :alternating-colours [(ut/hex->color '0xffdead) ;funny colour name!
                                     (ut/hsb->color 0.931 0.863 0.545)] 
               :total-pieces 48 ;24 ;;temporary hack
               :obligatory-move 'jump
               :rel-values {:soldier 1 :prince 3}
               :mover move
               :team-moves team-moves
               :hinter checkers-best-move
               :scorer score-by-count
               :pref-depth 6
               :board-atom  current-checkers
               :record-name "Clondie24.checkers.CheckersPiece"
               :game-starter start-checkers!
               :mappings board-mappings-checkers
               :north-player-start  (starting-checkers true)
               :south-player-start  (starting-checkers false)})              
            

(def make-checker  (partial core/piece details)) ;better avoiding this indirection (involves reflection) 

                 
(defn to-2d [b] ; the checkers or chess board as 8 rows of 8 columns
(map vec        ;apply 'reverse' to every second item in 1d-board
(map #(%1 %2) (cycle [identity reverse]) (partition 8 b)))) 
;-----------------------------------------------------------------------------------
(comment
(def ^:dynamic red-direction 1)
(def ^:dynamic yellow-direction -1)
(def valid-checkers-positions    (range 32))
(def invalid-checkers-positions  (repeat 32 -1))

(def checkers-1d ;the checkers-board as a list. -1 represents invalid positions
(interleave invalid-checkers-positions 
            valid-checkers-positions))
            
)
            
(defn -main 
"Starts a graphical (swing) Chess game." 
[& args]  
(gui/show-gui! details)
#_(time (s/go -1 (start-checkers! false) 6) #_(println @s/mmm))
#_(time (do (s/go -1 (start-checkers! false) 4) (println @s/mmm))) 
)            
            
            
               
