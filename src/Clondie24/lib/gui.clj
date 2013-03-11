(ns Clondie24.lib.gui
    (:require [Clondie24.lib.util :as ut]
              [Clondie24.lib.core :as core]
              [Clondie24.lib.srepl :as sre]
              [seesaw.core :as ssw]
              [seesaw.chooser :as choo] 
              [org.dipert.swingrepl.main :as swrepl])
    (:import  [java.awt AlphaComposite Graphics Graphics2D Toolkit]
              [java.awt.event MouseEvent]
              [javax.swing SwingWorker UIManager]) )
              
;-------------------------------------<SOURCE-CODE>--------------------------------------------------------------------
;----------------------------------------------------------------------------------------------------------------------    
(def brand-new {:selection nil
                :highlighting? false
                :pruning? false
                :block? false 
                :hint nil
                :whose-turn "Yellow"})

(def knobs "Various knobs for the gui. Better keep them together."
(atom brand-new))
      
(definline knob! [k nv]
`(swap! knobs assoc ~k ~nv)) 

(defmacro with-block [& blocking-jobs]
`(try (knob! :block? true)  ~@blocking-jobs
 (catch Exception e# (.getMessage e#)) 
 (finally (knob! :block? false))))    

(def curr-game (promise))

(defn refresh [& {:as knobs}]
  (doseq [[k v] knobs]
    (knob! k v)))
 
(defmacro reset-knobs! []
`(reset! knobs brand-new)) 

(definline turn [dir] ;direction of the player who just moved
`(if (pos? ~dir) "Yellow " "Black "))

(defmacro with-worker 
"Starts a background worker thread with busy cursor on component c while busy. 
 When no storage is provided you must call .get on the result to get the value of 'job' (if it returns anything)." 
[c storage & jobs]
`(do (ssw/config! ~c :cursor :wait)
 (doto (proxy [SwingWorker] []
       (doInBackground [] (if (nil? ~storage) ~@jobs 
                              (knob! ~storage ~@jobs)))
       (done [] (ssw/repaint! ~c) 
                (ssw/config! ~c :cursor :default)))       
 (.execute))))
 
(defmacro with-busy-cursor 
"Starts a future to execute some jobs with busy cursor whilst busy, on component c. 
 Optionally if we want a result back we need to provide a place to save it.
 3 auxilliary slots are provided in knobs in case you need some."
 [c storage-key & jobs] 
 `(future (ssw/config! ~c :cursor :wait) 
             (knob! ~storage-key (with-block ~@jobs))
      (ssw/invoke-later (ssw/repaint! ~c) 
                        (ssw/config! ~c :cursor :default)
                        (.. Toolkit getDefaultToolkit beep))))
      

(defn clear! "Clears everything (history and current board)." [] 
(do (reset-knobs!)
(->  @curr-game
     :board-atom
     (reset! (peek (core/clear-history!))))) ) ;(peek []) = nil
    
(defn undo! "Go back a state." []
(->  @curr-game
     :board-atom
     (reset! (peek (core/undo!)))))    

(defn identify-p 
"Returns the piece that corresponds to the coordinates on this board." 
[m b cl-coords]
(let [balancer (ut/balance :down (:tile-size @curr-game))
      [bx by] (mapv balancer cl-coords)
      l-pos (core/translate-position bx by m)
      f-piece (get b l-pos)]
(when-not (nil? f-piece) f-piece)))

(declare canvas status-label hint show-repl!)

            
(defn make-menubar 
"Constructs and returns the entire menu-bar." []
(let [a-new (ssw/action :handler (fn [e]  (reset-knobs!) ((:game-starter @curr-game) false) 
                                          (ssw/config! status-label :text "Game on! Yellow moves first...")
                                          (knob! :block? false)
                                          (ssw/repaint! canvas))
                        :name (str "New " (:name @curr-game)) 
                        :tip  "Start new game." 
                        :key  "menu N")                           
      a-save (ssw/action :handler (fn [e] (choo/choose-file :type :save
                                                            :filters [["Clojure-data" ["clo"]]] ;;use java serialisation for now
                                                            :success-fn (fn [_ f] (ut/serialize! @core/board-history f))))
      
                        :name "Save" 
                        :tip "Save a game to disk." 
                        :key "menu S")
      a-load (ssw/action :handler (fn [e] 
                                    (when-let [f (choo/choose-file :filters [["Clojure-data" ["clo"]]])]  
                                    (reset! core/board-history (ut/deserialize! f)) ;;use java serialisation for now
                                    (ssw/repaint! canvas)))
                        :name "Load" 
                        :tip  "Load a game from disk." 
                        :key  "menu L")
      a-quit (ssw/action :handler (fn [e] (when-let [repl-var (ns-resolve 'Clondie24.lib.gui 'Clondie24-nREPL)] 
                                            (try (sre/stop (var-get repl-var)) 
                                            (catch Exception e nil)) )
                                          (System/exit 0)) 
                         :name "Quit" 
                         :tip  "Close arena" 
                         :key  "menu Q")                  
      a-pref (ssw/action :handler (fn [e] (ssw/alert "Not implemented!")) 
                         :name "Preferences" 
                         :tip  "Show options" 
                         :key  "menu O")
      a-details (ssw/action :handler (fn [e] (ssw/alert "Not implemented!")) 
                            :name "Details" 
                            :tip  "Show info abou your PC."
                            :key  "menu I")
      lo-repl (ssw/action   :handler (fn [e] (show-repl!)) 
                            :name "REPL" 
                            :tip  "Show a swing-based local REPL") 
      re-repl (ssw/action   :handler (fn [e] (sre/defserver Clondie24-nREPL  8989 false) 
                            (ssw/alert (str "nREPL server is up and running! Make a note of the following:\nI.P. = " (ut/external-ip) "\nPort = 8989\nName = Clondie24-nREPL")) )    
                            :name "nREPL server" 
                            :tip  "Start nREPL server that accepts remote clients.")
      a-bout    (ssw/action :handler (fn [e] (ssw/alert "Not implemented!")) 
                           :name "About" 
                           :tip  "A few words." 
                           :key  "menu A")]   
(ssw/menubar :items 
   [(ssw/menu :text "Game"    :items [a-new a-save a-load a-quit])
    (ssw/menu :text "Options" :items [a-pref])
    (ssw/menu :text "Tools"   :items [lo-repl re-repl])
    (ssw/menu :text "Help"    :items [a-details a-bout])]) ))

(defn draw-grid [c ^Graphics g]
  (let [w (ssw/width c)
        h (ssw/height c)
        ts (:tile-size @curr-game)]
    (doseq [x (range 0 w ts)]
      (.drawLine g x 0 x h))
    (doseq [y (range 0 h ts)]
        (.drawLine g 0 y w y))))
        
(defn draw-images [^Graphics g]
(when (seq @core/board-history) 
(let [b  (peek @core/board-history) ;(:board-atom @curr-game)
      ps (remove nil? b) 
      balancer (ut/balance :up (:tile-size @curr-game))]
  (doseq [p ps]
  (let [[bx by] (mapv balancer (if (vector? (:position p)) (:position p) 
                                 (ut/Point->Vec (:position p))));the balanced coords
         pic ((:image p))]  ;the actual picture
    (.drawImage g pic bx by nil)))))) ;finally call g.drawImage() 
     
    
(defn highlight-rects [^Graphics g]
  (let [sel (:selection @knobs)
        tile-size (:tile-size @curr-game)]
 (when (and (not (nil? sel)) (or (:hint @knobs) 
                                 (:highlighting? @knobs))) 
 (let [pmvs (if-let [h (:hint @knobs)] (list (:move h)) ;expecting a hint?
                (core/getMoves sel (peek @core/board-history) nil)) ;getMoves of selected piece
       balancer (ut/balance :up tile-size)]
   (doseq [m pmvs]
     (let [[x y :as end-pos] (:end-pos m)
           [rx ry] (mapv balancer (if (sequential? x) x end-pos))]
     (.setColor g (ut/predefined-color 'green))
     (.setComposite ^Graphics2D g (AlphaComposite/getInstance 
                                   AlphaComposite/SRC_OVER (float 0.5)))
     (.fillRect g rx ry tile-size tile-size)))))))           
        
(defn draw-tiles [d ^Graphics g]
  (let [tile-size (:tile-size @curr-game)]  
(when (:alternating-colours @curr-game)
  (doseq [[[x y] c] (:tiles @curr-game)]
       (.setColor g c)
       (.fillRect g x y tile-size tile-size)) )
 (draw-grid d g) 
 (draw-images g)
 (highlight-rects g)))
 
(defmulti canva-react (fn [cg _] (:name cg))) ; (fn [_] (:name @curr-game))) ;ignore the argument  (fn [cg _] (:name cg))
(defmethod canva-react 'Chess [_ ^MouseEvent e]
(let [spot  (vector (.getX e) (.getY e)) ;;(seesaw.mouse/location e)
      piece (identify-p (:mappings @curr-game) (peek @core/board-history) spot)
      sel   (:selection @knobs)
      le-loc (mapv (ut/balance :down) spot)]
(cond 
  (or
    (and (nil? sel) (not (nil? piece)))           
    (and (not (nil? sel)) (= (:direction piece) (:direction sel)))) 
       (do (refresh :highlighting? false 
                      :hint nil
                      :selection piece)
          (ssw/repaint! canvas))
  (nil? sel) nil ; if selected piece is nil and clicked loc is nil then do nothing
:else (when-let [sel-move (some #(when (or (= le-loc (:end-pos %))
                                             (= le-loc (first (:end-pos %)))) %) 
                                (core/getMoves (:selection @knobs) (peek @core/board-history) true))]
   (core/execute! sel-move (:board-atom @curr-game)) 
      (when-let [res ((:referee-gui @curr-game) (peek @core/board-history))];check if we have a winner 
       (ssw/alert (str "GAME OVER...\n " res))
       (knob! :block? true)) ;block movements if someone won               
       (refresh :whose-turn (turn (get-in @knobs [:selection :direction]))
                :highlighting? false
                :hint nil
                :selection nil);;;;;;;;;;;;
       (ssw/repaint! canvas)
       (ssw/config! status-label :text (str (:whose-turn @knobs) "moves next..."))))))      
            
 
(def canvas "The paintable canvas - our board"
 (ssw/canvas
    :paint draw-tiles
    :id :canvas
    :listen [:mouse-clicked (fn [^MouseEvent e] (when (and  (not (:block? @knobs)) 
                                                            (realized? curr-game)) 
                                                            ((partial canva-react @curr-game) e)))]
    ;:background "#222222"; no need for background anymore
    ))

(def status-label (ssw/label :id :status :text "Ready!"))
    
(defn arena "Constructs and returns the entire arena frame." []
 (ssw/frame
    :title "Clondie24 Arena"
    :size  (:arena-size @curr-game)
    :resizable? false
    :on-close :dispose
    :menubar  (make-menubar)                   
    :content  (ssw/border-panel
               :border 10
               :hgap 10
               :vgap 10
               :north  (ssw/horizontal-panel :items 
                       [(ssw/button :text "Undo"  :listen [:action (fn [e] (when-not (:block? @knobs) 
                                                                           (do (refresh :highlighting? false 
                                                                                        :hint nil) 
                                                                               (undo!) (ssw/repaint! canvas))))]) [:fill-h 10] 
                        (ssw/button :text "Clear" 
                                    :listen [:action 
                                             (fn [e] (when-not (:block? @knobs)
                                                      (do (refresh :highlighting? false 
                                                                   :hint nil) 
                                                               (clear!) (ssw/repaint! canvas))))]) [:fill-h 8]
                        (ssw/button :text "Available Moves" 
                                    :listen [:action (fn [e] 
                                                       (when-not (:block? @knobs) 
                                                           (do (refresh :highlighting? true 
                                                                        :hint nil) 
                                                                (ssw/repaint! canvas))))]) [:fill-h 8]
                        (ssw/button :text "Hint" 
                                    :listen [:action (fn [e] (when-not (:block? @knobs)
                                                      (knob! :highlighting? false) 
                                                        (with-busy-cursor canvas 
                                                           :hint (hint (:pruning? @knobs)))))]) [:fill-h 8]
                       (ssw/radio    :text "Sport" 
                                     :selected? false 
                                     :listen [:action (fn [e] 
                                                       (when-not (:block? @knobs)
                                                        (knob! :pruning? (not (:pruning? @knobs)))))]) [:fill-h 5]]) 
               :center canvas
               :south  status-label)))
              
               
(defn hint "Ask the computer for a hint." 
[pruning?] 
 ((:hinter @curr-game) 
    (get-in @knobs [:selection :direction]) 
    (peek @core/board-history) pruning?))
          

(defn set-laf! "Set look and feel of the ui, provided its name as a string."  
[laf-name]
(when-let [lf1 (some #(when (= laf-name (.getName %)) %)
                     (UIManager/getInstalledLookAndFeels))]
 (UIManager/setLookAndFeel (.getClassName lf1))))
 

(defmethod canva-react 'Checkers [_ ^MouseEvent e]
(let [spot  (vector (.getX e) (.getY e)) ;;(seesaw.mouse/location e)
      board (peek @core/board-history)
      piece (identify-p (:mappings @curr-game) board spot)
      sel   (:selection @knobs)]
(cond 
  (or
    (and (nil? sel) (not (nil? piece)))           
    (and (not (nil? sel)) (= (:direction piece) (:direction sel)))) 
         (do (refresh :highlighting? false 
                      :hint nil
                      :selection piece)
             (ssw/repaint! canvas))
  (nil? sel) nil ; if selected piece is nil and lcicked loc is nil then do nothing
:else 
  (let [sen-spot (mapv (ut/balance :down) spot)
        sel-move (core/dest->Move board sel sen-spot (:mover @curr-game))]
(when 
 (and (some #(when (= sen-spot (:end-pos %)) %)
            ((:team-moves @curr-game) board (:direction sel) false))
  (some #(when (= sen-spot (:end-pos %)) %)
        (core/getMoves sel board nil)))
   (core/execute! sel-move (:board-atom @curr-game)) 
   (refresh :whose-turn (turn (:direction sel))
            :highlighting? false
            :hint nil
            :selection nil)
   (ssw/config! status-label :text (str (:whose-turn @knobs) "moves..."))
    (ssw/repaint! canvas))))))

(defn request-canva-repaint []
(ssw/repaint! canvas))

(definline alert! [s]
`(ssw/alert ~s))

(defn replay
  "Replays a saved-game (history) on screen." 
  [history]
 ((:game-starter @curr-game) false)
  (doseq [b history]
    (reset! (:board-atom @curr-game) b)
    (ssw/repaint! canvas)
    (Thread/sleep 600))) 

(defn resize! 
"Handy macro for resizing the frame outside the gui when we don't have access to swing." 
[f w h]
(ssw/config! f :size [w :by h]))
     
                                                       
(defn show-gui! "Everything starts from here." [game-map]
 (reset-knobs!)      ;start from scratch 
 (set-laf! "Nimbus") ;try to look nice
  (deliver curr-game game-map) ;firstly make the gui aware of what game we want it to display
   (ssw/invoke-later 
     (doto (arena) ssw/show!)))
                                                
               
(defn show-repl! "Pop up a local working REPL"
 ([opt-map]  (swrepl/make-repl-jframe opt-map))
 ([] (swrepl/make-repl-jframe)))        
