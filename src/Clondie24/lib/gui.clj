(ns Clondie24.lib.gui
    (:require [Clondie24.lib.util :as ut]
              [Clondie24.lib.core :as core] 
              [seesaw.core :as ssw]
              [seesaw.chooser :as choo])
    (:import  [java.awt AlphaComposite Graphics Graphics2D]
              [java.awt.event MouseEvent]
              [javax.swing SwingWorker]) )
              
;-------------------------------------<SOURCE-CODE>--------------------------------------------------------------------
;----------------------------------------------------------------------------------------------------------------------    
(def brand-new {:selection nil
                :highlighting? false
                :busy? false 
                :hint nil
                :aux1 nil
                :aux2 nil
                :aux3 nil
                :whose-turn "Yellow"})

(def knobs "Various knobs for the gui. Better keep them together."
(atom brand-new))
      
(defmacro knob! [k nv]
`(swap! knobs assoc ~k ~nv))     

(def curr-game (promise))

(definline refresh [m]
`(doseq [e# ~m]
 (knob! (first e#) (second e#))))
 
(defmacro reset-knobs! []
`(reset! knobs brand-new)) 

(definline turn [dir] ;direction of the player who just moved
`(if (pos? ~dir) "Yellow " "Black "))

(defmacro with-worker 
"Starts a background worker thread with busy cursor on component c while busy. 
 When no storage is provided you must call .get on the result to get the value of 'job' (if it returns anything)." 
[c job storage]
`(do (ssw/config! ~c :cursor :wait)
 (doto (proxy [SwingWorker] []
       (doInBackground [] (if (nil? ~storage) ~job 
                              (knob! ~storage ~job)))
       (done [] (ssw/repaint! ~c) 
                (ssw/config! ~c :cursor :default)))       
 (.execute))))
 
(defmacro with-busy-cursor 
"Starts a future to execute a job with busy cursor whilst busy, on component c. 
 Optionally if we want a result back we need to provide a place to save it.
 3 auxilliary slots are provided in knobs in case you need some."
 [c job storage-key] 
`(do (ssw/config! ~c :cursor :wait)
  (future (if (nil? ~storage-key) ~job
  (do (knob! ~storage-key ~job) 
      (ssw/invoke-later (ssw/repaint! ~c) 
                        (ssw/config! ~c :cursor :default)))))))
      

(defn clear! "Clears everything (history and current board)." [] 
(do (reset-knobs!)
(->  @curr-game
     (:board-atom)
     (reset! (peek (core/clear-history!))))) ) ;(peek []) = nil
    
(defn undo! "Go back a state." []
(->  @curr-game
     (:board-atom)
     (reset! (peek (core/undo!)))))    

(defn balance [how]
(condp = how
   :up   (partial * 50)
   :down (comp int #(/ % 50))))

(defn identify-p 
"Returns the piece that corresponds to the coordinates on this board." 
[m b cl-coords]
(let [balancer (balance :down)
      r-ids (map balancer cl-coords)
      l-pos (core/translate-position (first r-ids) (second r-ids) m)
      f-piece (get b l-pos)]
(when-not (nil? f-piece) f-piece)))

(declare canvas status-label hint)

            
(defn make-menubar 
"Constructs and returns the entire menu-bar." []
(let [a-new (ssw/action :handler (fn [e] (do (reset-knobs!) (apply (:game-starter @curr-game) '(false)) 
                                          (ssw/config! status-label :text "Game on! Yellow moves first...")))
                        :name (str "New " (:name @curr-game)) 
                        :tip  "Start new game." 
                        :key  "menu N")                           
      a-save (ssw/action :handler (fn [e] (choo/choose-file :type :save
                                                            :filters [["Clojure-data" ["clo"]]] ;;use the reader for now
                                                            :success-fn (fn [_ f] (ut/data->string @core/board-history f))))
      
                        :name "Save" 
                        :tip "Save a game to disk." 
                        :key "menu S")
      a-load (ssw/action :handler (fn [e]  (when-let [f (choo/choose-file :filters [["Clojure-data" ["clo"]]])]  
                                           (reset! core/board-history (ut/string->data f))));;use the reader for now
                        :name "Load" 
                        :tip  "Load a game from disk." 
                        :key  "menu L")
      a-quit (ssw/action :handler (fn [e] (System/exit 0)) 
                        :name "Quit" 
                        :tip  "Exit Clondie24" 
                        :key  "menu Q")                  
      a-pref (ssw/action :handler (fn [e] (ssw/alert "Not implemented!")) 
                        :name "Preferences" 
                        :tip  "Show options" 
                        :key  "menu O")
      a-details (ssw/action :handler (fn [e] (ssw/alert "Not implemented!")) 
                           :name "Details" 
                           :tip  "Show info abou your PC."
                           :key  "menu I")
      a-bout    (ssw/action :handler (fn [e] (ssw/alert "Not implemented!")) 
                           :name "About" 
                           :tip  "A few words." 
                           :key  "menu A")]   
(ssw/menubar :items 
   [(ssw/menu :text "Game"    :items [a-new a-save a-load a-quit])
    (ssw/menu :text "Options" :items [a-pref])
    (ssw/menu :text "Help"    :items [a-details a-bout])]) ))

(defn draw-grid [c ^Graphics g]
  (let [w (ssw/width c)
        h (ssw/height c)]
    (doseq [x (range 0 w 50)]
      (.drawLine g x 0 x h))
    (doseq [y (range 0 h 50)]
        (.drawLine g 0 y w y))))
        
(defn draw-images [^Graphics g]
(when (seq @core/board-history) 
(let [b  (peek @core/board-history) ;(:board-atom @curr-game)
      ps (remove nil? b) 
      balancer (balance :up)]
  (doseq [p ps]
  (let [[bx by] (vec (map balancer (if (vector? (:position p)) (:position p) (ut/Point->Vec (:position p)))));the balanced coords
         pic (:image p)]  ;the actual picture
    (.drawImage g pic bx by nil)))))) ;finally call g.drawImage() 
     
    
(defn highlight-rects [^Graphics g]
 (when (and (not (nil? (:selection @knobs))) (or (:hint @knobs) 
                                                 (:highlighting? @knobs))) 
 (let [pmvs (if-let [h (:hint @knobs)] (list (get-in h [:move :end-pos])) 
                (core/getMoves (:selection @knobs) (peek @core/board-history) true))
       balancer (balance :up)]
   (doseq [m pmvs]
     (let [[rx ry] (vec (map balancer m))]
     (.setColor g (ut/predefined-color 'green))
     (.setComposite ^Graphics2D g (AlphaComposite/getInstance 
                                   AlphaComposite/SRC_OVER (float 0.5)))
     (.fillRect g rx ry 50 50))))))           
        
(defn draw-tiles [d ^Graphics g]
  (let [w (ssw/width d)
        h (ssw/height d)
        tiles (map vector (for [x (range 0 w 50) 
                                y (range 0 h 50)] [x y]) 
                          (cycle [(ut/hex->color '0xffdead) ;funny colour name!
                                  (ut/hsb->color 0.931 0.863 0.545)
                                  ;(ut/predefined-color 'lightGray) 
                                  #_(ut/predefined-color 'black)]))]  
    (doseq [[[x y] c] tiles]
       (.setColor g c)
       (.fillRect g x y 50 50)) 
 (draw-grid d g) 
 (draw-images g)
 (highlight-rects g)))
               
(defn canva-react [^MouseEvent e]
(let [spot  (vector (.getX e) (.getY e)) ;;(seesaw.mouse/location e)
      piece (identify-p (:mappings @curr-game) (peek @core/board-history) spot)
      sel   (:selection @knobs)]
(cond 
  (or
    (and (nil? sel) (not (nil? piece)))           
    (and (not (nil? sel)) (= (:direction piece) (:direction sel)))) 
         (do (refresh {:highlighting? false 
                       :hint nil
                       :selection piece})
             (ssw/repaint! canvas))
  (nil? sel) nil ; if selected piece is nil and lcicked loc is nil then do nothing
  (some #{(vec (map (balance :down) spot))} (core/getMoves (:selection @knobs) (peek @core/board-history) true))
   (do (core/execute! 
       (core/dest->Move (peek @core/board-history) 
                        (:selection @knobs) 
                        (vec (map (balance :down) spot))
                        (:mover @curr-game)) (:board-atom @curr-game)) 
       (refresh {:whose-turn (turn (get-in @knobs [:selection :direction]))
                 :highlighting? false
                 :hint nil
                 :selection nil})
       (ssw/config! status-label :text (str (:whose-turn @knobs) "moves..."))
       (ssw/repaint! canvas)))))
            
 
(defonce canvas
 (ssw/canvas
    :paint draw-tiles
    :id :canvas
    :listen [:mouse-clicked (fn [e] (when-not (:busy? @knobs) 
                                              (canva-react e)))]
    ;:background "#222222"; no need for background
    ))

(def status-label (ssw/label :id :status :text "Ready!"))
    
(defn make-arena 
"Constructs and returns the entire arena frame." []
 (ssw/frame
    :title "Clondie24 Arena"
    :size  [421 :by 502] ;412 :by 462 with border=5
    :resizable? false
    :on-close :exit
    :menubar  (make-menubar)                   
    :content  (ssw/border-panel
               :border 10
               :hgap 10
               :vgap 10
               :north  (ssw/horizontal-panel :items 
                       [(ssw/button :text "Undo"  :listen [:action (fn [e] (when-not (:busy? @knobs) 
                                                                           (do (refresh {:highlighting? false 
                                                                                         :hint nil}) 
                                                                               (undo!) (ssw/repaint! canvas))))]) [:fill-h 10] 
                        (ssw/button :text "Clear" :listen [:action (fn [e] (when-not (:busy? @knobs)
                                                                           (do (refresh {:highlighting? false 
                                                                                         :hint nil}) 
                                                                               (clear!) (ssw/repaint! canvas))))]) [:fill-h 10]
                        (ssw/button :text "Available Moves" :listen [:action (fn [e] (when-not (:busy? @knobs) 
                                                            (do (refresh {:highlighting? true 
                                                                          :hint nil}) 
                                                                (ssw/repaint! canvas))))]) [:fill-h 10]
                        (ssw/button :text "Hint" :listen [:action (fn [e] (when-not (:busy? @knobs)
                                                     (do (refresh {:highlighting? false}) 
                                                         (with-busy-cursor canvas 
                                                          (hint (:pref-depth @curr-game)) :hint))))]) [:fill-h 10]])
               :center canvas
               :south  status-label)))
              
               
(defn hint "Ask the computer for a hint." 
[depth] 
(when-not (:busy? @knobs) ;ONLY IF NOT 'THINKING'
(do (refresh {:busy? true}) 
 (apply (:hinter @curr-game) 
          (list (get-in @knobs [:selection :direction]) (peek @core/board-history) depth)))))
      
 
                                                       
(defn show-gui! "Everything starts from here." [game-map] 
  (do #_(ssw/native!)
        (deliver curr-game game-map) ;firstly make the gui aware of what game we want it to display
        (ssw/invoke-later 
          (doto (make-arena) ssw/show!))))
                                                
               
        
