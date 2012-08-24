(ns Clondie24.lib.gui
    (:require [Clondie24.lib.util :as ut]
              [Clondie24.lib.core :as core] 
              [seesaw.core :as ssw])
    (:import  [java.awt AlphaComposite Graphics Graphics2D]
              [java.awt.event MouseEvent]) )
              
;-------------------------------------<SOURCE-CODE>--------------------------------------------------------------------
;----------------------------------------------------------------------------------------------------------------------    
(def knobs "Various knobs for the gui. Better keep them together."
(atom {:selection nil
       :highlighting? false 
       :hinting? false
      }))
      
(defmacro knob! [k nv]
`(swap! knobs assoc ~k ~nv))     

(def curr-game (promise))

(defn clear! "Clears everything (history and current board)." [] 
(->  @curr-game
     (:board-atom)
     (reset! (peek (core/clear-history!)))) ) ;(peek []) returns nil
    
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
      f-piece (nth b l-pos)]
(when-not (nil? f-piece) f-piece)))

(declare canvas status-label hint)

            
(defn make-menubar 
"Constructs and returns the entire menu-bar." []
(let [a-new (ssw/action :handler (fn [e]  (apply (:game-starter @curr-game) '()) 
                                          (ssw/config! status-label :text "Game on! Yellow moves first..."))
                        :name (str "New " (:name @curr-game)) 
                        :tip  "Start new game." 
                        :key  "menu N")                           
      a-save (ssw/action :handler (fn [e] (ssw/alert "Not implemented!")) 
                        :name "Save" 
                        :tip "Save a game to disk." 
                        :key "menu S")
      a-load (ssw/action :handler (fn [e] (ssw/alert "Not implemented!")) 
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
(let [b  (:board-atom @curr-game)
      ps (remove nil? @b) 
      balancer (balance :up)]
  (doseq [p ps]
  (let [[bx by] (vec (map balancer (:position p)));the balanced coords
         pic (:image p)]  ;the actual picture
    (.drawImage g pic bx by nil)))))) ;finally call g.drawImage() 
     
    
(defn highlight-rects [^Graphics g]
 (when (and (not (nil? (:selection @knobs))) (or (:hinting? @knobs) 
                                                 (:highlighting? @knobs))) 
 (let [pmvs (if (:hinting? @knobs)  (list (get-in (hint) [:move :end-pos]))
                (core/getMoves (:selection @knobs) @(:board-atom @curr-game)))
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
      piece (identify-p (:mappings @curr-game) @(:board-atom @curr-game) spot)
      sel   (:selection @knobs)]
(cond 
  (or
    (and (nil? sel) (not (nil? piece)))           
    (and (not (nil? sel)) (= (:direction piece) (:direction sel)))) 
         (do (knob! :highlighting? false)
             (knob! :hinting? false)   
             (knob! :selection piece)
             (ssw/repaint! canvas))
  (nil? sel) nil ; if selected piece is nil and lcicked loc is nil then do nothing
  (some #{(vec (map (balance :down) spot))} (core/getMoves (:selection @knobs) @(:board-atom @curr-game)))
   (do (core/execute! 
       (core/dest->Move @(:board-atom @curr-game) (:selection @knobs) (vec (map (balance :down) spot))) (:board-atom @curr-game)) 
       (knob! :selection nil)
       (knob! :highlighting? false)
       (knob! :hinting? false)  
       (ssw/repaint! canvas)))))
            
 
(def canvas
 (ssw/canvas
    :paint draw-tiles
    :id :canvas
    :listen [:mouse-clicked (fn [e] (canva-react e))]
    ;:background "#222222"; no need for background
    ))

(def status-label (ssw/label :id :status :text "Ready!"))

(defn hint [] ;NEEDS CHANGING - TESTING ATM
(apply (:hinter @curr-game) (list (get-in @knobs [:selection :direction]) @(:board-atom @curr-game) 2)))
    
(defn make-arena 
"Constructs and returns the entire arena frame" []
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
                       [(ssw/button :text "Undo"  :listen [:action (fn [e] (do (undo!) (ssw/repaint! canvas)))]) [:fill-h 10] 
                        (ssw/button :text "Clear" :listen [:action (fn [e] (do (clear!) (ssw/repaint! canvas)))]) [:fill-h 10]
                        (ssw/button :text "Available Moves" :listen [:action (fn [e]  
                                                            (do (knob! :highlighting? true) (ssw/repaint! canvas)))]) [:fill-h 10]
                        (ssw/button :text "Hint" :listen [:action (fn [e] 
                                                     (do (knob! :hinting? true) (ssw/repaint! canvas)))])  [:fill-h 10]])
               :center canvas
               :south  status-label)))
                              
(defn show-gui! [game-map] 
  (do #_(ssw/native!)
        (deliver curr-game game-map) ;firstly make the gui aware of what game we want it to display
        (ssw/invoke-later 
          (doto (make-arena) ssw/show!))))
                                                
               
        
