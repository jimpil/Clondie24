(ns Clondie24.lib.gui
    (:require [Clondie24.lib.util :as ut]
              [Clondie24.lib.core :as core] 
              [seesaw.core :as ssw]))
;-------------------------------------<SOURCE-CODE>--------------------------------------------------------------------
;----------------------------------------------------------------------------------------------------------------------    
(def curr-game (promise)) 

(defn clear! [] (core/clear-history!))

(declare canvas status-label)

            
(defn make-menubar 
"Constructs and returns the entire menu-bar." []
(let [a-new (ssw/action  :handler (fn [e] (apply (:game-starter @curr-game) '()) 
                                          (ssw/config! status-label :text "Game on! White moves first..."))
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
   [(ssw/menu :text "Game"    :items [a-new a-save a-load])
    (ssw/menu :text "Options" :items [a-pref])
    (ssw/menu :text "Help"    :items [a-details a-bout])]) ))

(defn draw-grid [c g]
  (let [w (ssw/width c)
        h (ssw/height c)]
    (doseq [x (range 0 w 50)]
      (.drawLine g x 0 x h))
    (doseq [y (range 0 h 50)]
        (.drawLine g 0 y w y))))
        
(defn draw-images [g]
(let [b  (:board-atom @curr-game) ;nil
      ps (filter (complement nil?) @b) 
      balancer (partial * 50)]
  (doseq [p ps]
  (let [[bx by] (vec (map balancer (:position p)));the balanced coords
         pic (:image p)]  ;the actual picture
    (.drawImage g pic bx by nil))))) ;finally call g.drawImage()        
        
(defn draw-tiles [d g]
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
 (when (seq @core/board-history) 
               (draw-images g))))
             
 
(def canvas
 (ssw/canvas
    :paint draw-tiles
    :id :canvas
    ;:background "#222222"; no need for background
    ))

(def status-label (ssw/label :id :status :text "Ready!"))
    
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
                       [(ssw/button :text "Undo"  :listen [:action #(ssw/alert "Not implemented!")]) [:fill-h 10] 
                        (ssw/button :text "Clear" :listen [:action #(clear!)])                       [:fill-h 10]
                        (ssw/button :text "Available Moves" :listen [:action #(ssw/alert "Not implemented!")]) [:fill-h 10]
                        (ssw/button :text "Hint" :listen [:action #(ssw/alert "Not implemented!")])  [:fill-h 10]])
               :center canvas
               :south  status-label)))
               
               


(defn show-gui! [game-map] 
  (do  
        (deliver curr-game game-map) ;firstly make the gui aware of what game we want it to display
        (ssw/invoke-later 
          (doto (make-arena) ssw/show!))))
                                                
               
        
