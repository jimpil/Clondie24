(ns Clondie24.gui
    (:require [Clondie24.util :as ut] 
              [seesaw.core :as ss]
))
;-------------------------------------<SOURCE-CODE>--------------------------------------------------------------------
;----------------------------------------------------------------------------------------------------------------------    
           
(defn make-menubar 
"Constructs and returns the entire menu-bar." []
(let [n-chess (ss/action :handler (fn [e] (ss/alert "Not implemented!")) 
                         :name "Chess" 
                         :tip "Start new Chess." 
                         :key "menu C")
      n-checkers (ss/action :handler (fn [e] (ss/alert "Not implemented!")) 
                            :name "Checkers" 
                            :tip "Start new Checkers." 
                            :key "menu D")
      a-save (ss/action :handler (fn [e] (ss/alert "Not implemented!")) 
                        :name "Save as" 
                        :tip "Save a game to disk." 
                        :key "menu S")
      a-load (ss/action :handler (fn [e] (ss/alert "Not implemented!")) 
                        :name "Load" 
                        :tip "Load a game from disk." 
                        :key "menu L")
      a-pref (ss/action :handler (fn [e] (ss/alert "Not implemented!")) 
                        :name "Preferences" 
                        :tip "Show options" 
                        :key "menu O")
      a-details (ss/action :handler (fn [e] (ss/alert "Not implemented!")) 
                           :name "Details" 
                           :tip "Show info abou your PC."
                           :key "menu I")
      a-bout    (ss/action :handler (fn [e] (ss/alert "Not implemented!")) 
                           :name "About" 
                           :tip "A few words." 
                           :key "menu A")]   
(ss/menubar :items 
   [(ss/menu :text "Game"    :items [(ss/menu :text "New" :items [n-chess n-checkers]) a-save a-load])
    (ss/menu :text "Options" :items [a-pref])
    (ss/menu :text "Help"    :items [a-details a-bout])]  )))

(defn draw-grid [c g]
  (let [w (ss/width c)
        h (ss/height c)]
    (doseq [x (range 0 w 50)]
      (.drawLine g x 0 x h))
    (doseq [y (range 0 h 50)]
        (.drawLine g 0 y w y))))
        
(defn draw-tiles [d g]
  (let [w (ss/width d)
        h (ss/height d)
        tiles (map vector (for [x (range 0 w 50) 
                                y (range 0 h 50)] [x y]) 
                          (cycle [(ut/make-color 'WHITE) 
                                  (ut/make-color 'BLACK)]))]  
    (doseq [[[x y] c] tiles]
       (.setColor g c)
       (.fillRect g x y 50 50)) ))
 
(defn make-canvas []
 (ss/canvas
    :paint draw-tiles
    :id :canvas
    ;:background "#222222"; no need for background
    ))
    
(defn make-arena 
"Constructs and returns the entire arena frame" []
 (ss/frame
    :title "Clondie24 Arena"
    :size  [410 :by 452] 
    :resizable? false
    :on-close :exit
    :menubar  (make-menubar)
    :content  (ss/border-panel
               :border 5
               :hgap 5
               :vgap 5
               ;:north  (make-toolbar)
               :center (make-canvas)
               :south  (ss/label :id :status :text "Ready!"))))              
               

(defn -main [& args]
#_(native!)
  (doto (make-arena) ss/show!)) 
        
