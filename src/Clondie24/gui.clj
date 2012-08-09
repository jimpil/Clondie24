(ns Clondie24.gui
    (:require [Clondie24.util :as ut])
    (:use seesaw.core))
;-------------------------------------<SOURCE-CODE>--------------------------------------------------------------------
;----------------------------------------------------------------------------------------------------------------------    
           
(defn make-menubar 
"Constructs and returns the entire menu-bar." []
(let [n-chess    (action :handler (fn [e] (alert "Not implemented!")) :name "Chess" :tip "Start a new chess-game." :key "menu C")
      n-checkers (action :handler (fn [e] (alert "Not implemented!")) :name "Checkers" :tip "Start a new chess-game." :key "menu D")
      a-save     (action :handler (fn [e] (alert "Not implemented!")) :name "Save as" :tip "Save a game to disk." :key "menu S")
      a-load     (action :handler (fn [e] (alert "Not implemented!")) :name "Load" :tip "Load a game from disk." :key "menu L")
      a-pref     (action :handler (fn [e] (alert "Not implemented!")) :name "Preferences" :tip "Show options" :key "menu O")
      a-details  (action :handler (fn [e] (alert "Not implemented!")) :name "Details" :tip "Show info abou your PC." :key "menu I")
      a-bout     (action :handler (fn [e] (alert "Not implemented!")) :name "About" :tip "A few words." :key "menu A")]   
(menubar :items 
   [(menu :text "Game"    :items [(menu :text "New" :items [n-chess n-checkers]) a-save a-load])
    (menu :text "Options" :items [a-pref])
    (menu :text "Help"    :items [a-details a-bout])]  )))

(defn draw-grid [c g]
  (let [w (width c)
        h (height c)]
    (doseq [x (range 0 w 50)]
      (.drawLine g x 0 x h))
    (doseq [y (range 0 h 50)]
        (.drawLine g 0 y w y))))
        
(defn draw-tiles [d g]
  (let [w (width d)
        h (height d)
        tiles (map vector (for [x (range 0 w 50) 
                                y (range 0 h 50)] [x y]) 
                          (cycle [(ut/make-color 'WHITE) 
                                  (ut/make-color 'BLACK)]))]  
    (doseq [[[x y] c] tiles]
       (.setColor g c)
       (.fillRect g x y 50 50)) ))
 
(defn make-canvas []
 (canvas
    :paint draw-tiles
    :id :canvas
    ;:background "#222222"; no need for background
    ))
    
(defn make-arena 
"Constructs and returns the entire arena frame" []
 (frame
    :title "Clondie24 Arena"
    :size  [410 :by 452] 
    :resizable? false
    :on-close :exit
    :menubar  (make-menubar)
    :content  (border-panel
               :border 5
               :hgap 5
               :vgap 5
               ;:north  (make-toolbar)
               :center (make-canvas)
               :south  (label :id :status :text "Ready!"))))              
               

(defn -main [& args]
#_(native!)
 
  (doto (make-arena) show!)) 
        
