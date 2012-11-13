(ns Clondie24.games.dummy
          (:require [Clondie24.lib.gui :as gui]))
          
(def details {:name 'Dummy  ;; need at least a game name 
              :players 2    ;;and players
              :arena-size [420 :by 505] ;;seesaw-ready size
              :tile-size 133})          
          
(defn -main 
"Starts a graphical (swing) Chess game." 
[& args]  
(gui/show-gui! details))           
