(ns Clondie24.util
      (:use [clojure.pprint :only (pprint, print-table)]
            [clojure.inspector :only (inspect-table)]))
            
;----------------------------------------<SOURCE>--------------------------------------------------------------------
;----------------------------------------<CODE>----------------------------------------------------------------------
(defn record-factory [recordname]
  (let [recordclass ^Class (resolve (symbol recordname))
        max-arg-count (apply max (map #(count (.getParameterTypes %))
                                      (.getConstructors recordclass)))
        args (map #(symbol (str "x" %)) (range (- max-arg-count 2)))]
    (eval `(fn [~@args] (new ~(symbol recordname) ~@args)))))
    
(defn vector-of-doubles 
^clojure.lang.PersistentVector [v] 
(vec (map double v)))       
    
(defmacro doeach 
"Like doseq but in a map-like manner. Assumes f is side-effecty." 
 [f coll]
`(doseq [x# ~coll] (~f x#)))

;Helper fn for creting Points
(defn make-point 
"Helper fn for creting java.awt.Point" 
^java.awt.Point [p]
(let [[x y] p]
(java.awt.Point. x y)))


(defn make-image 
"Returns a buffered-image from the specified file or nil if the file is not there.." 
[path-to-image]
(try 
  (javax.imageio.ImageIO/read (java.io.File. path-to-image))
(catch java.io.IOException e ;returning nil here is ok! 
  (println path-to-image "does not exist! Reverting to 'nil'..."))))

;Helper macro for creting pre-defined Colours
(defn make-color 
"Helper fn for creting java.awt.Color given a color symbol (e.g 'RED)"
^java.awt.Color [^String predefined-name]
(.get (.getField (Class/forName "java.awt.Color") 
      (str predefined-name)) nil)) 
      
(defn piece->point [p] ;needs to be in the gui namespace
 (let [[x y] (getGridPosition p)]
 (java.awt.Point. x y))) 
 
(defmacro no-nils "Filter out nils from a collection." 
  [c] 
 `(filter (complement nil?) ~c)) 
 
(defmacro bury-dead "Will filter out dead-pieces from a collection"
  [c]
 `(filter (complement dead-piece?) ~c)) 
 
(defn clean 
 ^clojure.lang.PersistentVector [c]
 (filter #(and (not (nil? %)) 
               (not (dead-piece? %)))  c))  
 
(defmacro print-board 
"Will print the detailed board with nils where vacant. Calls build-board without 'cleaning' it." 
[game] 
`(print-table (get (~game) :characteristics);the columns
       (deref (get (~game) :board-atom))))  ;the rows
       
(defmacro inspect-board [game] ;fails if the first element is nil
`(inspect-table ;(get (~game) :characteristics)  
         (deref (get (~game) :board-atom))))  
         
(defn vacant? 
 "Checks if a position [x, y] is vacant on the given board and mappings." 
 ^Boolean [m b pos]
 (let [[x y] pos]
 (nil? 
  (nth b (translate-position x y m)))))          
    