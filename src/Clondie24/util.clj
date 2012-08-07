(ns Clondie24.util
    (:use [clojure.pprint :only (pprint, print-table)]))
            
;----------------------------------------<SOURCE>--------------------------------------------------------------------
;----------------------------------------<CODE>----------------------------------------------------------------------
(defn record-factory [recordname]
  (let [recordclass ^Class (resolve (symbol recordname))
        max-arg-count (apply max (map #(count (.getParameterTypes %))
                                       (.getConstructors recordclass)))
        args (map #(symbol (str "x" %)) (range (- max-arg-count 2)))]
    (eval `(fn [~@args] (new ~(symbol recordname) ~@args)))))
    
(defn record-factory-aux 
"Same as record-factory but using the auxiliary constructor of records which accepts a meta-data map and 
 a field extension map as extra args. Useful." 
 [recordname]
  (let [recordclass ^Class (resolve (symbol recordname))
        max-arg-count (apply max (map #(count (.getParameterTypes %))
                                       (.getConstructors recordclass)))
        args (map #(symbol (str "x" %)) (range max-arg-count))]
    (eval `(fn [~@args] (new ~(symbol recordname) ~@args)))))

(defn double? [e]
(if (= (class e) (Class/forName  "java.lang.Double")) true false))
    
(defn vector-of-doubles 
^clojure.lang.PersistentVector [v]
(if (every? double? v) v
    (vec (map double v))))       
    
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
(catch java.io.IOException e ;returning nil here is ok for now! 
  (println path-to-image "does not exist! Reverting to 'nil'..."))))

;Helper fn for creting pre-defined Colours
(defn make-color 
"Helper fn for creting java.awt.Color given a color symbol (e.g 'RED)"
^java.awt.Color [^String predefined-name]
(.get (.getField (Class/forName "java.awt.Color") 
      (str predefined-name)) nil)) 
      
(defn piece->point [p] ;needs to be in the gui namespace
 (let [[x y] (.getGridPosition  p)]
 (java.awt.Point. x y))) 
 
(defn no-nils "Filter out nils from a collection." 
 [c] 
 (filter (complement nil?) c))  
 
(defn print-board 
"Will print the detailed board with nils where vacant." 
[game] 
(print-table (:characteristics (game));the columns
      (deref (:board-atom (game)))))  ;the rows
       

         
(defn old-table-model 
"Modified table-model that does not give up when first element is nil." 
 [data]
  (let [row1 (some #(when-not (nil? %) %) data) ;only this line is different from clojure.inspector/old-table-model  
    colcnt (count row1)
    cnt (count data)
    vals (if (map? row1) vals identity)]
    (proxy [javax.swing.table.TableModel] []
      (addTableModelListener [tableModelListener])
      (getColumnClass [columnIndex] Object)
      (getColumnCount [] colcnt)
      (getColumnName [columnIndex]
    (if (map? row1)
      (name (nth (keys row1) columnIndex))
      (str columnIndex)))
      (getRowCount [] cnt)
      (getValueAt [rowIndex columnIndex]
    (nth (vals (nth data rowIndex)) columnIndex))
      (isCellEditable [rowIndex columnIndex] false)
      (removeTableModelListener [tableModelListener]))))          
         
(defn inspect-table 
  "creates a graphical inspector on the supplied regular
  data, which must be a sequential data structure of data structures
  of equal length."
  [data]
  (doto (javax.swing.JFrame. "Clondie24 Board")
    (.add (javax.swing.JScrollPane. 
          (javax.swing.JTable. (old-table-model data))))
    (.setSize 500 600)
    (.setVisible true)))
    
(defn inspect-boards [bs] ;the boards
(map #(inspect-table %) bs))          
    
