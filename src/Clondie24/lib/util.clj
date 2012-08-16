(ns Clondie24.lib.util
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
    
(defn vector-of-doubles [v]
(if (every? double? v) v
    (vec (map double v))))       
    
(defmacro doeach 
"Like doseq but in a map-like manner. Assumes f is side-effecty." 
 [f coll]
`(doseq [x# ~coll] (~f x#)))

;Helper fn for creting Points
(defn make-point 
"Helper fn for creting java.awt.Point out of a [x y] coords." 
^java.awt.Point [p]
(let [[x y] (vector-of-doubles p)]
(java.awt.Point. x y)))


(defn make-image 
"Returns a buffered-image from the specified file or nil if the file is not there." 
[path-to-image]
(try 
  (javax.imageio.ImageIO/read (java.io.File. path-to-image))
(catch java.io.IOException e ;returning nil here is ok for now! 
  (println path-to-image "does not exist! Reverting to 'nil'..."))))

;Helper fn for creting pre-defined Colours
(defn predefined-color 
"Helper fn for creting java.awt.Color given a color symbol (e.g 'RED or 'red)."
^java.awt.Color [name]
(.get (.getField 
        (Class/forName "java.awt.Color") (str name)) nil))

(defn hex->color 
"Hepler fn for creating Color objects from a hex literal (e.g. '0xFF0096)."
 [hex-symbol] 
 (try (java.awt.Color/decode (str hex-symbol)) 
 (catch Exception e ; returning nil 
        (println "Incorrect hex mapping."))))
         
(defn hsb->color 
"Hepler fn for creating Color objects from hue/saturation/brightness (e.g. 0.56, 1, 0.8)." 
[h s b]
(java.awt.Color/getHSBColor (float h) (float s) (float b)))

(defn rgb->color [r g b] 
 (java.awt.Color. (double r) (double g) (double b)))             
      
(defn piece->point [p] ;needs to be in the gui namespace
 (let [[x y] (.getGridPosition  p)]
 (java.awt.Point. x y))) 
 
(defn no-nils 
"Filter out nils from a collection." 
 [c] 
 (remove nil? c))  
 
(defn print-board 
"Will print the detailed board with nils where vacant." 
[game] 
(print-table (:characteristics game);the columns
      (deref (:board-atom game))))  ;the rows

(defn persist-board! 
"Persists the board b on to the disk using Java serialization. Filename needs no extension - it will be appended (.ser)."
[b fname]
(with-open [oout (java.io.ObjectOutputStream. 
                 (java.io.FileOutputStream. (str fname ".ser")))]
                 (.writeObject oout b)))
                
(defn unpersist-board 
"Un-Persists a vector from the disk using Java serialization. Filename needs no extension - it will be appended (.ser)." 
^clojure.lang.PersistentVector [fname]
(let [^clojure.lang.PersistentVector upb (promise)] ;waiting for the value shortly
  (with-open [oin (java.io.ObjectInputStream. 
                  (java.io.FileInputStream. (str fname ".ser")))] 
                  (deliver upb (.readObject oin)))
       @upb))                      

         
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


(defn make-walker [dir]
 (fn [[sx sy]] 
   (condp = dir
          :north [sx (dec sy)]
          :south [sx (inc sy)]
          :east  [(dec sx) sy]
          :west  [(inc sx) sy]
          :north-east [(inc sx) (dec sy)]
          :north-west [(dec sx) (dec sy)]
          :south-east [(inc sx) (inc sy)]
          :south-west [(dec sx) (inc sy)])))
          
(defn resolve-direction 
[[sx sy] [ex ey]]
(let [dx (- ex sx)
      dy (- ey sy)]
(cond 
   (and (pos? dx)  (zero? dy)) :west
   (and (neg? dx)  (zero? dy)) :east
   (and (pos? dx)  (pos?  dy)) :south-east
   (and (pos? dx)  (neg?  dy)) :north-east
   (and (zero? dx) (pos? dy))  :south
   (and (zero? dx) (neg? dy))  :north
   (and (neg? dx)  (neg? dy))  :north-west
   (and (neg? dx)  (pos? dy))  :south-west)))  
   
         
    