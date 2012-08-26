(ns Clondie24.lib.util
    (:use [clojure.pprint :only (pprint, print-table)]))
            
;----------------------------------------<SOURCE>--------------------------------------------------------------------
;----------------------------------------<CODE>----------------------------------------------------------------------
(set! *unchecked-math* true)

(defn- with-thread-pool* [num-threads f]
  (let [pool (java.util.concurrent.Executors/newFixedThreadPool num-threads)]
    (try (f pool)
      (finally
        (when pool (.shutdown pool))))))

(defmacro with-thread-pool [[name num-threads] & body]
  `(with-thread-pool* ~num-threads (fn [~name] ~@body)))

(defn pmapp [f coll]
  (with-thread-pool [pool (.. Runtime getRuntime availableProcessors)]
    (doall
      (map #(.get %)
        (.invokeAll pool
          (map (partial partial f)  coll))))))

(defn record-factory [recordname]
  (let [recordclass ^Class (resolve (symbol recordname))
        max-arg-count (apply max (map #(count (.getParameterTypes ^java.lang.reflect.Constructor %))
                                       (.getConstructors recordclass)))
        args (map #(symbol (str "x" %)) (range (- max-arg-count 2)))]
    (eval `(fn [~@args] (new ~(symbol recordname) ~@args)))))
    
(defn record-factory-aux 
"Same as record-factory but using the auxiliary constructor of records which accepts a meta-data map and 
 a field extension map as extra args. Useful." 
 [recordname]
  (let [recordclass ^Class (resolve (symbol recordname)) 
        max-arg-count (apply max  (map #(count (.getParameterTypes ^java.lang.reflect.Constructor %))
                                       (.getConstructors recordclass)))
        args (map #(symbol (str "x" %)) (range max-arg-count))]
    (eval `(fn [~@args] (new ~(symbol recordname) ~@args)))))
    

(defn double? [e]
(if (= (class e) (Class/forName "java.lang.Double")) true false))

(defn round-to-int [n] 
(Math/round (float n)))
    
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
(let [[x y] p]
(java.awt.Point. x y)))


(defn make-image 
"Returns a buffered-image from the specified file or nil if the file is not there." 
[^String path-to-image]
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
 
 (defn Point->Vec [^java.awt.Point p]
 (vector (.x p) (.y p)))
 
 (defn Vec->Point [v]
 (java.awt.Point. (first v) (second v)))
 
(defn no-nils 
"Filter out nils from a collection." 
 [c] 
 (remove nil? c))  
 
(defn print-board 
"Will print the detailed board with nils where vacant." 
[game] 
(print-table (:characteristics game);the columns
      (deref (:board-atom game))))  ;the rows

(defn serialize-board! 
"Serialize the board b on to the disk using Java serialization. Filename needs no extension - it will be appended (.ser)."
[b fname]
(with-open [oout (java.io.ObjectOutputStream. 
                 (java.io.FileOutputStream. (str fname ".ser")))]
                 (.writeObject oout b)))
                
(defn deserialize-board 
"Deserializes a vector from the disk using Java serialization. Filename needs no extension - it will be appended (.ser)." 
^clojure.lang.PersistentVector [fname]
(let [^clojure.lang.PersistentVector upb (promise)] ;waiting for the value shortly
  (with-open [oin (java.io.ObjectInputStream. 
                  (java.io.FileInputStream. (str fname ".ser")))] 
                  (deliver upb (.readObject oin)))
       @upb))
       
(defn persist
"Write the data-structure b on a file f on disk as a string." 
[b f]
(spit f b))

(defn unpersist
"Read the file f back on memory. Contents should be a data-structure." 
[f]
(binding [*read-eval* false]
(read-string (slurp f))))                            

         
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
(map inspect-table bs))

(defn walk* [direction [sx sy]]
(condp = direction
          :north [sx (dec sy)]
          :south [sx (inc sy)]
          :east  [(inc sx) sy]
          :west  [(dec sx) sy]
          :north-east [(inc sx) (dec sy)]
          :north-west [(dec sx) (dec sy)]
          :south-east [(inc sx) (inc sy)]
          :south-west [(dec sx) (inc sy)]))

(def walk (memoize walk*))          

(defn make-walker [direction rank]
(if (= rank 'knight) nil ;;no walker for knight
 #(walk direction %)))
                   
          
(definline resolve-direction 
[sp ep]
`(let [dx# (- (first ~ep) (first ~sp))
       dy# (- (second ~ep) (second ~sp))]
(cond 
   (and (neg? dx#)  (zero? dy#)) :west
   (and (pos? dx#)  (zero? dy#)) :east
   (and (pos? dx#)  (pos?  dy#)) :south-east
   (and (pos? dx#)  (neg?  dy#)) :north-east
   (and (zero? dx#) (pos? dy#))  :south
   (and (zero? dx#) (neg? dy#))  :north
   (and (neg? dx#)  (neg? dy#))  :north-west
   (and (neg? dx#)  (pos? dy#))  :south-west)))  
   
        
    
