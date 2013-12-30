(ns Clondie24.lib.util
    (:require [clojure.pprint :refer [pprint, print-table]]
              ;[clojure.math.combinatorics :as combi]
              [clojure.edn :as edn]))
            
;----------------------------------------<SOURCE>--------------------------------------------------------------------
;----------------------------------------<CODE>----------------------------------------------------------------------
(set! *unchecked-math* true)
(set! *warn-on-reflection* true)

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
    
(defn scaffold
"Given an interface (or a class), returns a hollow body to use with 'deftype'." 
 [iface]
  (doseq [[iface methods] (->> iface .getMethods 
                            (map #(vector (.getName (.getDeclaringClass %)) 
                                    (symbol (.getName %))
                                    (count (.getParameterTypes %))))
                            (group-by first))]
    (println (str "  " iface))
    (doseq [[_ name argcount] methods]
      (println 
        (str "    " 
          (list name (into ['this] (repeatedly argcount gensym))))))))     
    
      
(defmacro with-captured-inputs 
 [f & args]
`(try (~f ~@args)
  (catch Exception e# (do (println "ERROR!") 
                           {:exception e# 
                            :function ~f 
                            :inputs (vector ~@args)})))) 
                            
(defmacro defn-capt [name [& args] & code]
`(defn ~name [~@args] 
   (try ~@code
   (catch Exception e# (do (println "ERROR!")
                          {:exception e#
                           :origin ~name
                           :inputs (vector ~@args)}))))) 
;(defn-capt foo [a b] (/ a b))
;(foo 1 0)                                

(defn double? [e]
(if (= (class e) (Class/forName "java.lang.Double")) true false))

(defn round-to-int [n] 
(Math/round (float n)))

(defn balance
([how] (balance how 50))
([how how-much] 
 (case how
      :up   #(* how-much %)
      :down (comp int #(/ % how-much)))))
    
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
^java.awt.Point [[x y]]
(java.awt.Point. x y))


(defn make-image 
"Returns a buffered-image from the specified file or nil if the file is not there." 
[^String path-to-image]
(try  (javax.imageio.ImageIO/read (java.io.File. path-to-image))
(catch java.io.IOException e ;returning nil here  
  (println path-to-image "does not exist! Reverting to nil..."))))

;Helper fn for creting pre-defined Colours
(defn predefined-color 
"Helper fn for creting java.awt.Color given a color symbol (e.g 'RED or 'red)."
^java.awt.Color [name]
(.get (.getField 
        (Class/forName "java.awt.Color") (str name)) nil))       

(defn hex->color 
"Hepler fn for creating Color objects from a hex literal (e.g. '0xFF0096)."
 ^java.awt.Color [hex-symbol] 
 (try (java.awt.Color/decode (str hex-symbol)) 
 (catch Exception e ; returning nil 
        (println "Incorrect hex mapping."))))
         
(defn hsb->color 
"Hepler fn for creating Color objects from hue/saturation/brightness (e.g. 0.56, 1, 0.8)." 
^java.awt.Color  [h s b]
(java.awt.Color/getHSBColor (float h) (float s) (float b)))

(defn rgb->color 
^java.awt.Color  [r g b] 
 (java.awt.Color. (double r) (double g) (double b)))             
      
(defn piece->point 
^java.awt.Point [p] ;needs to be in the gui namespace
 (let [[x y] (or (:position  p) 
                 (.getGridPosition p))]
 (java.awt.Point. x y))) 
 
 (defn Point->Vec [^java.awt.Point p]
 (vector (.x p) (.y p)))
 
(definline no-nils 
"Filter out nils from a collection." 
 [c] 
 `(remove nil? ~c))  
 
(defn print-board 
"Will print the detailed board with nils where vacant." 
[game] 
(print-table (:characteristics game);the columns
      (deref (:board-atom game))))  ;the rows

(defn serialize! 
"Serialize the object b on to the disk using standard Java serialization."
[b ^String fname]
(with-open [oout (java.io.ObjectOutputStream. 
                  (java.io.FileOutputStream. fname))]
  (.writeObject oout b)))
                
(defn deserialize! 
"Deserializes the object  in file f from the disk using standard Java serialization." 
 [^String fname]
(with-local-vars [upb nil]  ;;waiting for the value shortly
  (with-open [oin (java.io.ObjectInputStream. 
                   (java.io.FileInputStream. fname))] 
      (var-set upb (.readObject oin)))
       @upb))

       
(defn data->string
"Writes the object b on a file f on disk as a string."
[b f]
(io! (spit f b)
#_(with-open [w (clojure.java.io/writer f)]
  (binding [*out* w]  (prn b))))) 

(defn string->data
"Read the file f back on memory safely. Contents of f should be a clojure data-structure." 
([f game-details]
(io!
 (edn/read-string {:readers (get game-details :readers)} (slurp f))))
([f]  
  (string->data f {:readers {}}
  #_{'Clondie24.games.chess.ChessPiece #'Clondie24.games.chess/chesspiece-reader
   'Clondie24.games.tictactoe.TicTacToePiece #'Clondie24.gamestictactoe/ttt-piece-reader})))                             

         
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

(defn walk* 
([direction start distance]
(take (inc distance)
(iterate 
(fn [[sx sy]] 
(case  direction
          :north [sx (dec sy)]
          :south [sx (inc sy)]
          :east  [(inc sx) sy]
          :west  [(dec sx) sy]
          :north-east [(inc sx) (dec sy)]
          :north-west [(dec sx) (dec sy)]
          :south-east [(inc sx) (inc sy)]
          :south-west [(dec sx) (inc sy)])) start))) 
([direction start-pos]
  (walk* direction start-pos 1)))
  
(definline jump? [s e]
`(-> (Math/abs (- (first ~e) 
                  (first ~s))) 
      (rem  2)
      zero?))            

(def walk (memoize walk*))          

(defn make-walker [direction rank]
(when-not (= rank 'knight) ;;knight jumps - doesn't walk
  #(last (walk direction %))))

(defn within-limits? [[xlim ylim] [x y]]
 (and (>= x 0) (< x xlim)  
      (>= y 0) (< y ylim)))
                   
          
(definline resolve-direction 
[sp ep]
`(let [dx# (- (first ~ep) 
              (first ~sp))
       dy# (- (second ~ep) 
              (second ~sp))]
(cond 
   (and (neg? dx#)  (zero? dy#)) :west
   (and (pos? dx#)  (zero? dy#)) :east
   (and (pos? dx#)  (pos?  dy#)) :south-east
   (and (pos? dx#)  (neg?  dy#)) :north-east
   (and (zero? dx#) (pos? dy#))  :south
   (and (zero? dx#) (neg? dy#))  :north
   (and (neg? dx#)  (neg? dy#))  :north-west
   (and (neg? dx#)  (pos? dy#))  :south-west))) 
   
        
(defn external-ip "Returns the external ip address of the machine this code is running on, as a string." 
 ^String []
(let [ip-url (java.net.URL. "http://api.exip.org/?call=ip")]
(with-open [in (java.io.BufferedReader. 
               (java.io.InputStreamReader. (.openStream ip-url)))]
  (.readLine in))))
  
(defn grid 
 "Returns a grid represented as a sequence (vector) of co-ordinates (vectors))." 
 [width height]
 (vec 
   (for [y (range height) 
         x (range width)] 
    [x y])))   

(definline some-element 
"Like some but returns the element for which (pred x) returned a truthy value rather than the truthy value itself. 
 Useful for predicates that return true/false when actually we want to get the element and we don't really care about the boolean." 
[pred coll]
`(some #(when (~pred %) %) ~coll))


