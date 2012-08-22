(ns Clondie24.lib.reducing 
      (:require [clojure.core.reducers :as r]))
      
;;CREDIT GOES TO THE 'THEBUSBY' (http://www.thebusby.com/2012/07/tips-tricks-with-clojure-reducers.html)      
      

(defn fold-into-vec [coll]                                                                         
"Provided a reducer, concatenate into a vector. Same as (into [] coll), but parallel."
(r/fold (r/monoid into vector) conj coll)) 

(defn rindex-by-fn
  "Uses core.reducers/fold to return a hash-map indexed by the value of key-fn applied to each element in coll. 
   Note: key-fn can return a collection of multiple keys."
  ([key-fn coll] (rindex-by-fn key-fn identity coll))
  ([key-fn value-fn coll] (doall (r/fold         ;; combine + reduce strategy                                              
                                  (r/monoid (partial merge-with into) hash-map) ;;combine-fn  
                                  (fn [ndx elem]  ;; reduce-fn
                                    (let [key (key-fn elem)]       
                                      (if (coll? key)
                                        (reduce (fn [sndx selem] (assoc sndx selem (conj (get sndx selem []) (value-fn elem))))
                                                ndx key)
                                       (assoc ndx key (conj (get ndx key []) (value-fn elem))))))   coll))))
	                  
(defn vindex-by-fn
  "Uses core.reducers/fold to return a vector indexed by the value of key-fn applied to each element in coll. 
   Note: key-fn can return a collection of multiple keys."
  ([key-fn coll] (vindex-by-fn key-fn :move coll))
  ([key-fn value-fn coll] (doall (r/fold         ;; combine + reduce strategy                                              
                                  (r/monoid into vector) ;;combine-fn  
                                  (fn [ndx elem]  ;; reduce-fn
                                    (let [key (key-fn elem)]       
                                      (if (coll? key)
                                        (reduce (fn [sndx selem] (assoc sndx selem (value-fn elem)))
                                                ndx key)
                                       (assoc ndx key  (value-fn elem)))))  coll))))
