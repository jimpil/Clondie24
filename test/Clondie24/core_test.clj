(ns Clondie24.core-test
  (:use midje.sweet
        Clondie24.lib.core))

    
(def dummy-mappings [[0 0] [0 1] [1 0] [1 1]]) 
(def dummy-game {:name 'dummy
                :players 2 
                :colors nil
                :characteristics [:color :position :rank :value]  
                :board-size 4 
                :total-pieces 2
                :rel-values {:cleaner 1 :manager 3}
                :board-atom  nil
                :record-name "whatever"
                :mappings dummy-mappings
                :north-player-start  nil
                :south-player-start  nil})
   
(fact (translate-position 0 dummy-mappings) => [0 0])
(fact (translate-position 1 dummy-mappings) => [0 1])
(fact (translate-position 2 dummy-mappings) => [1 0])
(fact (translate-position 3 dummy-mappings) => [1 1])

(fact (translate-position 1 1 dummy-mappings) => 3)
(fact (translate-position 1 0 dummy-mappings) => 2)
(fact (translate-position 0 1 dummy-mappings) => 1)
(fact (translate-position 0 0 dummy-mappings) => 0)

(fact (try (translate-position 2 0 dummy-mappings) ;INVALID POSITION
      (catch Exception e :caught)) => :caught 
(provided 
      (translate-position 2 0 dummy-mappings) =throws=> (IllegalStateException.)))




(fact (alive? (with-meta [1 2 3] {:alive false})) => false)
(fact (alive? (with-meta [5 6 7] {:alive true})) => true)
(fact (alive? nil) => nil) ;NO NPE


(fact (empty-board dummy-game) => '(nil nil nil nil))


(fact (populate-board [(with-meta [1 2 3] {:alive false}) 
                       (with-meta [4 5 6] {:alive true})
                       (with-meta [7 8 9] {:alive false})
                       (with-meta [10 11 12] {:alive false})]) => [nil [4 5 6] nil nil])
                                              


