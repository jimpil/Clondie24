(ns Clondie24.rules-test
  (:use midje.sweet
        Clondie24.rules))
        
(fact (-> (queen-moves (vec (repeat 64 nil)) 0 0 1) ;by testing the queen we're also testing the rook and the bishop
          (.containsAll (list [1 1] [2 2] [3 3] [4 4] [5 5] [6 6] [7 7]   ;diagonally (like a bishop)
                              [1 0] [2 0] [3 0] [4 0] [5 0] [6 0] [7 0]   ;right (like a rook)
                              [0 1] [0 2] [0 3] [0 4] [0 5] [0 6] [0 7])));down (like a rook)
                                                                            => true) 
                                                                            
(fact (-> (king-moves (vec (repeat 64 nil)) 0 0 1) 
          (.containsAll (list [1 1] [1 0] [0 1]))) => true) 
                                     

