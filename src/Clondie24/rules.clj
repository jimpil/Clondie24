(ns Clondie24.rules)

(defn ^Boolean diagonally? 
"Returns true if moving diagonally, false otherwise. Expects 2 vectors denoting start/end poisitions." 
[from to] )
(defn ^Boolean forward? 
"Returns true if moving forwards, false otherwise. Expects 2 vectors denoting start/end poisitions." 
[from to])
(defn ^Boolean backward? 
"Returns true if moving backwards, false otherwise. Expects 2 vectors denoting start/end positions."
[from to])
(defn ^Boolean left? 
"Returns true if moving left, false otherwise. Expects 2 vectors denoting start/end positions."
[from to])
(defn ^Boolean right? 
"Returns true if moving right , false otherwise. Expects 2 vectors denoting start/end positions." 
[from to])

(defn ^Boolean legal-move? 
"Returns true if the move is legal according to the rules , false otherwise. Expects a direction (1/-1) and 2 vectors denoting start/end positions ." 
[board direction from to])

