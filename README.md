Clondie24
=========

Blondie24 goes Functional!

This is an attempt to re-create Blondie24 [1], the computer program that learnt to play checkers on its own (by evolutionary means). At the moment I 've got the basic structures for the game (the board, the pieces and moving the pieces) and next comes creating the rules for the game and after that I will need some of sort of minimax/alpha-beta algorithm for doing the recursion. Now that clojure-encog [2] is around I can easily hook up the neural net and the genetic algorithm at the end... 


This is work in progress... I wouldn't even call it version 0.0.1!


#Update [17/6/2012]
--------------------

- It's turning out to be more like a framework for writing board games rather than a recreation of Blondie24.
- The core engine is ready. The 'world' is fully immutable. 
- No need to keep a stack of moves since I'm logging every new state of the board.
- Chess and Checkers namespaces are ready (records that satisfy protocols from core).
- How each piece should move will be implented in core.logic in rules.clj (knight-moves is ready).
- As soon as I've got the rules of each piece in core.logic I can start putting everything together...




[1] http://en.wikipedia.org/wiki/Blondie24 
[2] https://github.com/jimpil/clojure-encog
