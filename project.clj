(defproject Clondie24 "0.5.2"
  :description "Blondie24 Extreme-Makeover! "
  :url "https://github.com/jimpil/Clondie24"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 ;[org.clojure/core.logic "0.8.4" ] 
                 [org.clojure/tools.nrepl "0.2.3"] 
                ;[org.clojure/core.memoize "0.5.6"]
                 [org.clojure/math.combinatorics "0.0.7"]
                 [seesaw "1.4.3" :exclusions[[org.clojure/clojure]]]
                 [enclog "0.6.2"]
                 [swingrepl "1.3.0" :exclusions [org.clojure/clojure]]  
                 ]
  ;:dev-dependencies [[[midje "1.5-alpha2"] 
  ;                    [org.clojure/tools.trace "0.7.3"]]]               
  ;:plugins [[lein-swank "1.4.4"]]             
  :jvm-opts ^:replace
            ["-Xmx2g" "-server" 
             "-XX:+UseConcMarkSweepGC"
             "-XX:+DoEscapeAnalysis"] 
 ;:javac-options {:classpath "target/dependency/encog-core-3.1.0.jar" :destdir "target/classes"}
 :java-source-paths ["src/encog_java"]
 ;:main     Clondie24.games.tictactoe             
 ;:aot :all
 )
