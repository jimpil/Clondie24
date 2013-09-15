(defproject Clondie24 "0.5.0-SNAPSHOT"
  :description "Blondie24 Extreme-Makeover! "
  :url "https://github.com/jimpil/Clondie24"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [org.clojure/core.logic "0.8.0-rc2" ] ; 
                 [org.clojure/core.match "0.2.0-rc5"]
                 [org.clojure/tools.nrepl "0.2.3"] 
                 [org.clojure/core.memoize "0.5.6"]
                ;[org.clojure/math.combinatorics "0.0.4"]
                 [seesaw "1.4.3"]
                 [enclog "0.6.2"]
                 [swingrepl "1.3.0" :exclusions [org.clojure/clojure]]  
                 #_[clj-tuple "0.1.0"] ;;Zach Tellman's peristent tuple can replace all the 2-element vectors without any fuss but performance is a tiny bit worse overall
                 ]
  :dev-dependencies [[[midje "1.5-alpha2"] 
                      [org.clojure/tools.trace "0.7.3"]]]               
  ;:plugins [[lein-swank "1.4.4"]]             
  :jvm-opts ["-Xmx2g" "-server" 
             "-XX:+UseConcMarkSweepGC"
             "-XX:+DoEscapeAnalysis"] 
 ;:javac-options {:classpath "target/dependency/encog-core-3.1.0.jar" :destdir "target/classes"}
 :java-source-paths ["src/encog_java"]
 ;:main     Clondie24.games.tictactoe             
 )
