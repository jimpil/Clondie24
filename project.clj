(defproject Clondie24 "0.5.0-SNAPSHOT"
  :description "Blondie24 Extreme-Makeover! "
  :url "https://github.com/jimpil/Clondie24"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.5.0"]
                 [org.clojure/tools.trace "0.7.3"]
                 [org.clojure/core.logic "0.8.0-rc2" ] ; 
                 ;[midje "1.5-alpha2"]
                 [org.clojure/core.match "0.2.0-alpha12"]
                 [ghostandthemachine/seesaw "1.4.3-SNAPSHOT" :exclusions [org.clojure/clojure]] 
                 [enclog "0.5.8"] 
                 ]
  :dev-dependencies [[[midje "1.5-alpha2"]]]               
  ;:plugins [[lein-swank "1.4.4"]]             
  :jvm-opts ["-Xmx2g" "-server" "-XX:-UseCompressedOops"] 
 ;:javac-options {:classpath "target/dependency/encog-core-3.1.0.jar" :destdir "target/classes"}
 ;:java-source-path "src/java"
 :main     Clondie24.games.chess             
 )
