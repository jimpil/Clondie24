(defproject Clondie24 "0.1.0-SNAPSHOT"
  :description "Blondie24 Extreme-Makeover! "
  :url "https://github.com/jimpil/Clondie24"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.5.0-master-SNAPSHOT"]
                 ;[org.clojure/clojure "1.5.0-alpha3"]
                 [org.clojure/core.logic "0.7.5"]
                ; [midje "1.4.0"] 
                 [seesaw "1.4.2" :exclusions [org.clojure/clojure]]
                 ;[clojure-encog "0.4.1-SNAPSHOT"] ;not needed yet
                 ]
  :dev-dependencies [[midje "1.4.0"]]               
  ;:plugins [[lein-swank "1.4.4"]]             
  :jvm-opts ["-Xmx2g" "-server"] 
  :warn-on-reflection true
 ;:javac-options {:classpath "target/dependency/encog-core-3.1.0.jar" :destdir "target/classes"}
 ;:java-source-path "src/java"
 :main     Clondie24.games.chess             
 )
