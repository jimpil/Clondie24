(defproject Clondie24 "0.3.0-SNAPSHOT"
  :description "Blondie24 Extreme-Makeover! "
  :url "https://github.com/jimpil/Clondie24"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.5.0-alpha4"]
                 ;[org.clojure/clojure "1.5.0-alpha3"]
                 [org.clojure/core.logic "0.8-alpha3" ] ; 
                ; [midje "1.4.0"]
                 [org.clojure/core.match "0.2.0-alpha11"]
                 [ghostandthemachine/seesaw "1.4.3-SNAPSHOT" :exclusions [org.clojure/clojure]] 
                 ;[seesaw "1.4.2" :exclusions [org.clojure/clojure]]
                 [enclog "0.5.8-SNAPSHOT"] 
                 ]
  :dev-dependencies [[midje "1.4.0"]]               
  ;:plugins [[lein-swank "1.4.4"]]             
  :jvm-opts ["-Xmx2g" "-server" "-XX:+UseCompressedOops"] 
  :warn-on-reflection true
 ;:javac-options {:classpath "target/dependency/encog-core-3.1.0.jar" :destdir "target/classes"}
 ;:java-source-path "src/java"
 ;:main     Clondie24.games.chess             
 )
