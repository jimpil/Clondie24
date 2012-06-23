(defproject Clondie24 "0.1.0-SNAPSHOT"
  :description "Blondie24 Extreme-Makeover! "
  :url "https://github.com/jimpil/Clondie24"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.4.0"]
                 [org.clojure/core.logic "0.7.4"]
                 [midje "1.4.0"]
                 ;[clojure-encog "0.4.0-SNAPSHOT"]
                 ]
  :plugins [[lein-swank "1.4.4"]]
  ;:dev-dependencies [[lein-midje "2.0.0-SNAPSHOT"]]              
  :jvm-opts ["-Xmx512m" "-server"] 
 ;:javac-options {:classpath "target/dependency/encog-core-3.1.0.jar" :destdir "target/classes"}
 ;:java-source-path "src/java"
 ;:main     Clondie24.core             
 )
