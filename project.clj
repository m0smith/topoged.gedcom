(defproject topoged/gedcom "0.1.2-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]]
  :source-paths ["target/classes"]
  :resource-paths ["target/classes"]
  :hooks [cljx.hooks]

  :plugins [[com.keminglabs/cljx "0.4.0" :exclusions [org.clojure/clojure]]]

  :cljx {:builds [{:source-paths ["src/cljx"]
                   :output-path "target/classes"
                   :rules :clj}
                  
                  {:source-paths ["src/cljx"]
                   :output-path "target/classes"
                   :rules :cljs}]}
  :profiles { :dev 
             {
             :dependencies [[org.clojure/core.async "0.1.346.0-17112a-alpha"]
                              [marginalia "0.8.0"] 
                              [org.clojure/test.check "0.5.9"] ;; property testing
                              ]}}
)
