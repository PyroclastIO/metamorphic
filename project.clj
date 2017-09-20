(defproject io.pyroclast/metamorphic "0.1.1-SNAPSHOT"
  :description "A complex event processing library for Clojure and ClojureScript."
  :url "https://github.com/pyroclastio/metamorphic"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.9.0-alpha17"]
                 [rhizome "0.2.7"]]
  :profiles {:dev {:plugins [[lein-codox "0.10.3"]]}}
  :codox {:output-path "doc"})
