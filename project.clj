(defproject clj-json "0.1.0-SNAPSHOT"
  :description "A simple json library for clojure and clojurescript."
  :url "http://github.com/vhqr0/clj-json"
  :license {:name "GPL-3.0"
            :url "https://www.gnu.org/licenses/gpl-3.0.txt"}
  :dependencies [[org.clojure/clojure "1.12.0-alpha11"]]
  :profiles
  {:cljs {:dependencies [[thheller/shadow-cljs "2.28.5"]]}
   :perf {:dependencies [[com.clojure-goes-fast/clj-async-profiler "0.5.0"]
                         [criterium/criterium "0.4.6"]
                         [org.clojure/data.json "2.5.0"]]
          :source-paths ["resources"]
          :jvm-opts ["-Djdk.attach.allowAttachSelf=true"]
          :repl-options {:init-ns json.perf}}})
