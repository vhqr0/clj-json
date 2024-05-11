(ns json.perf
  (:require [clojure.string :as str]
            [clj-async-profiler.core :as prof]
            [criterium.core :refer :all]
            [clojure.data.json :as cjson]
            [json.core :as json]))

(defmacro profiling [times & body]
  `(try
     (prof/start {})
     (dotimes [_# ~times]
       ~@body)
     (finally
       (prof/stop {:transform (fn [s#]
                                (if (or (str/index-of s# "err_codes_unix")
                                        (str/index-of s# "["))
                                  nil
                                  (-> s#
                                      (str/replace #"^.+/.*read-profiling.+?;" "START;")
                                      (str/replace #"^.*nextToken.*" "jackson-next-token")
                                      (str/replace #"^.*getText.*" "jackson-get-text")
                                      (str/replace #"^.*next.token.*" "data-json-next-token")
                                      (str/replace #"^.*read.quoted.string.*" "data-json-read_quoted_string")
                                      (str/replace #".*assoc.*" "ASSOC;")
                                      (str/replace #".*Map.*" "ASSOC;"))))}))))

(defn json-data [size]
  (slurp (str "resources/json" size ".json")))

;;; bench

(defn do-read-bench [size]
  (let [json (json-data size)]
    (println "Results for"  size "json:")
    (println "clj-json:")
    (println (with-out-str (quick-bench (json/read-string json))))
    (println "data.json:")
    (println (with-out-str (quick-bench (cjson/read-str json))))))

(defn do-write-bench [size]
  (let [edn (cjson/read-str (json-data size))]
    (println "Results for"  size "json:")
    (println "clj-json:")
    (println (with-out-str (quick-bench (json/write-string edn))))
    (println "data.json:")
    (println (with-out-str (quick-bench (cjson/write-str edn))))))

(defn read-bench-all-sizes []
  (doseq [size ["10b" "100b" "1k" "10k" "100k"]]
    (do-read-bench size)))

(defn write-bench-all-sizes []
  (doseq [size ["10b" "100b" "1k" "10k" "100k"]]
    (do-write-bench size)))

;;; profile

(defn do-read-profile [size]
  (let [json (json-data size)]
      (profiling 10000 (json/read-string json))))

(defn do-write-profile [size]
  (let [edn (cjson/read-str (json-data size))]
      (profiling 10000 (json/write-string edn))))

(defn profile-read-all-sizes []
  (doseq [size ["10b" "100b" "1k" "10k" "100k"]]
    (do-read-profile size)))

(defn profile-write-all-sizes []
  (doseq [size ["10b" "100b" "1k" "10k" "100k"]]
    (do-write-profile size)))

;;; nested arrays

(defn read-bench-nested-arrays []
  (let [json (slurp "resources/nested-arrays.json")]
    (println "Results for nested arrays:")
    (println "clj-json:")
    (println (with-out-str (quick-bench (json/read-string json))))
    (println "data.json:")
    (println (with-out-str (quick-bench (cjson/read-str json))))))
