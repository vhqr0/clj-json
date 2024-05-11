(ns json.core-test
  (:require #?(:clj [clojure.test :refer :all]
               :cljs [cljs.test :refer [deftest is]])
            [clojure.string :as str]
            [json.core :as json]))

(deftest read-numbers
  (is (= 42 (json/read-string "42")))
  (is (= -3 (json/read-string "-3")))
  (is (= 3.14159 (json/read-string "3.14159"))))

(deftest read-null
  (is (= nil (json/read-string "null"))))

(deftest read-strings
  (is (= "Hello, World!" (json/read-string "\"Hello, World!\""))))

(deftest escaped-slashes-in-strings
  (is (= "/foo/bar" (json/read-string "\"\\/foo\\/bar\""))))

(deftest unicode-escapes
  (is (= " \u0beb " (json/read-string "\" \\u0bEb \""))))

(deftest escaped-whitespace
  (is (= "foo\nbar" (json/read-string "\"foo\\nbar\"")))
  (is (= "foo\rbar" (json/read-string "\"foo\\rbar\"")))
  (is (= "foo\tbar" (json/read-string "\"foo\\tbar\""))))

(deftest read-booleans
  (is (= true (json/read-string "true")))
  (is (= false (json/read-string "false"))))

(deftest ignore-whitespace
  (is (= nil (json/read-string "\r\n   null"))))

(deftest read-arrays
  (is (= (vec (range 35))
         (json/read-string "[0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34]")))
  (is (= ["Ole" "Lena"] (json/read-string "[\"Ole\", \r\n \"Lena\"]"))))

(deftest read-objects
  (is (= {:k1 1, :k2 2, :k3 3, :k4 4, :k5 5, :k6 6, :k7 7, :k8 8
          :k9 9, :k10 10, :k11 11, :k12 12, :k13 13, :k14 14, :k15 15, :k16 16}
         (binding [json/*read-keyfn* keyword]
           (json/read-string "{\"k1\": 1, \"k2\": 2, \"k3\": 3, \"k4\": 4,
                          \"k5\": 5, \"k6\": 6, \"k7\": 7, \"k8\": 8,
                          \"k9\": 9, \"k10\": 10, \"k11\": 11, \"k12\": 12,
                          \"k13\": 13, \"k14\": 14, \"k15\": 15, \"k16\": 16}")))))

(deftest read-nested-structures
  (is (= {:a [1 2 {:b [3 "four"]} 5.5]}
         (binding [json/*read-keyfn* keyword]
           (json/read-string "{\"a\":[1,2,{\"b\":[3,\"four\"]},5.5]}")))))

(deftest reads-long-string-correctly
  (let [long-string (str/join "" (take 100 (cycle "abcde")))]))

(deftest get-string-keys
  (is (= {"a" [1 2 {"b" [3 "four"]} 5.5]}
         (json/read-string "{\"a\":[1,2,{\"b\":[3,\"four\"]},5.5]}"))))

(deftest keywordize-keys
  (is (= {:a [1 2 {:b [3 "four"]} 5.5]}
         (binding [json/*read-keyfn* keyword]
           (json/read-string "{\"a\":[1,2,{\"b\":[3,\"four\"]},5.5]}")))))

(deftest print-json-strings
  (is (= "\"Hello, World!\"" (json/write-string "Hello, World!")))
  (is (= "\"\\\"Embedded\\\" Quotes\"" (json/write-string "\"Embedded\" Quotes"))))

(deftest print-json-null
  (is (= "null" (json/write-string nil))))

(deftest print-json-arrays
  (is (= "[1,2,3]" (json/write-string [1 2 3])))
  (is (= "[1,2,3]" (json/write-string (list 1 2 3))))
  (is (= "[1,2,3]" (json/write-string (sorted-set 1 2 3)))))

(deftest print-empty-arrays
  (is (= "[]" (json/write-string [])))
  (is (= "[]" (json/write-string (list))))
  (is (= "[]" (json/write-string #{}))))

(deftest print-json-objects
  (is (= "{\"a\":1,\"b\":2}" (json/write-string (sorted-map :a 1 :b 2)))))

(deftest object-keys-must-be-strings
  (is (= "{\"1\":1,\"2\":2}" (json/write-string (sorted-map 1 1 2 2)))))

(deftest print-empty-objects
  (is (= "{}" (json/write-string {}))))

(deftest accept-sequence-of-nils
  (is (= "[null,null,null]" (json/write-string [nil nil nil]))))

(deftest characters-in-map-keys-are-escaped
  (is (= "{\"\\\"\":42}" (json/write-string {"\"" 42}))))
