(ns clj-json.core-test
  (:require #?(:clj [clojure.test :refer :all]
               :cljs [cljs.test :refer [deftest is]])
            [clojure.string :as str]
            [clj-json.core :as json]))

(deftest read-numbers
  (is (= 42 (json/read-string "42")))
  (is (= -3 (json/read-string "-3")))
  (is (= 3.14159 (json/read-string "3.14159")))
  (is (= 6.022e23M (json/read-string "6.022e23"))))

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

; from http://www.json.org/JSON_checker/test/pass1.json
(def pass1-string
  "[
    \"JSON Test Pattern pass1\",
    {\"object with 1 member\":[\"array with 1 element\"]},
    {},
    [],
    -42,
    true,
    false,
    null,
    {
        \"integer\": 1234567890,
        \"real\": -9876.543210,
        \"e\": 0.123456789e-12,
        \"E\": 1.234567890E+34,
        \"\":  23456789012E66,
        \"zero\": 0,
        \"one\": 1,
        \"space\": \" \",
        \"quote\": \"\\\"\",
        \"backslash\": \"\\\\\",
        \"controls\": \"\\b\\f\\n\\r\\t\",
        \"slash\": \"/ & \\/\",
        \"alpha\": \"abcdefghijklmnopqrstuvwyz\",
        \"ALPHA\": \"ABCDEFGHIJKLMNOPQRSTUVWYZ\",
        \"digit\": \"0123456789\",
        \"0123456789\": \"digit\",
        \"special\": \"`1~!@#$%^&*()_+-={':[,]}|;.</>?\",
        \"hex\": \"\\u0123\\u4567\\u89AB\\uCDEF\\uabcd\\uef4A\",
        \"true\": true,
        \"false\": false,
        \"null\": null,
        \"array\":[  ],
        \"object\":{  },
        \"address\": \"50 St. James Street\",
        \"url\": \"http://www.JSON.org/\",
        \"comment\": \"// /* <!-- --\",
        \"# -- --> */\": \" \",
        \" s p a c e d \" :[1,2 , 3

,

4 , 5        ,          6           ,7        ],\"compact\":[1,2,3,4,5,6,7],
        \"jsontext\": \"{\\\"object with 1 member\\\":[\\\"array with 1 element\\\"]}\",
        \"quotes\": \"&#34; \\u0022 %22 0x22 034 &#x22;\",
        \"\\/\\\\\\\"\\uCAFE\\uBABE\\uAB98\\uFCDE\\ubcda\\uef4A\\b\\f\\n\\r\\t`1~!@#$%^&*()_+-=[]{}|;:',./<>?\"
: \"A key can be any string\"
    },
    0.5 ,98.6
,
99.44
,

1066,
1e1,
0.1e1,
1e-1,
1e00,2e+00,2e-00
,\"rosebud\"]")

(deftest pass1-test
  (let [input (json/read-string pass1-string)]
    (is (= "JSON Test Pattern pass1" (first input)))
    (is (= "array with 1 element" (get-in input [1 "object with 1 member" 0])))
    (is (= 1234567890 (get-in input [8 "integer"])))
    (is (= "rosebud" (last input))))
  (is (binding [json/*read-bigdec* true]
        (= (json/read-string pass1-string)
           (json/read-string (json/write-string (json/read-string pass1-string)))))))
