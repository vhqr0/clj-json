(ns json.core
  (:refer-clojure :exclude [read read-string])
  #?@(:cljs [(:import [goog.string StringBuffer])]))

;; sb: :clj Appendable (StringBuilder, Writer, etc) :cljs StringBuffer

(defn- sb-make []
  #?(:clj (StringBuilder.)
     :cljs (StringBuffer.)))

(defn- sb-str [sb]
  #?(:clj (.toString ^StringBuilder sb)
     :cljs (.toString sb)))

(defn- sb-conj-char [sb c]
  #?(:clj (doto ^Appendable sb (.append ^char c))
     :cljs (doto sb (.append c))))

(defn- sb-conj-str [sb s]
  #?(:clj (doto ^Appendable sb (.append ^String s))
     :cljs (doto sb (.append s))))

(defn- str->int [s]
  #?(:clj (Long/parseLong s)
     :cljs (js/parseInt s)))

(defn- str->float [s]
  #?(:clj (Double/parseDouble s)
     :cljs (js/parseFloat s)))

(defn- hex->int [h]
  #?(:clj (Long/parseLong h 16)
     :cljs (js/parseInt h 16)))

(def ^:private ws-chars #{\tab \newline \return \space})
(def ^:private num-chars #{\0 \1 \2 \3 \4 \5 \6 \7 \8 \9})

(defn- ws-char? [c]
  (and c
       (case (char c)
         (\tab \newline \return \space) true
         false)))

(defn- num-char? [c]
  (and c
       (case (char c)
         (\0 \1 \2 \3 \4 \5 \6 \7 \8 \9) true
         false)))

;;; reader

;; read-XXX: s->(s, it)

(defn- skip-whitespace [s]
  (loop [s s]
    (if-not (ws-char? (first s))
      s
      (recur (rest s)))))

(defmulti read (fn [s] (first s)))

(defn read-string [s]
  (let [[s x] (read s)]
    (assert (empty? (skip-whitespace s)))
    x))

;; ws

(doseq [c ws-chars]
  (defmethod read c [s]
    (read (skip-whitespace (rest s)))))

;; symbol literals: null true false

(defmethod read \n [s]
  (let [[cs s] (split-at 4 s)]
    (assert (= (apply str cs) "null"))
    [s nil]))

(defmethod read \t [s]
  (let [[cs s] (split-at 4 s)]
    (assert (= (apply str cs) "true"))
    [s true]))

(defmethod read \f [s]
  (let [[cs s] (split-at 5 s)]
    (assert (= (apply str cs) "false"))
    [s false]))

;; string

;; \u_XXXX
(defn- read-js-hex-char [s]
  (let [[cs s] (split-at 4 s)]
    (assert (= (count cs) 4))
    [s (char (hex->int (apply str cs)))]))

;; \_X
(defn- read-js-escaped-char [s]
  (case (char (first s))
    (\" \\ \/) [(rest s) (first s)]
    \b [(rest s) \backspace]
    \f [(rest s) \formfeed]
    \n [(rest s) \newline]
    \r [(rest s) \return]
    \t [(rest s) \tab]
    \u (read-js-hex-char (rest s))))

;; "_XXX"
(defn- read-js-string [s]
  (loop [s s sb (sb-make)]
    (let [c (first s)]
      (assert c)
      (case (char c)
        \" [(rest s) (sb-str sb)]
        \\ (let [[s c] (read-js-escaped-char (rest s))]
             (recur s (sb-conj-char sb c)))
        (recur (rest s) (sb-conj-char sb c))))))

(defmethod read \" [s]
  (read-js-string (rest s)))

;; number

;; NOTE: doesn't support e/E in numbers; for clj, parse number by
;; Long/parseLong and Double/parseDouble, doesn't support big number.

;; _-123 | _0.789
(defn- read-js-number [s]
  (let [sb (sb-make)
        [s sb] (if (not= (first s) \-)
                 [s sb]
                 [(rest s) (sb-conj-char sb \-)])
        [s sb] (do (assert (num-char? (first s)))
                   (if (= (first s) \0)
                     [(rest s) (sb-conj-char sb \0)]
                     (loop [s s sb sb]
                       (if-not (num-char? (first s))
                         [s sb]
                         (recur (rest s) (sb-conj-char sb (first s)))))))]
    (if (not= (first s) \.)
      [s (str->int (sb-str sb))]
      (let [s (rest s)
            sb (sb-conj-char sb \.)]
        (assert (num-char? (first s)))
        (loop [s s sb sb]
          (if-not (num-char? (first s))
            [s (str->float (sb-str sb))]
            (recur (rest s) (sb-conj-char sb (first s)))))))))

(doseq [c (conj num-chars \-)]
  (defmethod read c [s]
    (read-js-number s)))

;; array

;; [_] | [_ 1, 2, 3]
(defn- read-js-array [s]
  (let [s (skip-whitespace s)]
    (if (= (first s) \])
      [(rest s) []]
      (loop [s s a []]
        (let [[s v] (read s)
              s (skip-whitespace s)]
          (case (char (first s))
            \] [(rest s) (conj a v)]
            \, (recur (rest s) (conj a v))))))))

(defmethod read \[ [s]
  (read-js-array (rest s)))

;; object

(def ^:dynamic *read-keyfn* identity)

(defn- read-js-key [s]
  (let [[s k] (read s)]
    (assert (string? k))
    [s (*read-keyfn* k)]))

;; {_} | [_ "k1": 1, "k2": "2"]
(defn- read-js-object [s]
  (let [s (skip-whitespace s)]
    (if (= (first s) \})
      [(rest s) {}]
      (loop [s s o {}]
        (let [[s k] (read-js-key s)
              s (skip-whitespace s)]
          (assert (= (first s) \:))
          (let [[s v] (read (rest s))
                s (skip-whitespace s)]
            (case (char (first s))
              \} [(rest s) (assoc o k v)]
              \, (recur (rest s) (assoc o k v)))))))))

(defmethod read \{ [s]
  (read-js-object (rest s)))

;;; writer

(defprotocol IJSWritable
  (-js-write [this sb]))

(defn write [sb x]
  (if-not x
    (sb-conj-str sb "null")
    (-js-write x sb)))

(defn write-string [x]
  (-> (sb-make) (write x) sb-str))

;; string

(defn- write-js-string [sb s]
  (loop [s s sb (sb-conj-char sb \")]
    (let [c (first s)]
      (if-not c
        (sb-conj-char sb \")
        (case (char c)
          \\         (recur (rest s) (sb-conj-str sb "\\\\"))
          \"         (recur (rest s) (sb-conj-str sb "\\\""))
          \backspace (recur (rest s) (sb-conj-str sb "\\b"))
          \formfeed  (recur (rest s) (sb-conj-str sb "\\f"))
          \newline   (recur (rest s) (sb-conj-str sb "\\n"))
          \return    (recur (rest s) (sb-conj-str sb "\\r"))
          \tab       (recur (rest s) (sb-conj-str sb "\\t"))
          (recur (rest s) (sb-conj-char sb c)))))))

;; array

(defn- write-js-array [sb a]
  (if (empty? a)
    (sb-conj-str sb "[]")
    (loop [s (rest a)
           sb (-> (sb-conj-char sb \[)
                  (write (first a)))]
      (if (empty? s)
        (sb-conj-char sb \])
        (recur
         (rest s)
         (-> (sb-conj-char sb \,)
             (write (first s))))))))

;; object

(defn default-write-keyfn [k]
  (if (or (keyword? k) (symbol? k))
    (name k)
    (do
      (assert k)
      (str k))))

(def ^:dynamic *write-keyfn* default-write-keyfn)

(defn- write-js-key [sb k]
  (write-js-string sb (*write-keyfn* k)))

(defn- write-js-object [sb o]
  (if (empty? o)
    (sb-conj-str sb "{}")
    (loop [s (rest o)
           sb (let [[k v] (first o)]
                (-> (sb-conj-char sb \{)
                    (write-js-key k)
                    (sb-conj-char \:)
                    (write v)))]
      (if (empty? s)
        (sb-conj-char sb \})
        (recur
         (rest s)
         (let [[k v] (first s)]
           (-> (sb-conj-char sb \,)
               (write-js-key k)
               (sb-conj-char \:)
               (write v))))))))

;; extends

(extend-protocol IJSWritable
  ;; default

  nil
  (-js-write [this sb]
    (sb-conj-str sb (str this)))

  #?(:clj Number
     :cljs number)
  (-js-write [this sb]
    (sb-conj-str sb (str this)))

  ;; string like

  #?(:clj String
     :cljs string)
  (-js-write [this sb]
    (write-js-string sb this))

  #?(:clj clojure.lang.Keyword
     :cljs cljs.core/Keyword)
  (-js-write [this sb]
    (write-js-string sb (:name this)))

  #?(:clj clojure.lang.Symbol
     :cljs cljs.core/Symbol)
  (-js-write [this sb]
    (write-js-string sb (:name this)))

  ;; array like

  #?(:clj clojure.lang.PersistentVector
     :cljs cljs.core/PersistentVector)
  (-js-write [this sb]
    (write-js-array sb this))

  #?(:clj clojure.lang.PersistentList$EmptyList
     :cljs cljs.core/EmptyList)
  (-js-write [this sb]
    (write-js-array sb this))

  #?(:clj clojure.lang.PersistentList
     :cljs cljs.core/List)
  (-js-write [this sb]
    (write-js-array sb this))

  #?(:clj clojure.lang.PersistentHashSet
     :cljs cljs.core/PersistentHashSet)
  (-js-write [this sb]
    (write-js-array sb this))

  #?(:clj clojure.lang.PersistentTreeSet
     :cljs cljs.core/PersistentTreeSet)
  (-js-write [this sb]
    (write-js-array sb this))

  ;; object like

  #?(:clj clojure.lang.PersistentHashMap
     :cljs cljs.core/PersistentHashMap)
  (-js-write [this sb]
    (write-js-object sb this))

  #?(:clj clojure.lang.PersistentTreeMap
     :cljs cljs.core/PersistentTreeMap)
  (-js-write [this sb]
    (write-js-object sb this))

  #?(:clj clojure.lang.PersistentArrayMap
     :cljs cljs.core/PersistentArrayMap)
  (-js-write [this sb]
    (write-js-object sb this)))
