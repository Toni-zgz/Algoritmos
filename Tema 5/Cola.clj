(ns Cola)
(defprotocol IEncolable
  (que-queue [this, elt])
  (que-dequeue [this])
  (que-peek [this]))

(defprotocol IEquiv
  (equiv [this other]))

(deftype Cola [content]
  IEncolable
  (que-queue [this, elt]
    (if (vector? content)
      (let [list-nueva (concat [elt] content)
            vector-nuevo (into [] list-nueva)]
       (Cola. vector-nuevo))
      (println "Error: No es un objeto Cola")))
  (que-peek [this]
    (if (vector? content)
      (peek content)
      (println "Error: No es un objeto Cola")))
  (que-dequeue [this]
    (if (vector? content)
      (vector (peek content)(Cola. (pop content)))
      (println "Error: No es un objeto Cola")))
  IEquiv
  (equiv [this other] 
         (and 
          (= (type this) (type other))
          (= (.content this) (.content other)))))

;;; Ejemplo de utilización
(require '[clojure.test :as test])
(def a (->Cola []))
(def b (->Cola [3]))
(def c (->Cola [4 3]))
(def d (->Cola [4]))
(test/testing "cola-tests"
         (test/is (= (que-peek a) nil))
         (test/is (= (que-peek b) 3))
         (test/is (= (que-peek c) 3))
         (test/is (= (que-peek d) 4))
         (test/is (equiv (que-queue a 3) b))
         (test/is (equiv (que-queue b 4) c))
         (let [value (que-dequeue c)]
             (test/is (= (first value) 3))
             (test/is (equiv (second value) d)))
         (let [value (que-dequeue b)]
             (test/is (= (first value) 3))
             (test/is (equiv (second value) a)))
         (let [value (que-dequeue d)]
             (test/is (= (first value) 4))
             (test/is (equiv (second value) a))))
