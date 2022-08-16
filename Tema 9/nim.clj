(ns nim)

; ganarec :: Long -> Long -> Boolean
(defn ganarec [i j]
  (println i j)
  (if (< i j)
    (println "Error: el segundo parámetro no puede ser mayor que el primero")
    (loop [k-sec (range 1 (+ j 1))]
      (if (= k-sec '())
        false
        (let [k (first k-sec)
              i-k (- i k)
              k2 (* 2 k)
              minimo (min i-k k2)]
          (if (not (ganarec i-k minimo))
            true
            (recur (rest k-sec))))))))

; nim :: Long -> Long -> Boolean
(def nim (memoize ganarec))

; Ejemplo de utilización
(require '[clojure.test :as test])
(test/testing "nim-tests"
              (test/is (= (nim 5 4) false))
              (test/is (= (nim 2 2) true))
              (test/is (= (nim 4 2) true))
              (test/is (= (nim 3 3) true))
              (test/is (= (nim 0 0) false))
              (test/is (= (nim 4 3) true))
              (test/is (= (nim 3 2) false))
              (test/is (= (nim 1 1) true))
              (test/is (= (nim 2 1) false)))