(ns quicksort)

; quicksort :: [Integer] -> [Integer]
(defn quicksort [vect]
  (let [pivote (first vect)
        vect2 (rest vect)]
    (if (<= (count vect) 1)
      vect
      (into [] (lazy-cat (quicksort (filter #(<= % pivote) vect2))
                (vector pivote)
                (quicksort (filter #(> % pivote) vect2))))))) 

;; Ejemplo de utilización
(require '[clojure.test :as test])
(test/is (= (quicksort [5 3 7 2 4 1 6]) [1 2 3 4 5 6 7]))