(ns quicksort-lv)

; quicksort :: [Integer] -> [Integer]
(defn quicksort-lv [vect]
  (if (<= (count vect) 1)
    vect
    (let [num-elems (count vect)
          num-aleatorio (rand-int num-elems)
          pivote (get vect num-aleatorio)
          antes (subvec vect 0 num-aleatorio)
          despues (subvec vect (+ num-aleatorio 1))
          vect2 (into [] (concat antes despues))]
      (into [] (lazy-cat (quicksort-lv (into [] (filter #(<= % pivote) vect2)))
                         (vector pivote)
                         (quicksort-lv (into [](filter #(> % pivote) vect2))))))))

;; Ejemplo de utilización
(require '[clojure.test :as test])
(test/is (= (quicksort-lv [5 3 7 2 4 1 6]) [1 2 3 4 5 6 7]))