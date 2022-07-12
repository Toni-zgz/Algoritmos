(ns ordenar-por-fusion)

; fusionar :: [Integer] -> [Integer] -> (Integer)
; esta función fusiona 2 vectores en una lista ordenada de menor
; a mayor.
(defn fusionar [vector1 vector2]
  (cond (= vector1 []) vector2
        (= vector2 []) vector1
        :else (if (> (first vector1)
                     (first vector2))
                (cons (first vector2) (fusionar vector1 (rest vector2)))
                (cons (first vector1) (fusionar (rest vector1) vector2)))))

; ordenar-por-fusion :: [Integer] -> [Integer]
; esta función ordena un vector de menor a mayor utilizando
; el algoritmo de fusión.
(defn ordenar-por-fusion [vect]
  (if (< (count vect) 2)
    vect
    (let [medio (int (/ (count vect) 2))
          vect1 (subvec vect 0 medio)
          vect2 (subvec vect medio)
          vect1-ord (ordenar-por-fusion vect1) 
          vect2-ord (ordenar-por-fusion vect2)]
     (into [] (fusionar vect1-ord vect2-ord)))))

;; Ejemplo de utilización
(require '[clojure.test :as test])
(test/is (= (ordenar-por-fusion [5 3 7 2 4 1 6]) [1 2 3 4 5 6 7]))