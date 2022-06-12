(ns minimizacion)

; minimiza :: [Integer] -> [Integer]
; Esta función, a partir de una lista de tiempos, 
; nos devuelve una lista de clientes ordenada
; en la cual la suma de tiempos es la optima.
(defn minimiza [tiempos]
  (->> (count tiempos)
    (+ 1)
    (range 1)
    (zipmap tiempos)
    (sort (fn [elt1 elt2]
            (< (first elt1) (first elt2))))
    (map (fn [elt] (second elt)))))

;; Ejemplo de utilización
(require '[clojure.test :as test])
(test/is (= (minimiza [5 10 3]) '(3 1 2)))