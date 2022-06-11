(ns mochila)

; mochila :: [Integer] -> [Integer] -> Integer -> [Float]
(defn mochila [pesos valores peso-total]
  (let [n (count pesos)
        salida (into [] (repeat n 0))
        peso 0
        indices (range 1 (+ n 1))
        comb-ord (->> (map (fn [elt1 elt2 elt3] (list elt1 elt3 (/ (float elt2) (float elt1)))) pesos valores indices)
                      (sort (fn [elt1 elt2] (> (last elt1) (last elt2)))))]
    (loop [peso-bucle peso
           salida-bucle salida
           comb-bucle comb-ord]
      (if (>= peso-bucle peso-total)
        (into [] (map float salida-bucle))
        (let [objeto (first comb-bucle)
              peso (first objeto)
              indice (second objeto)
              xi (if (<= (+ peso-bucle peso) peso-total)
                   1
                   (/ (- peso-total peso-bucle) peso))
              nuevo-salida (assoc salida-bucle (- indice 1) xi )
              nuevo-peso (if (<= (+ peso-bucle peso) peso-total)
                   (+ peso-bucle peso)
                   peso-total)]
         (recur nuevo-peso nuevo-salida (rest comb-bucle)))))))

;;; Ejemplo de utilización
(let [pesos [10 20 30 40 50]
      valores [20 30 66 40 60]
      moch-final (mochila pesos valores 100)]
  (->> (map (fn [elt1 elt2] (* elt1 elt2)) moch-final valores)
    (reduce +)
    (int)))