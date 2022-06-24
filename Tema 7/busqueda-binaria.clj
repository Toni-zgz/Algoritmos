(ns busqueda-binaria)

; busqueda-binaria :: [Int] -> Int -> Int
; esta función busca el elemento especificado en el 2º argumento
; en el vector del 1º argumento, devolviendo la posición en la
; que se encuentra el elemento o -1 si el elemento no está presente.
; Como precondición, se establece que el vector del 1er argumento
; está ordenado.
(defn busqueda-binaria [vect elt]
  (let [num-elem (count vect)
        ultimo (get vect (- num-elem 1))]
    (if (or (= num-elem 0)
            (> elt ultimo))
      -1
      (loop [indice-inf 1
             indice-sup num-elem]
        (if (= indice-inf indice-sup)
          indice-inf
          (let [indice-medio (int (/ (+ indice-inf indice-sup) 2))
                valor_medio (get vect (- indice-medio 1))]
            (if (<= elt valor_medio)
              (recur indice-inf indice-medio)
              (recur (+ indice-medio 1) indice-sup))))))))

;; Ejemplo de utilización
(require '[clojure.test :as test])
(test/is (= (busqueda-binaria [3 5 10] 5) 2))
(test/is (= (busqueda-binaria [3 5 10] 51) -1))