(ns mochila-va)

; mochila-va :: [Long] -> [Long] -> Long -> [(Long), Long]
(defn mochila-va [pesos valores peso-maximo]
  (loop [pesos-temp pesos       ; pesos candidatos a visitar. 
         valores-temp valores   ; valores candidatos a visitar.
         pesos-visitados '()    ; pesos visitados para una solución dada.
         valores-visitados '()  ; valores visitados para una solución dada.
         pesos-valores-def nil] ; almacenamiento definitivo de pesos y valores.
    (if (= pesos-temp '())
      (->> pesos-valores-def
           (sort (fn [elt1 elt2] (> (second elt1) (second elt2)))) ; ordenamos las soluciones en orden decreciente,
           (first)) ; y nos quedamos con la solución cuyo valor sea mayor.
      (let [peso-actual (first pesos-temp)
            valor-actual (first valores-temp)
            suma-pesos-procesados (+ peso-actual (reduce + pesos-visitados)) 
            nuevos-pesos-temp (if (<= suma-pesos-procesados peso-maximo)
                                (concat pesos (rest pesos-temp))
                                (rest pesos-temp))
            nuevos-valores-temp (if (<= suma-pesos-procesados peso-maximo)
                                  (concat valores (rest valores-temp))
                                  (rest valores-temp))
            nuevos-pesos-visitados (cond (and (> suma-pesos-procesados peso-maximo)
                                              (= peso-actual (last pesos))
                                              (not (= (first pesos-visitados) (last pesos)))) (rest pesos-visitados)
                                          (and (> suma-pesos-procesados peso-maximo)
                                              (= peso-actual (last pesos))
                                              (= (first pesos-visitados) (last pesos))) (rest (rest pesos-visitados))
                                   (<= suma-pesos-procesados peso-maximo) (cons peso-actual pesos-visitados)
                                   :else pesos-visitados)
            nuevos-valores-visitados (cond (and (> suma-pesos-procesados peso-maximo)
                                              (= peso-actual (last pesos))
                                              (not (= (first pesos-visitados) (last pesos)))) (rest valores-visitados)
                                          (and (> suma-pesos-procesados peso-maximo)
                                              (= peso-actual (last pesos))
                                              (= (first pesos-visitados) (last pesos))) (rest (rest valores-visitados))
                                           (<= suma-pesos-procesados peso-maximo) (cons valor-actual valores-visitados)
                                           :else valores-visitados)
            nuevos-pesos-valores-def (let [valor (reduce + valores-visitados)
                                           peso-valor (list pesos-visitados valor)]
                                       (if (and (> suma-pesos-procesados peso-maximo)
                                                (= peso-actual (last pesos)))
                                         (cons peso-valor pesos-valores-def)
                                         pesos-valores-def))]
        (recur nuevos-pesos-temp nuevos-valores-temp nuevos-pesos-visitados nuevos-valores-visitados nuevos-pesos-valores-def)))))

; Ejemplo de utilización
(require '[clojure.test :as test])
(test/testing "Tests del problema de la mochila resuelto por backtracking"
              (test/is (= (mochila-va [1 2] [3 5] 3) '((1 1 1) 9)))
              (test/is (= (mochila-va [1 2 3] [3 5 10] 4) '((1 3) 13)))
              (test/is (= (mochila-va [2 3 4 5] [3 5 6 10] 8) '((3 5) 15))))