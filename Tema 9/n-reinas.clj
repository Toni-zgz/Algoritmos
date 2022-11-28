(ns n-reinas)

; movimientos-validos :: (Long) -> Long -> (Long)
(defn movimientos-validos [lista num-filas]
  (let [rango (range 0 num-filas)
        columna (count lista)
        ; filas-prohibidas :: [Long] -> (Long)
        filas-prohibidas (fn [ary]
                           (let [long (count ary)]
                             (loop [idx 0
                                    ary2 []]
                               (if (= idx long)
                                 (distinct ary2) ; Eliminamos duplicados.
                                 (let [fila (get ary idx)
                                       diag45 (+ fila (- long idx))
                                       diag135 (- fila (- long idx))
                                       nuevo-ary2 (conj ary2 fila diag45 diag135)]
                                   (recur (+ idx 1) nuevo-ary2))))))
        ; no-esta-en :: (Long) -> (Long) -> (Long)
        no-esta-en (fn [lst-origen lst-excluidos]
                (let [lst-origen-ord (sort lst-origen)
                      lst-excluidos-ord (sort lst-excluidos)]
                  (loop [lst-origen-bucle lst-origen-ord
                         lst-excluidos-bucle lst-excluidos-ord
                         lst-salida '()]
                    (cond (= lst-origen-bucle '()) lst-salida
                      (= lst-excluidos-bucle '()) (concat lst-salida lst-origen-bucle)
                      :else (let [elem-origen (first lst-origen-bucle)
                                  elem-excluidos (first lst-excluidos-bucle)
                                  nueva-lst-origen (if (<= elem-origen elem-excluidos)
                                                     (rest lst-origen-bucle)
                                                     lst-origen-bucle)
                                  nueva-lst-excluidos (if (< elem-origen elem-excluidos)
                                                        lst-excluidos-bucle
                                                        (rest lst-excluidos-bucle))
                                  nueva-lst-salida (if (< elem-origen elem-excluidos)
                                                     (conj lst-salida elem-origen)
                                                     lst-salida)]
                              (recur nueva-lst-origen nueva-lst-excluidos nueva-lst-salida))))))]
    (->> lista
      (into [])
      (filas-prohibidas)
      (no-esta-en rango)
      (sort))))

(defn n-reinas [num-reinas]
  (let [casillas (range 0 num-reinas)]
    (loop [casillas-temp casillas       
           casillas-visitadas '()    
           soluciones '()] 
      ;(println casillas-temp "---"  casillas-visitadas)
      (if (= casillas-temp '())
        (into [] (distinct soluciones))
        (let [casilla-actual (first casillas-temp)
              casillas-totales (conj (into [] casillas-visitadas) casilla-actual)
              movimientos (if (= casilla-actual :ultimo)
                            '()
                            (movimientos-validos casillas-totales num-reinas))
              movimientos-def (if (= movimientos '())
                                '()
                                (concat movimientos [:ultimo]))
              nuevas-casillas-temp (cond (= casilla-actual :ultimo) (rest casillas-temp)
                                         (= movimientos-def '()) (rest casillas-temp)
                                         :else (concat movimientos-def (rest casillas-temp)))
              nuevas-casillas-visitadas (cond (= casilla-actual :ultimo) (pop casillas-visitadas)
                                              (= movimientos-def '()) casillas-visitadas
                                              :else casillas-totales)
              nuevas-soluciones (cond (= casilla-actual :ultimo) soluciones
                                      (= (count casillas-totales) num-reinas) (cons casillas-totales soluciones)
                                      :else soluciones)]
          (recur nuevas-casillas-temp nuevas-casillas-visitadas nuevas-soluciones))))))

; Ejemplo de utilización
(require '[clojure.test :as test])
(test/testing "Tests del problema de las n-reinas resuelto por backtracking"
              (test/is (= (count (n-reinas 1))  1))
              (test/is (= (count (n-reinas 2))  0))
              (test/is (= (count (n-reinas 3))  0))
              (test/is (= (count (n-reinas 4))  2))
              (test/is (= (count (n-reinas 5)) 10))
              (test/is (= (count (n-reinas 6))  4))
              (test/is (= (count (n-reinas 7)) 40))
              (test/is (= (count (n-reinas 8)) 92)))