(ns n-reinas-lv)

; siguiente-reina :: [Long] -> Long -> (Long)
(defn siguiente-reina [reinas num-reinas]
  (let [rango (range 0 num-reinas)
        columna (count reinas)
        ; filas-prohibidas :: [Long] -> (Long)
       filas-prohibidas (fn [ary-reinas]
                           (let [longitud (count ary-reinas)]
                             (loop [idx 0
                                    ary-bucle []]
                               (if (= idx longitud)
                                 (distinct ary-bucle) ; Eliminamos duplicados.
                                 (let [fila (get ary-reinas idx)
                                       diag45 (+ fila (- longitud idx))
                                       diag135 (- fila (- longitud idx))
                                       nuevo-ary-bucle (conj ary-bucle fila diag45 diag135)]
                                   (recur (+ idx 1) nuevo-ary-bucle))))))
        ; no-esta-en :: (Long) -> (Long) -> (Long)
        no-esta-en (fn [lst-origen lst-excluidos]
                     (let [set-excluidos (set lst-excluidos)]
                       (filter (fn [elt] (nil? (get set-excluidos elt))) lst-origen)))]
    (->> reinas
      (into [])
      (filas-prohibidas)
      (no-esta-en rango)
      (sort))))

; n-reinas-lv :: Long -> ((Long), Bool)
(defn n-reinas-lv [num-reinas]
  (let [casillas (range 0 num-reinas)]
    (if (or (= num-reinas 2)
            (= num-reinas 3))
      '([] false)
      (loop [tablero []]
        (let [casillas-validas (into [] (siguiente-reina tablero num-reinas))
              numero-casillas-validas (count casillas-validas)
              numero-aleatorio (rand-int numero-casillas-validas)
              casilla-aleatoria (get casillas-validas numero-aleatorio)
              nuevo-tablero (conj tablero casilla-aleatoria)]
          (cond 
            (= casillas-validas []) '([] false)
            (= (count nuevo-tablero) num-reinas) (list nuevo-tablero true)
            :else (recur nuevo-tablero)))))))

; Ejemplo de uso
(require '[clojure.test :as test])
(test/testing "probando siguiente-reina"
              (test/is (= (siguiente-reina '() 8)
                          '(0 1 2 3 4 5 6 7)))
              (test/is (= (siguiente-reina '(3) 8)
                          '(0 1 5 6 7)))
              (test/is (= (siguiente-reina '(3 5) 8)
                          '(0 2 7)))
              (test/is (= (siguiente-reina '(3 5 2) 8)
                          '(4)))
              (test/is (= (siguiente-reina '(3 5 2 4) 8)
                          '(1 6)))
              (test/is (= (siguiente-reina '(3 5 2 4 1) 8) 
                          '(7)))
              (test/is (= (siguiente-reina '(3 5 2 4 1 7) 8)
                          '())))

(defn ejemplo [num-reinas]
  (let [solucion (n-reinas-lv num-reinas)]
    (if (second solucion)
      (first solucion)
      (ejemplo num-reinas))))