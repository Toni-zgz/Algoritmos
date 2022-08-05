(ns mochila-2)

; mochila2 :: [Long] -> [Long] -> Long -> [Long]
(defn mochila-2 [pesos-vec valores-vec peso-max]
  (if (not (= (count pesos-vec) (count valores-vec)))
    (println "Error: los vectores pesos y valores no tienen la misma longitud")
    (let [numero-pesos (count pesos-vec)
          array (make-array Long/TYPE numero-pesos (+ peso-max 1))]
      (loop [i-sec (range 0 numero-pesos)]
        (if (= i-sec '())
        array
        (let [linea (first i-sec)]
          (loop [j-sec (range 1 (+ peso-max 1))]
            (if (= j-sec '())
              '()
              (let [columna (first j-sec)
                    neg -1
                    peso-i (get pesos-vec linea)
                    valor-i (get valores-vec linea)
                    valor (cond
                            (and (= linea 0) (< columna peso-i)) neg
                            (= linea 0) (+ (aget array linea (- columna peso-i)) valor-i)
                            (< columna peso-i) (aget array (- linea 1) columna)
                            :else (max (aget array (- linea 1) columna) (+ (aget array linea (- columna peso-i)) valor-i)))]
                (aset array linea columna valor)
                (recur (rest j-sec)))))
          (recur (rest i-sec))))))))

; Ejemplo de utilización
(pprint (mochila-2 [1 2 5 6 7] [1 6 18 22 28] 11))