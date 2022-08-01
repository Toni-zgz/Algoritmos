(ns monedas)

; monedas :: Long -> [Long] -> [Long, Long]
(defn monedas [cantidad monedas-vec]
  (let [numero-monedas (count monedas-vec)
        array (make-array Long/TYPE numero-monedas (+ cantidad 1))]
    (loop [i-sec (range 0 numero-monedas)]
      (if (= i-sec '())
        array
        (let [linea (first i-sec)]
          (loop [j-sec (range 1 (+ cantidad 1))]
            (if (= j-sec '())
              '()
              (let [columna (first j-sec)
                    inf 10000
                    moneda-i (get monedas-vec linea)
                    valor (cond
                            (and (= linea 0) (< columna moneda-i)) inf
                            (= linea 0) (+ 1 (aget array linea (- columna moneda-i)))
                            (< columna moneda-i) (aget array (- linea 1) columna)
                            :else (min (aget array (- linea 1) columna) (+ 1 (aget array linea (- columna moneda-i)))))]
                (aset array linea columna valor)
                (recur (rest j-sec)))))
          (recur (rest i-sec)))))))

; Ejemplo de utilización
(pprint (monedas 8 [1 4 6]))