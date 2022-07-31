(ns serie)

; serie :: Integer -> Float -> Float
(defn serie [num-partidos prob-ganar-A]
  (let [array (make-array Double/TYPE (+ num-partidos 1) (+ num-partidos 1))
        prob-ganar-B (- 1 prob-ganar-A)]
    ; Primer bucle
    (loop [sec-col (range 1 (+ num-partidos 1))]
      (if (= sec-col '())
        array
        (let [col (first sec-col)]
          (aset array 0 col 1.0)
          (loop [sec-fila (range 1 (+ num-partidos 1))]
            (if (= sec-fila '())
              '()
              (let [fila (first sec-fila)
                    fila-ant (- fila 1)
                    valor-fila-ant (aget array fila-ant col)
                    col-ant (- col 1)
                    valor-col-ant (aget array fila col-ant)
                    valor (+ (* valor-fila-ant prob-ganar-A) (* valor-col-ant prob-ganar-B))]
                (aset array fila col valor)
                (recur (rest sec-fila)))))
          (recur (rest sec-col)))))))

;;; Ejemplo de utilización
(time (serie 10 0.6)) ; -> 118 ms
(time (serie 100 0.6)) ; -> 2606 ms
(time (serie 1000 0.6)) ; -> 83423 ms