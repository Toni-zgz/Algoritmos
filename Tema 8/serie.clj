(ns serie)

; serie :: Integer -> Float -> Float
(defn serie [num-partidos prob-ganar-A]
  (let [array (make-array Double/TYPE (+ num-partidos 1) (+ num-partidos 1))
        prob-ganar-B (- 1 prob-ganar-A)]
    ; Primer bucle
    (loop [sec-s (range 1 (+ num-partidos 1))]
      (if (= sec-s '())
        '()
        ; Ponemos la primera fila con 1s
        (let [s (first sec-s)]
          (aset array 0 s 1.0)
          ; Rellenamos desde la esquina superior izquierda
          ; hasta la diagonal secundaria
          (loop [sec-k (range 1 s)]
            (if (= sec-k '())
              '()
              (let [k (first sec-k)
                    fila-anterior (aget array (- k 1) (- s k))
                    columna-anterior (aget array k (- s k 1))
                    nuevo-valor (+ (* fila-anterior prob-ganar-A) (* columna-anterior prob-ganar-B))]
                (aset array k (- s k) nuevo-valor)
                (recur (rest sec-k)))))
          (recur (rest sec-s)))))
    ; Por ultimo, rellenamos desde la diagonal secundaria
    ; hasta la esquina inferior derecha
    (loop [sec-s (range 1 (+ num-partidos 1))]
      (if (= sec-s '())
        array
        (let [s (first sec-s)]
          (loop [sec-k (range 0 (+ (- num-partidos s) 1))]
            (if (= sec-k '())
              '()
              (let [k (first sec-k)
                    fila-anterior (aget array (- (+ s k) 1) (- num-partidos k))
                    columna-anterior (aget array (+ s k) (- num-partidos k 1))
                    nuevo-valor (+ (* fila-anterior prob-ganar-A) (* columna-anterior prob-ganar-B))]
                (aset array (+ s k) (- num-partidos k) nuevo-valor)
                (recur (rest sec-k)))))
          (recur (rest sec-s)))))))

;;; Ejemplo de utilización
(time (serie 10 0.6)) ; -> 226 ms
(time (serie 100 0.6)) ; -> 2608 ms
(time (serie 1000 0.6)) ; -> 77328 ms