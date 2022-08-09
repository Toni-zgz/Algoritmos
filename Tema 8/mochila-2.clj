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
                    Inf- -1000
                    peso-i (get pesos-vec linea)
                    valor-i (get valores-vec linea)
                    valor-fila-ant (if (= linea 0) 
                                     Inf-
                                     (aget array (- linea 1) columna))
                    valor-columna-ant (if (< columna peso-i ) 
                                        Inf-
                                        (aget array linea (- columna peso-i)))
                    valor (max valor-fila-ant (+ valor-columna-ant valor-i))]
                (aset array linea columna valor)
                (recur (rest j-sec)))))
          (recur (rest i-sec))))))))

; mochila2 :: [Long] -> [Long] -> Long -> Long -> Long -> Long
(defn mochila-2-rec 
  ([pesos-vec valores-vec peso-max] 
   (let [ultima-fila (- (count pesos-vec) 1)
         ultima-columna peso-max]
     (mochila-2-rec pesos-vec valores-vec peso-max ultima-fila ultima-columna 0)))
  ([pesos-vec valores-vec peso-max fila columna valor-acc]
  (if (not (= (count pesos-vec) (count valores-vec)))
    (println "Error: los vectores pesos y valores no tienen la misma longitud")
    (let [peso-fila (get pesos-vec fila)
          valor-fila (get valores-vec fila)] 
      (cond 
        (and (= fila 0) (< columna peso-fila)) valor-acc
        (= fila 0) (mochila-2-rec pesos-vec valores-vec peso-max fila (- columna peso-fila) (+ valor-fila valor-acc))
        (< columna peso-fila) (mochila-2-rec pesos-vec valores-vec peso-max (dec fila) columna valor-acc)
        :else (max (mochila-2-rec pesos-vec valores-vec peso-max fila (- columna peso-fila) (+ valor-fila valor-acc)) (mochila-2-rec pesos-vec valores-vec peso-max (dec fila) columna valor-acc)))))))   

(def mochila-2-memo (memoize mochila-2-rec))

; Ejemplo de utilización
(pprint (mochila-2 [1 2 5 6 7] [1 6 18 22 28] 11))
(time (mochila-2 [1 2 5 6 7] [1 6 18 22 28] 11))
(time (mochila-2-rec [1 2 5 6 7] [1 6 18 22 28] 11))
(time (mochila-2-memo [1 2 5 6 7] [1 6 18 22 28] 11))