(ns voraz)

; voraz :: [Integer] -> ([Integer] -> Bool) -> ([Integer] -> Integer -> Bool) -> 
;          ([Integer] -> Integer) -> ([Integer] -> [Integer]) -> [Integer]
; esta función define una plantilla para implementar
; algoritmos voraces.
; Para utilizarla solo hay que definir los predicados
; solucion? y factible?, las funciones seleccionar y
; extraer y llamarla con un conjunto de datos.
(defn voraz [conjunto solucion? factible? seleccionar extraer]
  (let [solucion (vector)]
    (loop [conjunto-bucle conjunto
           solucion-bucle solucion]
      (cond
        (solucion? solucion-bucle) solucion-bucle
        (= conjunto-bucle []) "No hay soluciones"
        :else (let [x (seleccionar conjunto-bucle)
                    nuevo-conjunto (extraer conjunto-bucle)
                    nueva-solucion (if (factible? solucion-bucle x)
                                     (conj solucion-bucle x)
                                     solucion-bucle)]
                (recur nuevo-conjunto nueva-solucion))))))

;;; Ejemplo de utilización
(defn devolver-cambio [n]
  (let [solucion? (fn [secuencia] (->> secuencia
                                       (reduce +)
                                       (= n)))
        factible? (fn [secuencia elt] (->> secuencia
                                           (reduce +)
                                           (+ elt) 
                                            (>= n)))
        seleccionar (fn [secuencia] (first secuencia))
        extraer (fn [secuencia] (rest secuencia))
        conjunto [100 25 10 5 1]]
    (voraz conjunto solucion? factible? seleccionar extraer)))