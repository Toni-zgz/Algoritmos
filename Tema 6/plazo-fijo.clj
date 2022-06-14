(ns plazo-fijo)

; plazo-fijo :: [Integer] -> [Integer]
; esta función ordena una secuencia de tiempos en una
; secuencia de acciones 
(defn plazo-fijo [tiempos]
  (let [max-tiempos (apply max tiempos)
        numero-elementos (count tiempos)
        p (min max-tiempos numero-elementos)
        F (into [] (range 0 (+ p 1)))
        j (into [] (take p (repeat 0)))]
    (->> (loop [indice-bucle 1
                j-bucle j
                F-bucle F]
           (if (> indice-bucle numero-elementos)
             (into [] j-bucle)
           (let [di (get tiempos (- indice-bucle 1))
                 k (min p di)
                 nuevo-indice (+ indice-bucle 1)
                 m (get F-bucle k)]
             (if (= m 0)
               (recur nuevo-indice j-bucle F-bucle)
               (let [nuevo-j (assoc j-bucle (- m 1) indice-bucle)
                     Fl (get F-bucle (- m 1))
                     nuevo-F (assoc F-bucle k Fl)]                 
                 (recur nuevo-indice nuevo-j nuevo-F))))))
      (filter (fn [elt] (> elt 0))))))

;;; Ejemplo de utilización
(require '[clojure.test :as test])
(test/is (= (plazo-fijo [3 1 1 3 1 3]) [2 4 1]))