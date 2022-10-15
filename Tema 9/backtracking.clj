(ns backtracking)

(defn backtracking [arg]
  (let [nodos (range 0 arg)]
    (loop [nodos-sin-visitar nodos       
           nodos-visitados '()    
           soluciones '()] 
      ;(println nodos-sin-visitar "---"  nodos-visitados)
      (if (= nodos-sin-visitar '())
        (devolver soluciones))
        (let [nodo-actual (first nodos-sin-visitar)
              casillas-totales (conj (into [] nodos-visitados) nodo-actual)
              movimientos (if (= nodo-actual :ultimo)
                            '()
                            (seleccionar casillas-totales arg))
              movimientos-def (if (= movimientos '())
                                '()
                                (concat movimientos [:ultimo]))
              nuevos-nodos-sin-visitar (cond (= nodo-actual :ultimo) (rest nodos-sin-visitar)
                                         (= movimientos-def '()) (rest nodos-sin-visitar)
                                         :else (concat movimientos-def (rest nodos-sin-visitar)))
              nuevos-nodos-visitados (cond (= nodo-actual :ultimo) (pop nodos-visitados)
                                              (= movimientos-def '()) nodos-visitados
                                              :else casillas-totales)
              nuevas-soluciones (cond (= nodo-actual :ultimo) soluciones
                                      (solucion? casillas-totales arg) (cons casillas-totales soluciones)
                                      :else soluciones)]
          (recur nuevos-nodos-sin-visitar nuevos-nodos-visitados nuevas-soluciones))))))

; Ejemplo de utilización
(defn devolver [arg]
  ...)

(defn seleccionar [arg1 arg2]
  ...)

(defn solucion? [arg1 arg2]
  ...)

(defn problema [arg]
  (backtracking arg))