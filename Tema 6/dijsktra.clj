(ns dijkstra)

; dijkstra :: [Integer, Integer] -> [Integer]
; A partir de una matriz bidimensional de distancias entre nodos
; numéricos, obtenemos la minima distancia entre el nodo 1 (raiz)
; y el resto de los nodos.
; dijkstra :: [Integer, Integer] -> [Integer]
; A partir de una matriz bidimensional de distancias entre nodos
; numéricos, obtenemos la minima distancia entre el nodo 1 (raiz)
; y el resto de los nodos.
(defn dijkstra [array-dist]
  ; iniciamos la matriz D[2..n]
   (let [n (alength array-dist)
        ; para i = 2 hasta n hacer D[i] <- array-dist[0, i]
         D (loop [i 2
                  temp (vector)]
             (if (> i n)
               temp
               (recur (+ i 1) (conj temp (aget array-dist 0 (- i 1))))))
         rango (range 2 (+ n 1))
        ; C <- [[2 dist-2],[3 dist-3]..[n dist-n]]
         C (into [] (zipmap rango D))]

  ; Bucle voraz
  ; repetir hasta que C esta vacio
     (loop [C-bucle C
            D-bucle D]
      ; si C está vacio, devolver D
       (if (= (count C-bucle) 1)
         D-bucle
    ; v <- algun elemento de C qye minimiza D[v]
         (let [C-ord (into [] (sort-by second C-bucle))
               v (first (first C-ord))
    ; C <- C - {v}
               nuevo-C (rest C-ord)
    ; para cada w perteneciente a C hacer
    ; D[w] <- min(D[w], D[v] + array-dist[v, w])
               nuevo-D (loop [vector-idx nuevo-C
                              vector-valor D-bucle]
                         (if (= vector-idx [])
                           vector-valor
                           (let [w (first (first vector-idx))
                                 D-w (get vector-valor (- w 2))
                                 D-v (get vector-valor (- v 2))
                                 desp (aget array-dist (- v 1) (- w 1))
                                 desp-corr (if (= desp 0)
                                             1000000
                                             desp)
                                 valor (min D-w (+ D-v desp-corr))
                                 nuevo-vector-valor (assoc vector-valor (- w 2) valor)]
                             (recur (rest vector-idx) nuevo-vector-valor))))]
           (recur nuevo-C nuevo-D))))))

;; Ejemplo de utilización
(require '[clojure.test :as test])
(def numero-nodos 5)
(def matriz (make-array Integer/TYPE numero-nodos numero-nodos))
(def ars-dist ['((1 2) 50) '((1 3) 30) '((1 4) 100) '((1 5) 10) '((3 2) 5) '((4 2) 20) '((4 3) 50) '((5 4) 10)])
(run! (fn [elt]
        (let [coord (first elt)
              fila (- (first coord) 1)
              col (- (second coord) 1)
              dist (second elt)]
          (aset-int matriz fila col dist))) ars-dist)
(test/is (= (dijkstra matriz) [35 30 20 10]))