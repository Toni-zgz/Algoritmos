(ns prim)

; Tipo Grafo
(deftype Grafo-adya [valor adyacente]
  java.lang.Object
  (equals [this other]
     (and
        (= (type this) (type other))
        (= (.valor this) (.valor other))
        (= (.adyacente this) (.adyacente other)))))

(defn nuevo-grafo-adya [nodos aristas]
  (let [num-elems (count nodos)
        rango (range 1 (+ num-elems 1))
        ary-adyac (make-array Boolean/TYPE num-elems num-elems)
        indices (range (count aristas))
        mapeo-nodos (zipmap nodos indices)
        lista-indices (map (fn [elt] (map (fn [elt2] (get mapeo-nodos elt2)) elt)) aristas)]
    (dorun (map (fn [elt] (aset ary-adyac (first elt) (second elt) true)) lista-indices))
    (->Grafo-adya nodos ary-adyac)))

; buscar :: [(a, a)] -> [a] -> (a, a)
(defn buscar [aristas vector-nodos]
  (loop [aristas-bucle aristas]
    (let [arista (first aristas-bucle)
          elt1  (first arista)
          elt2  (second arista)
          esta-en-vector? (fn [valor] (> (count (filter (fn [elt] (= elt valor)) vector-nodos)) 0))
          elt1-en-vector (esta-en-vector? elt1)
          elt2-en-vector (esta-en-vector? elt2)]
      (if (or (and elt1-en-vector
                   (not elt2-en-vector))
              (and (not elt1-en-vector)
                   elt2-en-vector)) 
        arista
        (recur (rest aristas-bucle))))))

; unir :: [a] -> (a, a) -> [a]
(defn unir [vector par]
  (let [primer (first par)
        segundo (second par)
        temp  (conj vector primer)
        final (conj temp segundo)]
    (into [] (distinct final))))

; quitar :: [(a, a)] -> (a, a) -> [(a, a)]
(defn quitar [aristas arista]
  (filter (fn [elt] (not (= elt arista))) aristas))

; prim :: Grafo -> [((a, a), Int)] -> [(a, a)]
(defn prim [grafo aristas-long]
  (let [vector-T (vector)
        vector-N (.valor grafo)
        longitud-N (count vector-N)
        elemento-inicial (rand-int longitud-N)
        vector-B (vector elemento-inicial)
        aristas-long-ord (sort (fn [elt1 elt2]
                            (let [dist-elt1 (second elt1)
                                  dist-elt2 (second elt2)]
                               (< dist-elt1 dist-elt2))) 
                          aristas-long)
        aristas-ord (map #(first %) aristas-long-ord)] 
    (loop [vector-B-bucle vector-B
           vector-salida []
           aristas-bucle aristas-ord]
      (if (= (sort vector-B-bucle) (sort vector-N))
        vector-salida
        (let [arista (buscar aristas-bucle vector-B-bucle)
              nuevo-vector-B (unir vector-B-bucle arista)
              nuevo-vector-salida (conj vector-salida arista)
              nuevo-aristas-long (quitar aristas-bucle arista)]
          (recur nuevo-vector-B nuevo-vector-salida nuevo-aristas-long))))))

;;; Ejemplo de utilización
;(require '[clojure.test :as test])
(def ars-dist ['((1 2) 1) '((1 4) 4) '((2 3) 2) '((2 4) 6) '((2 5) 4) '((3 5) 5) '((3 6) 6) '((4 5) 3) '((4 7) 4) '((5 6) 8) '((5 7) 7) '((6 7) 3)])
(def nod (into [] (range 1 8)))
(def ars (into [] (map #(first %) ars-dist)))
(def g (nuevo-grafo-adya nod ars))
(prim g ars-dist)