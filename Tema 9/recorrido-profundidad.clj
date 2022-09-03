(ns recorrido-profun)

; definimos tipos
(deftype Nodo [valor vecinos]
  java.lang.Object
  (equals [this other]
     (and
        (= (type this) (type other))
        (= (.valor this) (.valor other))
        (= (.vecinos this) (.vecinos other)))))

(deftype Grafo-lista [nodos]
  java.lang.Object
  (equals [this other]
     (and
        (= (type this) (type other))
        (= (.nodos this) (.nodos other)))))

; nuevo-grafo-lista :: [Long] -> [(Long, Long)] -> Grafo-lista
(defn nuevo-grafo-lista [nodos aristas]
  (let [hallar-vecinos (fn [nodo]
                         (let []
                           (->> aristas
                             (filter (fn [elt]
                                       (= (first elt) nodo)))
                             (map (fn [elt]
                                    (second elt))))))
        func-mapeo (fn [elt]
                     (let [vecinos (hallar-vecinos elt)]
                       (->Nodo elt vecinos)))
        ary-nodos (into [] (map func-mapeo nodos))
        grafo (->Grafo-lista ary-nodos)]
    grafo))

; in? :: [a] -> Boolean
; esta función devuelve true si valor esta en el vector
; y false en caso contrario.
(defn in? [vect valor]
  (loop [vect-bucle vect]
    (if (= vect-bucle [])
      false
      (if (= (first vect-bucle) valor)
        true
        (recur (rest vect-bucle))))))

; recorrido-p :: Grafo -> [Nodo]
(defn recorrido-p [grafo]
  (let [nodos (->> grafo
                (.nodos)
                (map (fn [elt] (.valor elt))))
        vecinos (->> grafo
                  (.nodos)
                  (map (fn [elt] (.vecinos elt)))
                  (zipmap nodos))]
        (loop [lst-nodos nodos
                  visitados []]
             (if (= lst-nodos [])
               visitados
               (let [nodo-actual (first lst-nodos)
                     vecinos-nodo (vecinos nodo-actual)
                     nueva-lst-nodos (if (in? visitados nodo-actual)
                                       (rest lst-nodos)
                                       (concat vecinos-nodo (rest lst-nodos)))
                     nuevo-visitados (if (in? visitados nodo-actual)
                                       visitados
                                       (conj visitados nodo-actual))]
                 (recur nueva-lst-nodos nuevo-visitados))))))

; Ejemplo de utilizacion grafo no dirigido
(require '[clojure.test :as test])
(let [nodos (range 1 9)
      aristas '((1 2) (1 3) (1 4) (2 1) (2 3) (2 5) (2 6) (3 1) (3 2) (3 6) (4 1) (4 7) (4 8) (5 2) (5 6) (6 2) (6 3) (6 5) (7 4) (7 8) (8 4) (8 7))
      grafo (nuevo-grafo-lista nodos aristas)]
  (test/is (= (recorrido-p grafo) [1 2 3 6 5 4 7 8])))
; Ejemplo de utilización grafo dirigido
(let [nodos (range 1 9)
      aristas '((1 2) (1 4) (1 8) (2 3) (3 1) (4 8) (5 2) (5 6) (6 2) (6 3) (6 5) (7 4) (8 7))
      grafo (nuevo-grafo-lista nodos aristas)]
  (test/is (= (recorrido-p grafo) [1 2 3 4 8 7 5 6])))