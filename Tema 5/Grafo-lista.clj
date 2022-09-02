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

; Ejemplo de utilización
(require '[clojure.test :as test])
(let [nodos [:alfa, :beta, :gamma, :delta]
      aristas ['(:alfa :beta) '(:alfa :gamma) '(:beta :delta) '(:gamma :alfa) '(:gamma :beta) '(:gamma :delta)]
      grafo (nuevo-grafo-lista nodos aristas)
      obtener-valor (fn [numero-nodo]
                      (-> (.nodos grafo)
                        (get numero-nodo)
                        (.valor)))
      obtener-vecinos (fn [numero-nodo]
                        (-> (.nodos grafo)
                          (get numero-nodo)
                          (.vecinos)))]
  (test/testing "grafo lista tests"
    (test/is (= (obtener-valor 0) :alfa))
    (test/is (= (obtener-valor 1) :beta))
    (test/is (= (obtener-valor 2) :gamma))
    (test/is (= (obtener-valor 3) :delta))
    (test/is (= (obtener-vecinos 0) '(:beta :gamma)))
    (test/is (= (obtener-vecinos 1) '(:delta)))
    (test/is (= (obtener-vecinos 2) '(:alfa :beta :delta)))
    (test/is (= (obtener-vecinos 3) '()))))
