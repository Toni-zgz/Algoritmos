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

(defn nuevo-grafo-lista [num-elems]
  (let [ary-nodos (make-array Integer/TYPE num-elems)]
    (->Grafo-lista ary-nodos)))
