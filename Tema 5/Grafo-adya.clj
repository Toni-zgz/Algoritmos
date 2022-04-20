(deftype Grafo-adya [valor adyacente]
  java.lang.Object
  (equals [this other]
     (and
        (= (type this) (type other))
        (= (.valor this) (.valor other))
        (= (.adyacente this) (.adyacente other)))))

(defn nuevo-grafo-adya [num-elems]
  (let [vec-valor (make-array Integer/TYPE num-elems)
        ary-adyac (make-array Boolean/TYPE num-elems num-elems)]
    (->Grafo-adya vec-valor ary-adyac)))
