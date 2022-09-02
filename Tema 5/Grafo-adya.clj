deftype Grafo-adya [valor adyacente]
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

;;; Ejemplo de utilización
(require '[clojure.test :as test])
(def nodos [:alfa, :beta, :gamma, :delta])
(def aristas ['(:alfa :beta) '(:alfa :gamma) '(:beta :delta) '(:gamma :alfa) '(:gamma :beta) '(:gamma :delta)])
(def grafo (nuevo-grafo-adya nodos aristas))