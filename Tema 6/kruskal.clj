(ns kruskal)

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

;Protocolos de Particion
(defprotocol IParticion
  (buscar [this elt])
  (fusionar [this elt1 elt2]))

;Tipo Particion
(deftype Particion [conjunto altura]
  IParticion
  ; buscar :: Particion -> a -> a
  ; Este método busca la raiz del grupo en el que está el argumento
  ; dentro de la partición.
  (buscar [this elt]
    (loop [r elt]
                 (let [nueva-r (get conjunto (- r 1))]
                   (if (= r nueva-r) 
                     r
                     (recur nueva-r)))))

  ; fusionar :: Particion -> a -> a -> Particion
  (fusionar [this elt1 elt2]
        (let [altura-1 (get altura (- elt1 1))
              altura-2 (get altura (- elt2 1))]
          (if (= altura-1 altura-2)
            (let [nueva-altura-1 (+ altura-1 1)
                  nueva-altura (assoc altura (- elt1 1) nueva-altura-1)
                  nuevo-conjunto (assoc conjunto (- elt2 1) elt1)]
              (Particion. nuevo-conjunto nueva-altura))
            (if (> altura-1 altura-2)
              (let [nuevo-conjunto (assoc conjunto (- elt2 1) elt1)]
                (Particion. nuevo-conjunto altura))
              (let [nuevo-conjunto (assoc conjunto (- elt1 1) elt2)]
                (Particion. nuevo-conjunto altura))))))

  java.lang.Object
  (equals [this other]
     (and
        (= (type this) (type other))
        (= (.conjunto this) (.conjunto other))
        (= (.altura this) (.altura other)))))

; nueva-particion :: Integer -> Particion
; Esta función crea una particion inicializada
; con el numero de elementos que se da como
; argumento.
(defn nueva-particion [num-elems]
  (let [rango (range 1 (+ num-elems 1))
        conjunto (into [] rango)
        altura (into [] (map (fn [elt] 0) rango))]
    (->Particion conjunto altura)))

;kruskal :: Grafo -> [((a, a), Int)] -> [(a, a)]
(defn kruskal [grafo aristas]
  ; se ordena el vector de aristas de menor a mayor longitud
  (let [aristas-ord (sort (fn [elt1 elt2]
                            (let [dist-elt1 (second elt1)
                                  dist-elt2 (second elt2)]
                               (< dist-elt1 dist-elt2))) 
                          aristas)
        ; n = numero de nodos que hay en el grafo      
        n (count (.valor grafo))
        ; Iniciamos el vector de salida (T)
        T (vector)
        ; iniciamos n conjuntos cada uno de los cuales contiene un 
        ; elemento distinto de N
        particion (nueva-particion n)]
    ; repetir 
    (loop [aristas aristas-ord
           particion particion
           salida T]
          ; si salida contiene n - 1 aristas devolvemos salida
          (if (= (count salida) (- n 1))
            salida
            ; cogemos la arista mas corta, aun no considerada (u, v)
            (let [arista (-> aristas
                           (first)
                           (first))
                  ; compu = buscar(u)
                  compu (buscar particion (first arista))
                  ; compv = buscar(v)
                  compv (buscar particion (second arista))
                  ; si compu <> compv entonces
                  ; fusionar (compu, compv)
                  nueva-particion (if (not (= compu compv))
                                    (fusionar particion compu compv)
                                    particion)
                  ; T = T U {e}
                  nueva-salida (if (not (= compu compv))
                                    (conj salida arista)
                                    salida)
                  nuevo-aristas (rest aristas)]

              ; cerramos el bucle
              (recur nuevo-aristas nueva-particion nueva-salida))))))

;;; Ejemplo de utilización
(require '[clojure.test :as test])
(let [ars-dist ['((1 2) 1) '((1 4) 4) '((2 3) 2) '((2 4) 6) '((2 5) 4) '((3 5) 5) '((3 6) 6) '((4 5) 3) '((4 7) 4) '((5 6) 8) '((5 7) 7) '((6 7) 3)]
      nod (into [] (range 1 8))
      ars (into [] (map #(first %) ars-dist))
      g (nuevo-grafo-adya nod ars)]
  (test/is (= (kruskal g ars-dist) ['(1 2) '(2 3) '(4 5) '(6 7) '(1 4) '(4 7)])))