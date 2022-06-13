(ns Particion)

;Tipo Particion
(deftype Particion [conjunto altura]
  java.lang.Object
  (equals [this other]
     (and
        (= (type this) (type other))
        (= (.conjunto this) (.conjunto other))
        (= (.altura this) (.altura other)))))

; buscar :: Particion -> a -> a
; Esta función busca la raiz del grupo en el que está el argumento
; dentro de la partición.
(defn buscar [particion elt]
  (loop [r elt]
    (let [nueva-r (get (.conjunto particion) (- r 1))]
      (if (= r nueva-r) 
        r
        (recur nueva-r)))))

  ; fusionar :: Particion -> a -> a -> Particion
(defn fusionar [particion elt1 elt2]
  (let [altura (.altura particion)
        conjunto (.conjunto particion)
        altura-1 (get altura (- elt1 1))
        altura-2 (get altura (- elt2 1))]
    (if (= altura-1 altura-2)
        (let [nueva-altura-1 (+ altura-1 1)
              nueva-altura (assoc altura (- elt1 1) nueva-altura-1)
              nuevo-conjunto (assoc conjunto (- elt2 1) elt1)]
          (->Particion nuevo-conjunto nueva-altura))
      (if (> altura-1 altura-2)
        (let [nuevo-conjunto (assoc conjunto (- elt2 1) elt1)]
          (Particion. nuevo-conjunto altura))
        (let [nuevo-conjunto (assoc conjunto (- elt1 1) elt2)]
          (Particion. nuevo-conjunto altura))))))

; nueva-particion :: Integer -> Particion
; Esta función crea una particion inicializada
; con el numero de elementos que se da como
; argumento.
(defn nueva-particion [num-elems]
  (let [rango (range 1 (+ num-elems 1))
        conjunto (into [] rango)
        altura (into [] (map (fn [elt] 0) rango))]
    (->Particion conjunto altura)))

;;; Ejemplo de utilización
(require '[clojure.test :as test])
(def a (nueva-particion 7))
(def b (fusionar a 1 2))
(test/testing "Particion tests"
    (test/is (= (buscar a 2) 2))
    (test/is (= (buscar b 2) 1)))