(ns recorrido-arboles)

(defn- recorrer [arbol orden]
       (if (or (= arbol nil)
          (= (:val arbol) nil))
         '()
         (let [valor (:val arbol)
               izquierda (recorrer (:izq arbol) orden)
               derecha (recorrer (:dcha arbol) orden)]
           (cond 
             (= orden :pre-orden) (flatten (list valor izquierda derecha))
             (= orden :in-orden) (flatten (list izquierda valor derecha))
             (= orden :post-orden) (flatten (list izquierda derecha valor))
             :else "Error: no corresponde a ningun orden"))))

(defn post-orden [arbol]
  (recorrer arbol :post-orden))

(defn pre-orden [arbol]
  (recorrer arbol :pre-orden))

(defn in-orden [arbol]
  (recorrer arbol :in-orden))

(defn vector->arbol [vect]
  (if (vector? vect)
    (let [[valor izquierda derecha] vect]
      {:val valor
       :izq (vector->arbol izquierda) 
       :dcha (vector->arbol derecha) })
    vect))

; Ejemplo de utilización
(require '[clojure.test :as test])
(test/testing "creacion-arboles tests"
              (test/is (= (vector->arbol []) {:val nil, :izq nil, :dcha nil}))
              (test/is (= (vector->arbol [1]) {:val 1, :izq nil :dcha nil}))
              (test/is (= (vector->arbol [1 [3] [5]]) {:val 1, :izq {:val 3, :izq nil, :dcha nil}, :dcha {:val 5, :izq nil, :dcha nil}}))
              (test/is (= (vector->arbol [1 [2 [3] [4]] [5]]) {:val 1, :izq {:val 2, :izq {:val 3, :izq nil, :dcha nil}, :dcha {:val 4, :izq nil :dcha nil}}, :dcha {:val 5 :izq nil :dcha nil}}))) 
(test/testing "post-orden tests"
              (test/is (= (post-orden (vector->arbol [])) '()))
              (test/is (= (post-orden (vector->arbol [1])) '(1)))
              (test/is (= (post-orden (vector->arbol [1 [3] [5]])) '(3 5 1)))
              (test/is (= (post-orden (vector->arbol [1 [2 [3] [4]] [5]])) '(3 4 2 5 1))))
(test/testing "pre-orden tests"
              (test/is (= (pre-orden (vector->arbol [])) '()))
              (test/is (= (pre-orden (vector->arbol [1])) '(1)))
              (test/is (= (pre-orden (vector->arbol [1 [3] [5]])) '(1 3 5)))
              (test/is (= (pre-orden (vector->arbol [1 [2 [3] [4]] [5]])) '(1 2 3 4 5))))
(test/testing "in-orden tests"
              (test/is (= (in-orden (vector->arbol [])) '()))
              (test/is (= (in-orden (vector->arbol [1])) '(1)))
              (test/is (= (in-orden (vector->arbol [1 [3] [5]])) '(3 1 5)))
              (test/is (= (in-orden (vector->arbol [1 [2 [3] [4]] [5]])) '(3 2 4 1 5))))