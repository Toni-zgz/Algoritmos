(ns Pila)
; Protocolos
(defprotocol IApilable
  (stk-push [this, elt])
  (stk-pop [this])
  (stk-peek [this]))

(defprotocol IEquiv
  (equiv [this other]))

; Tipo Pila
(deftype Pila [content]
  IApilable
  ; stk-push :: Pila -> a -> Pila
  ; este método inserta un valor de tipo a
  ; en el frente de la pila.
  (stk-push [this, elt]
    (if (list? content)
      (Pila. (conj content elt))
      (println "Error: No es un objeto Pila")))
  ; stk-peek :: Pila -> a
  ; este método extrae el valor que está en el
  ; frente de la pila sin modificar la pila.
  (stk-peek [this]
    (if (list? content)
      (peek content)
      (println "Error: No es un objeto Pila")))
  ; stk-pop :: Pila -> [a, Pila]
  ; este método extrae el valor que esta en el 
  ; frente de la pila y la pila sin ese valor.
  (stk-pop [this]
    (if (list? content)
      (vector (peek content) (Pila. (pop content)))
      (println "Error: No es un objeto Pila")))
  
  IEquiv
  ; equiv :: Pila -> Pila -> Boolean
  ; este método nos da un valor true si ambos
  ; parámetros tienen el mismo tipo y su content
  ; tienen los mismos valores.
  (equiv [this other] (and
                       (= (type this) (type other))
                       (= (.content this) (.content other)))))

;;; Ejemplo de utilización
(require '[clojure.test :as test])
(def a (->Pila '()))
(def b (->Pila '(3)))
(def c (->Pila '(4 3)))
(test/testing "pila-tests"
         (test/is (= (stk-peek a) nil))
         (test/is (= (stk-peek b) 3))
         (test/is (= (stk-peek c) 4))
         (test/is (equiv (stk-push a 3) b))
         (test/is (equiv (stk-push b 4) c))
         (let [value (stk-pop c)]
           (test/is (= (first value) 4))
           (test/is (equiv (second value) b)))
         (let [value (stk-pop b)]
           (test/is (= (first value) 3))
           (test/is (equiv (second value) a))))
