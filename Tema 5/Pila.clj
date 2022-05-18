(ns Pila)
; Protocolos
(defprotocol IApilable
  (stk-push [this elt])
  (stk-pop [this])
  (stk-peek [this]))

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
  
  java.lang.Object
  ; equals :: Pila -> Pila -> Boolean
  ; este método nos da un valor true si ambos
  ; parámetros tienen el mismo tipo y su content
  ; tienen los mismos valores.
  (equals [this other] (and
                    (= (type this) (type other))
                    (= (.content this) (.content other))))
          
)
; nueva-pila :: () -> Pila
; Esta función crea una pila vacia
(defn nueva-pila []
  (->Pila '()))

;;; Ejemplo de utilización
(require '[clojure.test :as test])
(def a (nueva-pila))
(def b (stk-push a 3))
(def c (stk-push b 4))
(test/testing "pila-tests"
         (test/is (= (stk-peek a) nil))
         (test/is (= (stk-peek b) 3))
         (test/is (= (stk-peek c) 4))
         (test/is (= (stk-push a 3) b))
         (test/is (= (stk-push b 4) c))
         (test/is (= (stk-pop c) [4 b]))
         (test/is (= (stk-pop b) [3 a])))