(ns Cola)
; Protocolos
(defprotocol IEncolable
  (que-queue [this, elt])
  (que-dequeue [this])
  (que-peek [this]))

; Tipo Cola
(deftype Cola [content]
  IEncolable
  ; que-queue :: Cola -> a -> Cola
  ; Este método introduce un valor en el frente
  ; de la cola, devolviendo la cola modificada.
  (que-queue [this, elt]
    (if (vector? content)
      (let [list-nueva (concat [elt] content)
            vector-nuevo (into [] list-nueva)]
       (Cola. vector-nuevo))
      (println "Error: No es un objeto Cola")))
  ; que-peek :: Cola -> a
  ; Este método devuelve el valor que esta en 
  ; la parte trasera de la Cola, sin modificar
  ; la cola.
  (que-peek [this]
    (if (vector? content)
      (peek content)
      (println "Error: No es un objeto Cola")))
  ; que-dequeue :: Cola -> [a, Cola]
  ; Este método devuelve el valor que esta en 
  ; la parte trasera de la Cola, y la cola 
  ; sin ese valor.
  (que-dequeue [this]
    (if (vector? content)
      (vector (peek content)(Cola. (pop content)))
      (println "Error: No es un objeto Cola")))
  
  java.lang.Object
  ; equals :: Cola -> Cola -> Boolean
  ; este método nos da un valor true si ambos
  ; parámetros tienen el mismo tipo y su content
  ; tienen los mismos valores.
  (equals [this other] (and
                    (= (type this) (type other))
                    (= (.content this) (.content other)))))
; nueva-cola :: () -> Cola
; Esta función crea una cola vacia.
(defn nueva-cola []
    (->Cola []))

;;; Ejemplo de utilización
(require '[clojure.test :as test])
(def a (nueva-cola))
(def b (que-queue a 3))
(def c (que-queue b 4))
(def d (que-queue a 4))
(test/testing "cola-tests"
         (test/is (= (que-peek a) nil))
         (test/is (= (que-peek b) 3))
         (test/is (= (que-peek c) 3))
         (test/is (= (que-peek d) 4))
         (test/is (= (que-queue a 3) b))
         (test/is (= (que-queue b 4) c))
         (test/is (= (que-dequeue c) [3 d]))
         (test/is (= (que-dequeue b) [3 a]))
         (test/is (= (que-dequeue d) [4 a])))
