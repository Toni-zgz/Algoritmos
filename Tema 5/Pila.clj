(ns Pila)
(defprotocol Apilable
  (stk-push [this, elt])
  (stk-pop [this])
  (stk-peek [this]))

(deftype Pila [content]
  Apilable
  (stk-push [this, elt]
    (if (list? content)
      (Pila. (conj content elt))
      (println "Error: No es un objeto Pila")))
  (stk-peek [this]
    (if (list? content)
      (peek content)
      (println "Error: No es un objeto Pila")))
  (stk-pop [this]
    (if (list? content)
      (Pila. (pop content))
      (println "Error: No es un objeto Pila"))))

;;; Ejemplo de utilización
(require '[clojure.test :as test])
(def a (->Pila '()))
(def b (->Pila '(3)))
(def c (->Pila '(4 3)))
(test/testing "pila-tests"
         (test/is (= (stk-peek a) nil))
         (test/is (= (stk-peek b) 3))
         (test/is (= (stk-peek c) 4))
         (test/is (= (stk-peek (stk-push a 3)) 3))
         (test/is (= (stk-peek (stk-push b 4)) 4))
         (test/is (= (stk-peek (stk-pop c)) 3))
         (test/is (= (stk-peek (stk-pop b)) nil)))