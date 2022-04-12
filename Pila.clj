(ns Pila)
(defprotocol Apilable
  (stk-push [this, elt])
  (stk-pop [this])
  (stk-peek [this]))

(deftype Pila [^:unsynchronized-mutable content]
  Apilable
  (stk-push [this, elt]
    (if (list? content)
      (set! content (conj content elt))
      (println "Error: No es un objeto Pila")))
  (stk-peek [this]
    (if (list? content)
      (peek content)
      (println "Error: No es un objeto Pila")))
  (stk-pop [this]
    (if (list? content)
      (set! content (pop content))
      (println "Error: No es un objeto Pila"))))

;;; Ejemplo de utilización
(require '[clojure.test :as test])
(def a (->Pila '()))
(test/testing "pila-tests"
         (test/is (= (stk-peek a) nil))
         (test/is (= (stk-push a 3) '(3)))
         (test/is (= (stk-peek a) 3))
         (test/is (= (stk-push a 4) '(4 3)))
         (test/is (= (stk-peek a) 4))
         (test/is (= (stk-pop a) '(3)))
         (test/is (= (stk-peek a) 3))
         (test/is (= (stk-pop a) '()))
         (test/is (= (stk-peek a) nil)))