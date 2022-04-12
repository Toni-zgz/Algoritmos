(ns Pila)
(defprotocol IApilable
  (stk-push [this, elt])
  (stk-pop [this])
  (stk-peek [this]))

(defprotocol IEquiv
  (equiv [this other]))

(deftype Pila [content]
  IApilable
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
      (println "Error: No es un objeto Pila")))
  IEquiv
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
         (test/is (equiv (stk-pop c) b))
         (test/is (equiv (stk-pop b) a)))
