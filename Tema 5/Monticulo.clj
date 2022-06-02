(ns Monticulo)

; Definición del tipo Montículo
(deftype Monticulo [datos comparador numero-hijos]

  ; Implementamos el metodo equals de java.lang.Object
  java.lang.Object
  ; equals :: Monticulo -> Monticulo -> Boolean
  ; este metodo compara 2 monticulos y devuelve un boolean true si 
  ; ambos monticulos tienen el mismo tipo y los campos tienen el 
  ; mismo valor
  (equals [this other]
     (and
        (= (type this) (type other))
        (= (.datos this) (.datos other))
        (= (.comparador this) (.comparador other))
        (= (.numero-hijos this) (.numero-hijos other)))))

; obtener-raiz :: Monticulo -> Integer
; esta función obtiene el valor raiz del monticulo, que será el
; valor máximo en un monticulo normal y el valor minimo en un 
; monticulo invertido y se obtiene como el primer elemento del
; campo datos del montículo.
(defn obtener-raiz [mont]
  (get (.datos mont) 0))

; hundir :: Monticulo -> Integer -> Monticulo
; esta función hunde el nodo i para restablecer la propiedad del 
; monticulo.
(defn hundir [mont nodo]
  (let [datos (.datos mont)
        n (- (count datos) 1)
        op (.comparador mont)]
    (loop [datos-bucle datos
           k (- nodo 1)]
        (let [j k
              Tk (get datos-bucle k)
              T2j (get datos-bucle (* 2 j))
              T2j+1 (get datos-bucle (+ (* 2 j) 1))
              Tj (get datos-bucle j)
              nuevo-k1 (if (and (<= (* 2 j) n)
                                (op T2j Tk)) 
                           (* 2 j)
                           k)
              Tk1 (get datos-bucle nuevo-k1)
              nuevo-k2 (if (and (< (* 2 j) n)
                                (op T2j+1 Tk1)) 
                           (+ (* 2 j) 1)
                           nuevo-k1)
              nuevo-Tk (get datos-bucle nuevo-k2)
              datos-nuevos (-> datos-bucle 
                             (assoc j nuevo-Tk)
                             (assoc nuevo-k2 Tj))]
          (if (= j nuevo-k2)
              datos-nuevos
              (recur datos-nuevos nuevo-k2))))))
  
; iniciar :: Monticulo -> Monticulo
; esta función inicializa un monticulo.
(defn iniciar [mont]
  (let [num-elem (count (.datos mont))
        inicio (int (/ num-elem 2))
        rango (range inicio 0 -1)
        comparador (.comparador mont)
        num-hijos (.numero-hijos mont)]
      (loop [indices rango
               datos-bucle (.datos mont)]
        (if (= indices '())
            (->Monticulo datos-bucle comparador num-hijos)
            (let [indice (first indices)
                  nuevo-mont (->Monticulo datos-bucle comparador num-hijos)
                  nuevos-datos-bucle (hundir nuevo-mont indice)]
              (recur (rest indices) nuevos-datos-bucle))))))

; crear-monticulo-binario :: Secuencia -> Monticulo
; Esta función genera un monticulo binario a partir 
; de una secuencia
(defn crear-monticulo-binario [datos]
  (let [vec-datos (into [] datos)
        monticulo (Monticulo. vec-datos > 2)]
    (iniciar monticulo)))

; crear-monticulo-binario-invertido :: Secuencia -> Monticulo
; Esta función genera un monticulo binario invertido
; a partir de una secuencia
(defn crear-monticulo-binario-invertido [datos]
  (let [vec-datos (into [] datos)
        monticulo (Monticulo. vec-datos < 2)]
    (iniciar monticulo)))

;;; Ejemplo de utilización
(require '[clojure.test :as test])
(test/testing "Monticulo tests" 
     (let [m1 (crear-monticulo-binario '(1 6 9 2 7 5 2 7 4 10))
           m2 (->Monticulo '(10 9 7 7 6 5 2 2 4 1) > 2)
           m3 (crear-monticulo-binario-invertido '(1 6 9 2 7 5 2 7 4 10))
           m4 (->Monticulo '(1 2 4 2 7 5 6 7 9 10) < 2)]
     (test/is (= m1 m2))
     (test/is (= m3 m4))
     (test/is (= (obtener-raiz m1) 10))
     (test/is (= (obtener-raiz m3) 1))
     (test/is (= (.datos (crear-monticulo-binario '())) []))))
