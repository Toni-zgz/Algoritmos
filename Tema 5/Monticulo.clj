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

; hundir :: [Integer] -> (Integer -> Integer -> Bool) -> Integer -> [Integer]
; esta función hunde el nodo i para restablecer la propiedad del 
; monticulo.
(defn hundir [datos op nodo]
  (let [n (count datos)]
    (loop [datos-bucle datos
           k nodo]
        (let [j k
              kx2 (* 2 j)
              kx2+1 (+ (* 2 j) 1)
              T2j (get datos-bucle (- kx2 1))
              Tk (get datos-bucle (- k 1))
              T2j+1 (get datos-bucle (- kx2+1 1))
              Tj (get datos-bucle (- j 1))
              nuevo-k1 (if (and (<= kx2 n)
                                (op T2j Tk)) 
                           kx2
                           k)
              Tk1 (get datos-bucle (- nuevo-k1 1))
              nuevo-k2 (if (and (< kx2 n)
                                (op T2j+1 Tk1)) 
                           (+ kx2 1)
                           nuevo-k1)
              nuevo-Tk (get datos-bucle (- nuevo-k2 1))
              datos-nuevos (-> datos-bucle 
                             (assoc (- j 1) nuevo-Tk)
                             (assoc (- nuevo-k2 1) Tj))]
          ;(println datos-nuevos)
          (if (= j nuevo-k2)
              datos-nuevos 
              (recur datos-nuevos nuevo-k2))))))

; flotar :: [Integer] -> (Integer -> Integer -> Bool) -> Integer -> [Integer]
; esta función hace flotar el nodo i para restablecer la propiedad del 
; monticulo.
(defn flotar [datos op nodo]
  (loop [datos-bucle datos
         k nodo]
    (let [j k
          j2 (quot j 2)
          Tk (get datos-bucle (- k 1))
          Tj2 (get datos-bucle (- j2 1))
          Tj (get datos-bucle (- j 1))
          nuevo-k (if (and (> j 1)
                           (not (op Tj2 Tk))) 
                      j2
                      k)
          nuevo-Tk (get datos-bucle (- nuevo-k 1))
          datos-nuevos (-> datos-bucle 
                           (assoc (- j 1) nuevo-Tk)
                           (assoc (- nuevo-k 1) Tj))]
      (if (= j nuevo-k)
          datos-nuevos
          (recur datos-nuevos nuevo-k)))))

; borrar-raiz :: Monticulo -> Monticulo
; esta función borra el elemento raiz y asegura la 
; propiedad del monticulo.
(defn borrar-raiz [mont]
  (let [datos (into [] (.datos mont))
        Tn (peek datos)
        comparador (.comparador mont)
        num-hijos (.numero-hijos mont)]
    (-> datos
      (pop)
      (assoc 0 Tn)
      (hundir comparador 1)
      (->Monticulo comparador num-hijos))))

; modificar-monticulo :: Monticulo -> Integer -> Integer -> Monticulo
; Esta función modifica el valor de un nodo del monticulo.
(defn modificar-monticulo [mont nodo valor]
 (let [vec-datos (into [] (.datos mont))
       op (.comparador mont)
       num-hijos (.numero-hijos mont)
       Ti (get vec-datos (- nodo 1))
       nuevos-datos (assoc vec-datos (- nodo 1) valor)]
   (-> (if (not (op valor Ti))
           (hundir nuevos-datos op nodo)
           (flotar nuevos-datos op nodo))
       (->Monticulo op num-hijos))))

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
                  nuevos-datos-bucle (hundir datos-bucle comparador indice)]
              (recur (rest indices) nuevos-datos-bucle))))))

; anadir-nodo :: Monticulo -> Integer -> Monticulo
; Esta función añade un nuevo elemento a un monticulo
(defn anadir-nodo [mont valor]
  (let [datos (.datos mont)
        comparador (.comparador mont)
        num-hijos (.numero-hijos mont)
        nuevos-datos (conj datos valor)
        num-elem (count datos)]
    (->
      (flotar nuevos-datos comparador (+ num-elem 1))
      (->Monticulo comparador num-hijos))))

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
           m2 (->Monticulo '(10 7 9 4 7 5 2 2 1 6) > 2)
           m3 (crear-monticulo-binario-invertido '(1 6 9 2 7 5 2 7 4 10))
           m4 (->Monticulo '(1 2 2 4 7 5 9 7 6 10) < 2)
           m5 (->Monticulo '(11 10 9 4 7 5 2 2 1 6 7) > 2)
           m6 (->Monticulo '(9 7 6 4 7 5 2 2 1) > 2)
           m7 (->Monticulo '(10 9 9 4 7 5 2 2 1 6) > 2)
           m8 (->Monticulo '(10 7 9 4 6 5 2 2 1 3) > 2)]
     (test/is (= m1 m2))
     (test/is (= m3 m4))
     (test/is (= (obtener-raiz m1) 10))
     (test/is (= (obtener-raiz m3) 1))
     (test/is (= (.datos (crear-monticulo-binario '())) []))
     (test/is (= (anadir-nodo m1 11) m5))
     (test/is (= (borrar-raiz m1) m6))
     (test/is (= (modificar-monticulo m1 5 9) m7))
     (test/is (= (modificar-monticulo m1 2 3) m8))))