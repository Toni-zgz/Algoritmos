(ns integración)

(require '[clojure.test :as test])

; area :: [(Long Long)] -> Long
(defn- area [vertices]
       (loop [entrada vertices
              salida '()]
         (if (= entrada '())
           (reduce * salida)
           (let [primero (first (first entrada))
                 segundo (second (first entrada))
                 resta (- segundo primero)
                 nueva-entrada (rest entrada)
                 nueva-salida (cons resta salida)]
             (recur nueva-entrada nueva-salida)))))

; int-mc :: function -> Long -> [(Long Long)] -> Float
(defn int-mc [f n vertices]
  (let [area-vertices (area vertices)
        dimension (count vertices)]
    (loop [num-valores n
           suma 0]
      (if (= num-valores 0)
        (* area-vertices (/ suma n))
        (let [coords (->> (range 0 dimension)
                       (map (fn [elt1 elt2]
                              (let [minimo (first elt1)
                                    maximo (second elt1)]
                                (+ minimo (* maximo (rand)))))
                            vertices))
              fx (f coords)
              nuevo-num-valores (dec num-valores)
              nueva-suma (+ suma fx)]
          (recur nuevo-num-valores nueva-suma))))))

; Ejemplo de uso
(defn fact [n]
  (loop [n-bucle n
         result 1]
    (if (= n-bucle 0)
      result
      (let [nuevo-n (dec n-bucle)
            nuevo-result (* result n-bucle)]
      (recur nuevo-n nuevo-result)))))

(defn f [valores]
  (if (not (= (count valores) 1))
    (println "La función debe tener una variable")
    (let [fact100 (fact 100)
          PI 3.1416
          x (first valores)]))
  
(test/testing "Integración test")