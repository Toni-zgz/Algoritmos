(ns mult-matr)

; fm :: Long -> Long -> [Long] -> Long
(defn fm [i j vector-d]
  (if (>= i j)
    0
    (loop [k-sec (range i j)
           m 10000000000]
      (if (= k-sec '())
        m 
        (let [k (first k-sec)
              fmik (fm i k vector-d)
              fmk+1 (fm (+ k 1) j vector-d)
              di-1 (get vector-d (- i 1))
              dk (get vector-d k)
              dj (get vector-d j)
              d (* di-1 dk dj)
              nuevo-valor (+ fmik fmk+1 d)
              nuevo-m (min m nuevo-valor)]
          (recur (rest k-sec) nuevo-m))))))

; fm-memo: version memoized de fm
(def fm-memo (memoize fm))

; Ejemplo de utilización
(time (fm 1 4 [13 5 89 3 34]))
(time (fm-memo 1 4 [13 5 89 3 34]))