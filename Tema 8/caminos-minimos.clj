(ns caminos-minimos)

; floyd :: [Long, Long] -> ([Long, Long], [Long, Long])
(defn floyd [matriz-l]
  (let [num-nodos (count matriz-l)
        matriz-d (aclone matriz-l)
        matriz-p (make-array Long/TYPE num-nodos num-nodos)]
    (loop [k-sec (range 0 num-nodos)]
      (if (= k-sec '())
        (list matriz-d matriz-p)
        (let [k (first k-sec)]
          (loop [i-sec (range 0 num-nodos)]
            (if (= i-sec '())
              '()
              (let [i (first i-sec)]
                (loop [j-sec (range 0 num-nodos)]
                  (if (= j-sec '())
                    '()
                    (let [j (first j-sec)
                          dik (aget matriz-d i k)
                          dkj (aget matriz-d k j)
                          dij (aget matriz-d i j)]
                      (if (< (+ dik dkj) dij)
                        (do
                          (aset matriz-d i j (+ dik dkj))
                          (aset matriz-p i j (+ k 1)))
                        (aset matriz-d i j dij))
                      (recur (rest j-sec)))))
                  (recur (rest i-sec)))))
          (recur (rest k-sec)))))))

; Ejemplo de utilización
 (let [l (make-array Long/TYPE 4 4)]
   (aset l 0 1 5)
   (aset l 0 2 1000)
   (aset l 0 3 1000)
   (aset l 1 0 50)
   (aset l 1 2 15)
   (aset l 1 3 5)
   (aset l 2 0 30)
   (aset l 2 1 1000)
   (aset l 2 3 15)
   (aset l 3 0 15)
   (aset l 3 1 1000)
   (aset l 3 2 5)
   (pprint (floyd l)))