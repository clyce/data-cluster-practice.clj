(defn rand-sub [coll n] (take n (shuffle coll)))
(defn bias [coll] (map #(- % (first coll)) coll))
(defn distance [coll-1 coll-2] 
  (apply + (map (fn [x] (#(* % %) (apply - x))) (partition 2 (interleave coll-1 coll-2)))))

(defn get-center [coll]
  (map #(/ % (count coll)) (let [+seq (fn [x y] (map #(apply + %) (partition 2 (interleave x y))))] 
                             (reduce +seq coll))))

(defn find-nearest-seed [point seeds]
  (->> (map #(vec [(distance % point) %]) seeds) (apply concat) (apply sorted-map) (first) (last)))

(defn seed-means [seeds coll]
  (->> (group-by last (map #(vec [% (find-nearest-seed % seeds)]) coll))
    (vals)
    (map (partial map first))))

(defn k-means [k coll]
  (loop [seed (rand-sub coll k) old-seed []]
    (let [it-res (seed-means seed coll)]
      (if (= seed old-seed) it-res
        (recur (map get-center it-res) seed)))))
