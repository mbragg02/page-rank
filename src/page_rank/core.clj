(ns page-rank.core)

;; PR(A) = (1-d) + d (PR(T1)/C(T1) + ... + PR(Tn)/C(Tn))

;; damping factor
(def d 0.85)

(def initial-state [1 1 1 1])

(def links [[0 1 1 0]
            [0 0 1 0]
            [1 0 0 0]
            [0 0 1 0]])

(defn normalise-vector
  [vector]
  (let [sum (reduce + vector)]
      (map #(/ % sum) vector)))

(defn normalise-matrix
      [matrix]
      (map normalise-vector matrix))

(defn transpose-matrix
     [matrix]
     (apply map list matrix))

(defn calculate-linked-pages-pr
  [page initial-state]
  (reduce + (map #(* %2 %1) page initial-state)))

(defn process-page
  [page initial-state]
  (+ (- 1 d) (* d (calculate-linked-pages-pr page initial-state))))

(defn process-links
  [pr links]
  (let [transposed-links (transpose-matrix (normalise-matrix links))]
    (map #(process-page % pr) transposed-links)))

;; (process-links initial-state)

;; Hardcode 20 interations for now. TODO implement "stablization" function to terminate 
(defn rank
  [initial-state links]
  (loop [x 20 pr initial-state]
      (when (> x 1)
        (let [current-pr (process-links pr links)]
          (println current-pr)
          (recur (- x 1) current-pr)))))

(rank initial-state links)
