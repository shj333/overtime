(ns overtime.shapes
  (:require [overtone.sc.envelope :as ot]))


(defmulti gen-shape (fn [stage _pos] (nth stage 4)))

(defmethod gen-shape (ot/ENV-SHAPES :step)
  [stage _pos]
  (let [[_y1 y2] stage]
    y2))

(defmethod gen-shape (ot/ENV-SHAPES :hold)
  [stage pos]
  (let [[y1 y2] stage]
    (if (< pos 1) y1 y2)))

(defmethod gen-shape (ot/ENV-SHAPES :linear)
  [stage pos]
  (let [[y1 y2] stage]
    (+ y1
       (* pos (- y2 y1)))))

(defmethod gen-shape (ot/ENV-SHAPES :exp)
  [stage pos]
  (let [[y1 y2] stage
        limit (max 0.0001 y1)]
    (* limit (Math/pow (/ y2 limit) pos))))

(defmethod gen-shape (ot/ENV-SHAPES :sine)
  [stage pos]
  (let [[y1 y2] stage]
    (+ y1
       (* (- y2 y1)
          (+ (* -1 (Math/cos (* Math/PI pos)) 0.5) 0.5)))))

(defmethod gen-shape (ot/ENV-SHAPES :welch)
  [stage pos]
  (let [[y1 y2] stage
        pos (if (< y1 y2) pos (- 1.0 pos))]
    (+ y1
       (* (- y2 y1)
          (Math/sin (* Math/PI 0.5 pos))))))

(defmethod gen-shape 5
  [stage pos]
  (let [[y1 y2 _ _ _ curvature] stage]
    (if (< (Math/abs curvature) 0.0001)
      (+ (* pos (- y2 y1))
         y1)
      (let [denominator (- 1.0 (Math/exp curvature))
            numerator (- 1.0 (Math/exp (* pos curvature)))]
        (+ y1
           (* (- y2 y1) (/ numerator denominator)))))))

(defmethod gen-shape (ot/ENV-SHAPES :squared)
  [stage pos]
  (let [[y1 y2] stage
        y1-s (Math/sqrt y1)
        y2-s (Math/sqrt y2)
        yp (+ y1-s (* pos (- y2-s y1-s)))]
    (* yp yp)))

(defmethod gen-shape (ot/ENV-SHAPES :cubed)
  [stage pos]
  (let [[y1 y2] stage]
    (let [y1-c (Math/pow y1 0.3333333)
          y2-c (Math/pow y2 0.3333333)
          yp (+ y1-c (* pos (- y2-c y1-c)))]
      (* yp yp yp))))



(defn- signal-pos
  [time start-time end-time]
  (->> (- end-time start-time)
       (/ (- time start-time))))

(defn- gen-stage-signal
  [time stage]
  (let [[_ _ start-time end-time] stage]
    (if (< time end-time)
      (gen-shape stage (signal-pos time start-time end-time))
      nil)))

(defn- final-level
  [env-stages]
  (second (last env-stages)))

(defn- gen-signal
  [env-stages ratio idx]
  (let [time (* ratio idx)
        signal (some #(gen-stage-signal time %) env-stages)]
    (if (nil? signal)
      (final-level env-stages)
      signal)))



(defn- stage-levels
  [stages]
  (->> (map first (rest stages))
       (cons (first (first stages)))
       (partition 2 1)))

(defn- stage-times
  [stages]
  (->> (map second (rest stages))
       (reductions +)
       (cons 0)
       (partition 2 1)))

(defn- stage-shapes
  [stages]
  (map #(drop 2 %) (rest stages)))

(defn- env-stages
  [env]
  (let [stages (partition 4 env)]
    (->> (map vector (stage-levels stages) (stage-times stages) (stage-shapes stages))
         (map flatten))))

(defn env->signal
  [env length]
  (let [env-stages (env-stages env)
        ratio (/ 1.0 (- length 1))]
    (map #(gen-signal env-stages ratio %) (range length))))
