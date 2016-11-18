(ns overtime.shapes
  (:require [incanter core charts datasets]
            [overtone.sc.envelope :as ot]))


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

; FIXME Overtone code does not use a keyword for curve but instead uses 5
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


;
; Public API
;
(defn env-stages
  ; FIXME better code doc for env-stages
  "Get the stages for the given envelope as a sequence of envelope stage data. Each envelope stage is a sequence of six values:
  Start value for stage
  End value for stage
  Start time of stage (relative to beginning time of envelope)
  End time of stage (relative to beginning time of envelope)
  Shape id of stage (as defined by SC, see multi method gen-shape)
  Curvature of stage (if shape id is :curve (5))
  "
  [env]
  (let [stages (partition 4 env)]
    (->> (map vector (stage-levels stages) (stage-times stages) (stage-shapes stages))
         (map flatten))))

(defn env-total-dur
  [env-stages]
  (-> (last env-stages)
      (nth 3)))

(defn signal-at
  "Returns the signal data for the given envelope (represented as stages for the envelope, see env-stages function) at the given time"
  [env-stages time]
  (let [signal (some #(gen-stage-signal time %) env-stages)]
    (if (nil? signal)
      (final-level env-stages)
      signal)))

(defn env->signals
  ; FIXME better code doc for env->signals
  "Converts the given envelope to a sequence of signals of the given length"
  [env length]
  (let [env-stages (env-stages env)
        total-dur (env-total-dur env-stages)
        ratio (/ 1.0 (- length 1))]
    (map #(signal-at env-stages (* total-dur ratio %)) (range length))))

(defn view-env
  "View envelope as X/Y plot using Incanter view"
  [env length]
  (let [signals (env->signals env length)]
    (-> (range length)
        (incanter.charts/xy-plot signals :title "Envelope" :x-label "range" :y-label "signals")
        (incanter.core/view :width 400 :height 320))))

; TODO Store envelopes in an atom: key => {:signals :stages :total-dir} -- similar to busses, buffers