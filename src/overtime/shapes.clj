(ns overtime.shapes
  (:require [incanter core charts datasets]
            [overtone.sc.envelope :as ot]))

(defonce ^:private half-pi (* 0.5 Math/PI))


(defmulti gen-shape (fn [[_beg-level _end-level _start-time _end-time shape-id _curvature] _pos] shape-id))

(defmethod gen-shape (ot/ENV-SHAPES :step)
  [[_beg-level end-level] _pos]
  end-level)

(defmethod gen-shape (ot/ENV-SHAPES :hold)
  [[beg-level end-level] pos]
  (if (< pos 1) beg-level end-level))

(defmethod gen-shape (ot/ENV-SHAPES :linear)
  [[beg-level end-level] pos]
  (+ beg-level (* pos (- end-level beg-level))))

(defmethod gen-shape (ot/ENV-SHAPES :exp)
  [[beg-level end-level] pos]
  (let [limit (max 0.0001 beg-level)]
    (* limit (Math/pow (/ end-level limit) pos))))

(defmethod gen-shape (ot/ENV-SHAPES :sine)
  [[beg-level end-level] pos]
  (+ beg-level
     (* (- end-level beg-level)
        (+ (* -1 (Math/cos (* Math/PI pos)) 0.5) 0.5))))

(defmethod gen-shape (ot/ENV-SHAPES :welch)
  [[beg-level end-level] pos]
  ; From SC src: lang/LangPrimSource/PyrArrayPrimitives.cpp, see case shape_Welch
  (if (< beg-level end-level)
    (+ beg-level (* (- end-level beg-level) (Math/sin (* half-pi pos))))
    (- end-level (* (- end-level beg-level) (Math/sin (- half-pi (* half-pi pos)))))))

; FIXME Overtone code does not use a keyword for curve but instead uses 5
(defmethod gen-shape 5
  [[beg-level end-level _ _ _ curvature] pos]
  (if (< (Math/abs curvature) 0.0001)
    (+ (* pos (- end-level beg-level))
       beg-level)
    (let [denominator (- 1.0 (Math/exp curvature))
          numerator (- 1.0 (Math/exp (* pos curvature)))]
      (+ beg-level
         (* (- end-level beg-level) (/ numerator denominator))))))

(defmethod gen-shape (ot/ENV-SHAPES :squared)
  [[beg-level end-level] pos]
  (let [beg-level-sroot (Math/sqrt beg-level)
        end-level-sroot (Math/sqrt end-level)
        val (+ beg-level-sroot (* pos (- end-level-sroot beg-level-sroot)))]
    (* val val)))

(defmethod gen-shape (ot/ENV-SHAPES :cubed)
  [[beg-level end-level] pos]
  (let [beg-level-cubed (Math/pow beg-level 0.3333333)
        end-level-cubed (Math/pow end-level 0.3333333)
        val (+ beg-level-cubed (* pos (- end-level-cubed beg-level-cubed)))]
    (* val val val)))





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