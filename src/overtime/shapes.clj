(ns overtime.shapes
  (:require [incanter core charts datasets]
            [overtone.sc.envelope :as otenv]
            [overtone.sc.buffer :as otbuf]
            [overtime.utils :as u]))

(defonce ^:private half-pi (* 0.5 Math/PI))
(defonce ^:private two-pi (* 2 Math/PI))

(defmulti gen-shape (fn [[_beg-level _end-level _start-time _end-time shape-id _curvature] _pos] shape-id))

(defmethod gen-shape (otenv/ENV-SHAPES :step)
  [[_beg-level end-level] _pos]
  end-level)

(defmethod gen-shape (otenv/ENV-SHAPES :hold)
  [[beg-level end-level] pos]
  (if (< pos 1) beg-level end-level))

(defmethod gen-shape (otenv/ENV-SHAPES :linear)
  [[beg-level end-level] pos]
  (+ beg-level (* pos (- end-level beg-level))))

(defmethod gen-shape (otenv/ENV-SHAPES :exp)
  [[beg-level end-level] pos]
  (let [limit (max 0.0001 beg-level)]
    (* limit (Math/pow (/ end-level limit) pos))))

(defmethod gen-shape (otenv/ENV-SHAPES :sine)
  [[beg-level end-level] pos]
  (+ beg-level
     (* (- end-level beg-level)
        (+ (* -1 (Math/cos (* Math/PI pos)) 0.5) 0.5))))

(defmethod gen-shape (otenv/ENV-SHAPES :welch)
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

(defmethod gen-shape (otenv/ENV-SHAPES :squared)
  [[beg-level end-level] pos]
  (let [beg-level-sroot (Math/sqrt beg-level)
        end-level-sroot (Math/sqrt end-level)
        val (+ beg-level-sroot (* pos (- end-level-sroot beg-level-sroot)))]
    (* val val)))

(defmethod gen-shape (otenv/ENV-SHAPES :cubed)
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

; Sinc envelopes
(defn- make-sinc-point
  [sinc-num x length]
  (let [val (* (u/num-lin-lin x 0 (dec length) (- 0 Math/PI) Math/PI) sinc-num)]
    (/ (Math/sin val) val)))

(defn make-sinc
  ; TODO API Doc for make-sinc
  [sinc-num length]
  (map #(make-sinc-point sinc-num % length) (range length)))

(defn make-sincs
  ; TODO API Doc for make-sincs
  [num-instances length]
  (into {} (for [idx (range 1 (inc num-instances))] [(keyword (str "sinc" idx)) (make-sinc idx length)])))


; Harmonics
(defn- add-harmonic
  [size amp phase harm]
  (otbuf/create-buffer-data size #(* amp (Math/sin (+ % phase))) 0 (* harm two-pi)))

(defn normalize
  ; TODO API doc for normalize
  [signals]
  (let [max (apply max signals)]
    (if (zero? max)
      signals
      (map #(* (/ 1.0 max) %) signals))))

(defn sine-fill
  ; TODO API doc for sine-fill
  ([size harm-amps] (sine-fill size harm-amps nil))
  ([size harm-amps phases]
    ; Create harmonic for each given amplitude, then add all the generated harmonics together and normalize to resulting wave
   (->> (range 1 (inc (count harm-amps)))
        (map add-harmonic (repeat size) harm-amps (or phases (repeat 0)))
        (apply map +)
        normalize)))

(defn rand-sine-fill
  ; TODO API doc for rand-sine-fill
  ([size num-harms] (rand-sine-fill size num-harms nil))
  ([size num-harms phases]
   (let [amps (->> (range 1 (inc num-harms))
                   (map #(/ 1.0 %))
                   shuffle)]
     (sine-fill size amps phases))))

(defn zero-last
  ; TODO API doc for zero-last
  [wave]
  ; Drop the last signal from wave and set it zero to ensure that wave form ends precisely on time in microsounds
  (-> wave
      drop-last
      vec
      (conj 0.0)))

