(ns overtime.microsound
  (:require [overtone.core :as ot]
            [overtime.shapes :as shapes]
            [overtime.probability :as prob]
            [overtime.utils :as u]
            [overtime.instr-control :as instr]
            [overtime.sound-control :as snd]))


(defonce ^:private random-density-range (atom [2 20]))
(defonce ^:private env-bufs (atom {}))
(defonce ^:private triggers-pans (atom {:triggers       {}
                                        :trigger-busses {}
                                        :pans           {}
                                        :pan-busses     {}}))


;
; Grain envelopes
;
(defn- make-sinc-point
  [sinc-num x length]
  (let [val (* (u/num-lin-lin x 0 (dec length) (- 0 Math/PI) Math/PI) sinc-num)]
    (/ (Math/sin val) val)))

(defn- make-sinc
  [sinc-num length]
  (map #(make-sinc-point sinc-num % length) (range length)))

(defn- make-sincs
  [num-instances length]
  (into {} (for [idx (range 1 (inc num-instances))] [(keyword (str "sinc" idx)) (make-sinc idx length)])))

(defn env->buffer
  [env-signals]
  (let [b (ot/buffer (count env-signals))]
    (ot/buffer-write! b env-signals)
    b))

(defn make-env-bufs [env-signals] (into {} (for [[k env] env-signals] [k (env->buffer env)])))


(defonce env-data {:guass       (ot/env-sine)
                   :quasi-guass (ot/envelope [0, 1, 1, 0] [0.33, 0.34, 0.33] :sin)
                   :linear      (ot/envelope [0, 1, 1, 0] [0.33, 0.34, 0.33] :lin)
                   :welch       (ot/envelope [0, 1, 1, 0] [0.33, 0.34, 0.33] :welch)
                   :expodec     (ot/envelope [1, 0.001] [1] :exp)
                   :rexpodec    (ot/envelope [0.001, 1] [1] :exp)
                   :perc1       (ot/env-perc 0.05 0.95)
                   :perc2       (ot/env-perc 0.1 0.9)})

(defonce env-signals (merge (make-sincs 10 400)
                            (into {} (for [[k env] env-data] [k (shapes/env->signal env 400)]))))




;
; Triggers
;
(ot/defsynth sync-trigger [out 0 density 1] (ot/out:kr out (ot/impulse:kr density)))
(ot/defsynth async-trigger [out 0 density 1] (ot/out:kr out (ot/dust:kr density)))
(ot/defsynth coin-trigger [out 0 density 1 prob 0.5] (ot/out:kr out (ot/coin-gate:kr prob (ot/impulse:kr density))))


;
; Pans
;
(ot/defsynth rand-pan [out 0 density 1] (ot/out:kr out (ot/lf-noise0:kr density)))
(ot/defsynth const-pan [out 0 pan 0] (ot/out:kr out pan))



;
; Sets density of given instruments to random values over time
;
(defn- random-density
  [prev-density-time]
  (let [[low high] @random-density-range
        density (prob/exp-rand low high)
        wait-time (Math/round (/ 1000.0 density))
        [_ prev-time] prev-density-time]
    [density (+ prev-time wait-time)]))

(defn random-density-loop
  [insts]
  (let [num-densities 50
        start-time (+ (ot/now) 500)
        densities-times (rest (take num-densities (iterate #(random-density %) [0 start-time])))
        next-time (last (last densities-times))]
    (doseq [[density time] densities-times]
      (ot/at time (doseq [inst insts] (ot/ctl inst :density density))))
    (ot/apply-by next-time #'random-density-loop [insts])))

(defn set-random-density-range
  [low high]
  (reset! random-density-range [low high]))


;
; Creates busses and instruments for triggers and pans that drive grain synths
;
(defn- make-busses-insts
  [synth-defs bus-type]
  (let [busses (into {} (for [key (keys synth-defs)] [key (bus-type)]))
        insts (into {} (for [[key bus] busses] [key ((key synth-defs) :out bus)]))]
    [busses insts]))

(defonce core-trigger-defs {:sync      sync-trigger
                            :rand-sync sync-trigger
                            :async     async-trigger
                            :coin      coin-trigger})

(defonce core-pan-defs {:left         const-pan
                        :center-left  const-pan
                        :center       const-pan
                        :center-right const-pan
                        :right        const-pan})

(defn- make-triggers-pans
  [trigger-defs]
  (let [all-trigger-defs (merge core-trigger-defs trigger-defs)
        [trigger-busses triggers] (make-busses-insts all-trigger-defs ot/control-bus)
        pan-defs (into {} (for [k (keys all-trigger-defs)] [k rand-pan]))
        all-pan-defs (merge core-pan-defs pan-defs)
        [pan-busses pans] (make-busses-insts all-pan-defs ot/control-bus)]
    (ot/ctl (:left pans) :pan -1)
    (ot/ctl (:center-left pans) :pan -0.5)
    (ot/ctl (:center pans) :pan 0)
    (ot/ctl (:center-right pans) :pan 0.5)
    (ot/ctl (:right pans) :pan 1)
    (random-density-loop [(:rand-sync triggers) (:rand-sync pans)])
    (swap! triggers-pans merge {:trigger-busses (merge (@triggers-pans :trigger-busses) trigger-busses)
                                :triggers       (merge (@triggers-pans :triggers) triggers)
                                :pan-busses     (merge (@triggers-pans :pan-busses) pan-busses)
                                :pans           (merge (@triggers-pans :pans) pans)})
    true))



;
; Initialize data structions (envelope buffers, triggers and pans) for this namespace
;
(defn init
  ([] (init {}))
  ([trigger-defs]
   (swap! env-bufs merge (make-env-bufs env-signals))
   (make-triggers-pans trigger-defs)
   true))


;
; Accessors to envelope buffers, triggers, pans
;
(defn env-buf [key] (u/check-nil (key @env-bufs) "Env Buf" key))
(defn env-buf-keys [] (keys @env-bufs))

(defn- get-triggers-pans [key] (u/check-nil (key @triggers-pans) "Triggers/Pans" key))
(defn trigger [key] (u/check-nil (key (get-triggers-pans :triggers)) "Trigger" key))
(defn trigger-bus [key] (u/check-nil (key (get-triggers-pans :trigger-busses)) "Trigger Bus" key))
(defn trigger-bus-keys [] (keys (get-triggers-pans :trigger-busses)))
(defn pan [key] (u/check-nil (key (get-triggers-pans :pans)) "Pan" key))
(defn pan-bus [key] (u/check-nil (key (get-triggers-pans :pan-busses)) "Pan Bus" key))
(defn pan-bus-keys [] (keys (get-triggers-pans :pan-busses)))


(defmethod instr/synth-instance :trigger [_type key] (trigger key))
(defmethod instr/synth-instance :pan [_type key] (pan key))

(defmethod snd/sound-param :env-buf [_type data] (env-buf data))
(defmethod snd/sound-param :trigger-bus [_type data] (trigger-bus data))
(defmethod snd/sound-param :pan-bus [_type data] (pan-bus data))
