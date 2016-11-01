(ns overtime.microsounds
  (:require [overtone.core :as ot]
            [overtime.busses :as bus]
            [overtime.instruments :as instr]
            [overtime.probability :as prob]
            [overtime.shapes :as shapes]
            [overtime.sounds :as snd]
            [overtime.utils :as u]
            [clojure.tools.logging :as log]))


(defonce ^:private random-density-range (atom [2 20]))
(defonce ^:private env-bufs (atom {}))
(defonce ^:private triggers (atom {}))
(defonce ^:private pans (atom {}))


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
  [instrs]
  (let [num-densities 50
        start-time (+ (ot/now) 500)
        densities-times (rest (take num-densities (iterate #(random-density %) [0 start-time])))
        next-time (last (last densities-times))]
    (doseq [[density time] densities-times]
      (ot/at time (doseq [instr instrs] (ot/ctl instr :density density))))
    (ot/apply-by next-time #'random-density-loop [instrs])))

(defn set-random-density-range
  [low high]
  (log/info "Setting random density range to" low "->" high)
  (reset! random-density-range [low high]))


;
; Creates busses and instruments for triggers and pans that drive grain synths
;
(defn- make-busses-instrs
  [synth-defs bus-category]
  (bus/add-busses bus-category (keys synth-defs) :control 1)
  (into {} (for [[key synth-def] synth-defs] [key (synth-def :out (bus/bus bus-category key))])))

(defonce core-trigger-defs {:sync      sync-trigger
                            :rand-sync sync-trigger
                            :async     async-trigger
                            :coin      coin-trigger})

(defonce core-pan-defs {:pan-left         const-pan
                        :pan-center-left  const-pan
                        :pan-center       const-pan
                        :pan-center-right const-pan
                        :pan-right        const-pan})

(defn- make-pan-key [key] (keyword (str "pan-" (name key))))

(defn- make-triggers-pans
  [trigger-defs]
  (let [all-trigger-defs (merge core-trigger-defs trigger-defs)
        new-triggers (make-busses-instrs all-trigger-defs :trigger)
        pan-defs (into {} (for [k (map make-pan-key (keys all-trigger-defs))] [k rand-pan]))
        all-pan-defs (merge core-pan-defs pan-defs)
        new-pans (make-busses-instrs all-pan-defs :pan)]
    (log/debug "Created triggers" (keys new-triggers))
    (log/debug "Created pans" (keys new-pans))
    (ot/ctl (:pan-left new-pans) :pan -1)
    (ot/ctl (:pan-center-left new-pans) :pan -0.5)
    (ot/ctl (:pan-center new-pans) :pan 0)
    (ot/ctl (:pan-center-right new-pans) :pan 0.5)
    (ot/ctl (:pan-right new-pans) :pan 1)
    (random-density-loop [(:rand-sync new-triggers) (:pan-rand-sync new-pans)])
    (swap! triggers merge new-triggers)
    (swap! pans merge new-pans)
    (instr/add-instrs new-triggers)
    (instr/add-instrs new-pans)
    true))


;
; Accessors to envelope buffers, triggers, pans
;
(defn env-buf [key] (u/check-nil (key @env-bufs) "Env Buf" key))
(defn env-buf-keys [] (keys @env-bufs))

(defn trigger [key] (u/check-nil (key @triggers) "Trigger" key))
(defn trigger-keys [] (keys @triggers))
(defn pan [key] (u/check-nil (key @pans) "Pan" key))
(defn pan-keys [] (keys @pans))

;
; Initialize data structions (envelope buffers, triggers and pans) for this namespace
;
(defn init
  ([] (init {}))
  ([{:keys [triggers density-range]}]
   (swap! env-bufs merge (make-env-bufs env-signals))
   (make-triggers-pans triggers)
   (when density-range (apply set-random-density-range density-range))
   (log/info "Finished microsounds init, env-bufs:" (env-buf-keys) ", triggers:" (trigger-keys) ", pans:" (pan-keys))
   true))


(defmethod snd/sound-param-val :env-buf [_type val] (env-buf val))

(comment
  (micro/init {:coin2 micro/coin-trigger}))