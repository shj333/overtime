(ns overtime.patterns
  (:require [overtone.core :as ot]
            [clojure.tools.logging :as log]
            [overtime.utils :as u]))


(defonce ^:private patterns (atom {}))
(defonce ^:private dflt-pattern-params {:dur-per-stage 5000})

(defn- get-pattern [pattern-key] (u/check-nil (pattern-key @patterns) "Pattern" pattern-key))

(defn- get-value
  [f param]
  (if (seq? param)
    (f param)
    param))

(defn- this-synth-params
  [params]
  (flatten (for [[k v] params] [k (get-value first v)])))

(defn- next-synth-params
  [params]
  (into {} (for [[k v] params] [k (get-value next v)])))

(defn- get-pattern-event
  [[prev-event pattern]]
  (let [time (:next-time prev-event)
        {:keys [name synth params]} @pattern
        this-synth (get-value first synth)
        this-params (this-synth-params params)
        this-dur (get-value first (:dur params))
        next-synth (get-value next synth)
        next-params (next-synth-params params)]
    (if (not-any? nil? this-params)
      (do
        (log/debug "Pattern:" name ", time:" time ", synth:" this-synth ", params:" this-params)
        (swap! pattern assoc :synth next-synth :params next-params)
        [{:synth     this-synth
          :params    this-params
          :time      time
          :next-time (+ time this-dur)
          :total-dur (+ (:total-dur prev-event) this-dur)}
         pattern])
      (do
        (log/debug "Pattern complete:" name)
        nil))))

(defn- is-not-stage-complete?
  [dur-per-stage event]
  (if (nil? event)
    false
    (< (:total-dur event) dur-per-stage)))

(defn- get-pattern-events
  [time pattern dur-per-stage]
  (let [[first-event] (get-pattern-event [{:next-time time :total-dur 0} pattern])
        lazy-events (->> (iterate get-pattern-event [first-event pattern])
                         (map first))
        [head tail] (split-with (partial is-not-stage-complete? dur-per-stage) lazy-events)]
    (concat head (take 1 tail))))

(defn- do-pattern-events
  [events]
  (doseq [{:keys [synth params time]} events] (if-not (nil? synth) (ot/at time (apply synth params)))))

(defn- setup-next-pattern-events
  [pattern-key last-event]
  (if-not (nil? last-event)
    (let [{:keys [next-time]} last-event]
      (u/apply-by next-time (#'do-pattern-at next-time pattern-key)))))

(defn do-pattern-at
  [time pattern-key]
  (let [pattern (get-pattern pattern-key)
        dur-per-stage (get-in @pattern [:params :dur-per-stage])
        events (get-pattern-events time pattern dur-per-stage)]
    (do-pattern-events events)
    (setup-next-pattern-events pattern-key (last events))))

(defn make-pattern
  [pattern-key synth params]
  (let [pattern (atom {:name (name pattern-key) :synth synth :params (merge dflt-pattern-params params)})]
    (swap! patterns assoc pattern-key pattern)
    true))

(defn change-pattern
  [pattern-key & params]
  (let [pattern (get-pattern pattern-key)]
    (swap! pattern update-in [:params] merge (apply hash-map params))
    true))

(defn pattern-value
  [pattern-key param-key]
  (let [pattern (get-pattern pattern-key)]
    (param-key (:params @pattern))))


(comment
  (if (ot/server-disconnected?) (ot/connect-external-server 4445))

  (ot/defsynth gabor [out-bus [0 :ir] freq [440 :ir] sustain [1 :ir] pan [0.0 :ir] amp [0.1 :ir] width [0.25 :ir]]
               (let [env (ot/lf-gauss:ar sustain width :loop 0 :action ot/FREE)
                     half-pi (* 0.5 (. Math PI))
                     son (* env (ot/f-sin-osc:ar freq half-pi))]
                 (ot/offset-out:ar out-bus (ot/pan2:ar son pan amp))))

  (make-pattern :gabor1
                gabor
                {:out-bus       0
                 :freq          (cycle [440 220])
                 :sustain       0.1
                 :pan           0.0
                 :amp           0.1
                 :width         0.25
                 :dur           1000
                 :dur-per-stage 2000})
  (do-pattern-at (+ (ot/now) 1000) :gabor1)
  (change-pattern :gabor1 :dur 25)
  (change-pattern :gabor1 :amp nil))